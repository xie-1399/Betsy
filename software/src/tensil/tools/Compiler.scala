/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package tensil.tools

import java.io._
import scala.collection.mutable
import org.tensorflow.framework.graph.GraphDef
import onnx.onnx.ModelProto

import tensil.common.{
  Architecture,
  ArchitectureDataType,
  TablePrinter,
  TableLine,
  InstructionLayout
}
import tensil.tools.model.{Model, Program, ConstsEntry, InputOutputEntry}
import tensil.tools.compiler.{
  Backend,
  Frontend,
  TfFrontend,
  OnnxFrontend,
  EmitContext,
  MemoryManager,
  ArenaMemorySpace,
  HeapMemorySpace,
  MemoryObjectAllocator,
  MemorySpanAllocator,
  StrideStats,
  MemoryObject,
  MemoryTag,
  MemoryAddressHelper,
  SchedulerResult,
  Stats,
  IsolatedLocalSchedulingContext,
  SharedLocalSchedulingContext,
  NilHIR,
  FrontendGraphPrinter,
  MemoryUsage
}

class CompilerException(message: String) extends Exception(message) {}

case class CompilerStats(
    constsVectorSize: Long,
    layersNumber: Int,
    programSizeBytes: Long,
    constsScalarSize: Long,
    constsUtilization: Float,
    cycles: Long,
    energy: Long,
    macs: Long,
    macEfficiency: Float
) {}

case class CompilerArtifact(
    kind: String,
    fileName: String
) {}

case class CompilerArtifactsAndResult(
    result: CompilerResult,
    artifacts: Seq[CompilerArtifact]
) {}

case class CompilerResult(
    arch: Architecture,
    inputObjects: Seq[MemoryObject],
    outputObjects: Seq[MemoryObject],
    stats: CompilerStats
) {}

object CompilerSourceType {
  val Tensorflow = TfFrontend.ModelFileNameExtension
  val ONNX       = OnnxFrontend.ModelFileNameExtension
}

object Compiler {
  def getModelSourceType(modelFileName: String): CompilerSourceType = {
    val i = modelFileName.lastIndexOf('.');

    if (i > 0)
      modelFileName.substring(i + 1)
    else
      ""
  }

  def compile(
      modelName: String,
      modelFileName: String,
      outputNames: Seq[String],
      options: CompilerOptions,
      traceContext: TraceContext = TraceContext.empty
  ): CompilerArtifactsAndResult = {
    val modelStream     = new FileInputStream(modelFileName)
    val modelSourceType = getModelSourceType(modelFileName)

    compileStreamToFiles(
      modelName,
      modelSourceType,
      modelStream,
      outputNames,
      options,
      traceContext
    )
  }

  def compileStreamToFiles(
      modelName: String,
      modelSourceType: CompilerSourceType,
      modelStream: InputStream,
      outputNames: Seq[String],
      options: CompilerOptions,
      traceContext: TraceContext = TraceContext.empty
  ): CompilerArtifactsAndResult = {
    val prefix =
      if (options.targetPath.isDefined && !options.targetPath.get.isEmpty())
        options.targetPath.get + (if (options.targetPath.get.endsWith("/")) ""
                                  else "/")
      else ""

    val constsFileName  = s"${modelName}.tdata"
    val programFileName = s"${modelName}.tprog"

    val constsFilePath   = s"${prefix}${constsFileName}"
    val programFilePath  = s"${prefix}${programFileName}"
    val manifestFilePath = s"${prefix}${modelName}.tmodel"
    val graphFilePath =
      if (options.printGraph) Some(s"${prefix}${modelName}.dot") else None
    val programAssemblyFilePath =
      if (options.printProgramAssembly) Some(s"${prefix}${modelName}.tasm")
      else None

    val constsStream  = new FileOutputStream(constsFilePath)
    val programStream = new FileOutputStream(programFilePath)

    val result = compileStreamToStreams(
      modelName,
      modelSourceType,
      modelStream,
      outputNames,
      programStream,
      constsStream,
      options,
      traceContext,
      programAssemblyFilePath,
      graphFilePath
    )

    constsStream.close()
    programStream.close()

    def objectToEntries(obj: MemoryObject) = {
      require(obj.span.forall(_.tag == MemoryTag.DRAM0))

      var entries = mutable.ArrayBuffer.empty[InputOutputEntry]

      var base = obj.span.head.raw
      for (i <- 1 until obj.span.size) {
        val nextExpected = obj.span(i - 1).raw + 1
        val nextBase     = obj.span(i).raw

        if (nextExpected != nextBase) {
          entries += InputOutputEntry(
            name = obj.name,
            base = base,
            size = nextExpected - base
          )
          base = nextBase
        }
      }

      entries += InputOutputEntry(
        name = obj.name,
        base = base,
        size = obj.span.last.raw + 1 - base
      )

      entries.toSeq
    }

    def objectsToEntries(objs: Seq[MemoryObject]) =
      objs
        .map(objectToEntries(_))
        .flatten
        .toArray
        .sortBy(_.base)
        .toSeq

    val model = Model(
      name = modelName,
      program = Program(
        fileName = programFileName,
        size = result.stats.programSizeBytes
      ),
      consts = Seq(
        ConstsEntry(
          fileName = constsFileName,
          base = 0,
          size = result.stats.constsVectorSize
        )
      ),
      inputs = objectsToEntries(result.inputObjects),
      outputs = objectsToEntries(result.outputObjects),
      arch = options.arch,
      loadConstsToLocal =
        options.strategy == CompilerStrategy.LocalConsts ||
          options.strategy == CompilerStrategy.LocalVarsAndConsts
    )

    val manifestStream = new FileOutputStream(manifestFilePath)

    upickle.default.writeToOutputStream(model, manifestStream)
    manifestStream.close()

    CompilerArtifactsAndResult(
      result = result,
      artifacts = Seq(
        CompilerArtifact("Manifest", manifestFilePath),
        CompilerArtifact("Program", programFilePath),
        CompilerArtifact("Constants", constsFilePath)
      ) ++ (if (graphFilePath.isDefined)
              Seq(
                CompilerArtifact(
                  "Graph",
                  graphFilePath.get
                )
              )
            else Nil) ++ (if (programAssemblyFilePath.isDefined)
                            Seq(
                              CompilerArtifact(
                                "Program assembly",
                                programAssemblyFilePath.get
                              )
                            )
                          else Nil)
    )
  }

  def compileStreamToStreams(
      modelName: String,
      modelSourceType: CompilerSourceType,
      modelStream: InputStream,
      outputNames: Seq[String],
      programStream: OutputStream,
      constsStream: OutputStream,
      options: CompilerOptions,
      traceContext: TraceContext = TraceContext.empty,
      programAssemblyFilePath: Option[String] = None,
      graphFilePath: Option[String] = None
  ): CompilerResult = {
    val startTime = System.nanoTime()

    val graphStream = graphFilePath.map(new FileOutputStream(_))

    val frontend: Frontend =
      if (modelSourceType == CompilerSourceType.Tensorflow) {
        new TfFrontend(
          graphDef = util.protoFromStream(GraphDef, modelStream),
          arch = options.arch,
          graphStream = graphStream,
          options = options
        )
      } else if (modelSourceType == CompilerSourceType.ONNX) {
        new OnnxFrontend(
          modelProto = util.protoFromStream(ModelProto, modelStream),
          arch = options.arch,
          graphStream = graphStream,
          options = options
        )
      } else
        throw new CompilerException(
          s"No frontend to support ${modelSourceType}"
        )

    val tempSpace =
      ArenaMemorySpace("Temp", MemoryTag.Temp, Long.MaxValue)
    val tempAllocator = new MemoryObjectAllocator(
      new MemorySpanAllocator()
    )

    val dram0Space =
      HeapMemorySpace("DRAM0", MemoryTag.DRAM0, options.arch.dram0Depth)
    val dram1Space =
      HeapMemorySpace("DRAM1", MemoryTag.DRAM1, options.arch.dram1Depth)
    val localSpace =
      HeapMemorySpace("Local", MemoryTag.Local, options.arch.threadLocalDepth)
    val freeableAllocator = new MemoryObjectAllocator(
      new MemorySpanAllocator()
    )

    val layout =
      InstructionLayout(options.arch)

    var layerSchedulerResults = mutable.ArrayBuffer.empty[SchedulerResult]
    var macs                  = 0L
    var macEfficiency         = 0f
    val backendStats          = new Stats()

    if (options.printProgress)
      println(
        s"Traversing from output node(s): ${outputNames.mkString(",")} ..."
      )

    val flowNodeNames = frontend.traverse(outputNames)

    if (options.printProgress) {
      println(s"Found ${flowNodeNames.size} node(s)")
      println(s"Rewriting emitters ...")
    }

    val flowEmitters = frontend.rewrite(flowNodeNames)

    if (options.printProgress) {
      println(s"Rewritten to ${flowEmitters.size} emitter(s)")
    }

    var nextLayerIndex = 0
    val schedulingContext = options.strategy match {
      case CompilerStrategy.LocalIsolated =>
        new IsolatedLocalSchedulingContext(options)
      case CompilerStrategy.LocalConsts | CompilerStrategy.LocalVars |
          CompilerStrategy.LocalVarsAndConsts =>
        new SharedLocalSchedulingContext(options, localSpace)
    }

    /**
      * For strategies with consts loaded into local memory
      * before running the program we need to allocate consts
      * before any of the vars are allocated to insure they
      * occupy continous memory space that will match the
      * artifact data file. To do this we execute frontend
      * emitters in two passes. First pass is used solely to
      * allocate consts in local memory and to write the
      * corresponding artifact data file. The second pass is
      * where the program is emitted, but unlike single-pass
      * compilation, it relies on previously allocated consts,
      * so no new consts are allocated or written out in the
      * artifact.
      */

    val mmPass1 = options.strategy match {
      case CompilerStrategy.LocalIsolated =>
        new MemoryManager(
          tempSpace = tempSpace,
          tempAllocator = tempAllocator,
          ioSpace = dram0Space,
          varsSpace = dram0Space,
          constsSpace = dram1Space,
          freeableAllocator = freeableAllocator,
          constsStream = constsStream,
          dataType = options.arch.dataType,
          arch = options.arch,
          mkConstsDimensions = frontend.mkConstsDimensions,
          traceContext = traceContext,
          tracepointConditions = options.tracepointConditions
        )
      case CompilerStrategy.LocalVars =>
        new MemoryManager(
          tempSpace = tempSpace,
          tempAllocator = tempAllocator,
          ioSpace = dram0Space,
          varsSpace = localSpace,
          constsSpace = dram1Space,
          freeableAllocator = freeableAllocator,
          constsStream = constsStream,
          dataType = options.arch.dataType,
          arch = options.arch,
          mkConstsDimensions = frontend.mkConstsDimensions,
          traceContext = traceContext,
          tracepointConditions = options.tracepointConditions
        )
      case CompilerStrategy.LocalConsts | CompilerStrategy.LocalVarsAndConsts =>
        new MemoryManager(
          tempSpace = tempSpace,
          tempAllocator = tempAllocator,
          ioSpace = tempSpace,
          varsSpace = tempSpace,
          constsSpace = localSpace,
          freeableAllocator = freeableAllocator,
          constsStream = constsStream,
          dataType = options.arch.dataType,
          arch = options.arch,
          mkConstsDimensions = frontend.mkConstsDimensions,
          traceContext = traceContext,
          tracepointConditions = options.tracepointConditions
        )
    }

    if (
      options.strategy == CompilerStrategy.LocalConsts || options.strategy == CompilerStrategy.LocalVarsAndConsts
    ) {
      if (options.printProgress) {
        println("Allocating consts in local memory ...")
      }

      for (emitter <- flowEmitters) {
        emitter(
          EmitContext(
            hir = new NilHIR(),
            mm = mmPass1,
            outputNames = outputNames
          )
        )
      }
    }

    val (mmPass2, spacesToFree) = options.strategy match {
      case CompilerStrategy.LocalIsolated =>
        (mmPass1, Seq(dram0Space, dram1Space))
      case CompilerStrategy.LocalVars =>
        (mmPass1, Seq(dram0Space, dram1Space, localSpace))
      case CompilerStrategy.LocalConsts =>
        (
          new MemoryManager(
            tempSpace = tempSpace,
            tempAllocator = tempAllocator,
            ioSpace = dram0Space,
            varsSpace = dram0Space,
            constsSpace = localSpace,
            freeableAllocator = freeableAllocator,
            constsStream = constsStream,
            dataType = options.arch.dataType,
            arch = options.arch,
            mkConstsDimensions = frontend.mkConstsDimensions,
            traceContext = traceContext,
            tracepointConditions = options.tracepointConditions
          ),
          Seq(dram0Space, localSpace)
        )
      case CompilerStrategy.LocalVarsAndConsts =>
        (
          new MemoryManager(
            tempSpace = tempSpace,
            tempAllocator = tempAllocator,
            ioSpace = dram0Space,
            varsSpace = localSpace,
            constsSpace = localSpace,
            freeableAllocator = freeableAllocator,
            constsStream = constsStream,
            dataType = options.arch.dataType,
            arch = options.arch,
            mkConstsDimensions = frontend.mkConstsDimensions,
            traceContext = traceContext,
            tracepointConditions = options.tracepointConditions
          ),
          Seq(dram0Space, localSpace)
        )
    }

    val backend = new Backend(
      layout = layout,
      tracepointConditions = options.tracepointConditions,
      resolveRefToObject = (ref) =>
        freeableAllocator
          .resolveRefToObject(ref)
          .orElse(tempAllocator.resolveRefToObject(ref)),
      traceContext = traceContext
    )

    val graphPrinter =
      if (graphStream.isDefined)
        Some(new FrontendGraphPrinter(graphStream.get, "model"))
      else None

    for (emitter <- flowEmitters) {
      if (graphPrinter.isDefined)
        graphPrinter.get.startLayer(
          s"layer_${nextLayerIndex}"
        )

      val scheduler = schedulingContext.mkScheduler(nextLayerIndex)

      emitter(
        EmitContext(
          hir = scheduler,
          mm = mmPass2,
          outputNames = outputNames,
          graphPrinter = graphPrinter
        )
      )

      if (graphPrinter.isDefined) graphPrinter.get.endLayer()

      val r = scheduler.lower(backend)
      freeableAllocator.freeConsumedObjects(spacesToFree)

      if (r.numberOfStages != 0) {
        nextLayerIndex += 1
        layerSchedulerResults += r
      }
    }

    if (graphPrinter.isDefined) graphPrinter.get.endPrint

    freeableAllocator.consumeAllObjects(MemoryManager.ReservedConsumers.All)
    freeableAllocator.freeConsumedObjects(spacesToFree)

    require(freeableAllocator.isEmpty)

    backend.writeSegments(
      programStream,
      programAssemblyFilePath,
      Some(backendStats)
    )

    macs = layerSchedulerResults.map(_.macs).sum
    macEfficiency = Stats.macEfficiency(backendStats, options.arch, macs)

    val programSizeBytes =
      backend.instructionsCount * layout.instructionSizeBytes
    val stats =
      CompilerStats(
        constsVectorSize = mmPass1.constsVectorSize,
        layersNumber = layerSchedulerResults.size,
        programSizeBytes = programSizeBytes,
        constsScalarSize = mmPass1.constsScalarSize,
        constsUtilization = mmPass1.constsUtilization,
        cycles = backendStats.executionCycles,
        energy = backendStats.executionEnergy,
        macs = macs,
        macEfficiency = macEfficiency
      )

    val endTime = System.nanoTime()

    if (graphStream.isDefined) graphStream.get.close()

    if (options.printSummary) {
      val tb = new TablePrinter(Some("COMPILER SUMMARY"))

      tb.addNamedLine("Model", modelName)
      layout.addTableLines(tb)

      val dram0SpaceUsage = dram0Space.usage
      val dram1SpaceUsage = dram1Space.usage
      val accumulatorUsage = MemoryUsage(
        layerSchedulerResults.map(_.accumulatorUsage)
      )
      val localUsage = MemoryUsage(
        localSpace.usage +: layerSchedulerResults.map(_.localUsage)
      )

      if (dram0SpaceUsage.maxSize != 0)
        tb.addNamedLine(
          "DRAM0 maximum usage (vectors/scalars)",
          dram0SpaceUsage.maxSize,
          dram0SpaceUsage.maxSize * options.arch.arraySize
        )
      if (dram0SpaceUsage.aggSize != 0)
        tb.addNamedLine(
          "DRAM0 aggregate usage (vectors/scalars)",
          dram0SpaceUsage.aggSize,
          dram0SpaceUsage.aggSize * options.arch.arraySize
        )
      if (dram1SpaceUsage.maxSize != 0)
        tb.addNamedLine(
          "DRAM1 maximum usage (vectors/scalars)",
          dram1SpaceUsage.maxSize,
          dram1SpaceUsage.maxSize * options.arch.arraySize
        )
      if (dram1SpaceUsage.aggSize != 0)
        tb.addNamedLine(
          "DRAM1 aggregate usage (vectors/scalars)",
          dram1SpaceUsage.aggSize,
          dram1SpaceUsage.aggSize * options.arch.arraySize
        )
      if (localUsage.maxSize != 0)
        tb.addNamedLine(
          "Local memory maximum usage (vectors/scalars)",
          localUsage.maxSize,
          localUsage.maxSize * options.arch.arraySize
        )
      if (localUsage.aggSize != 0)
        tb.addNamedLine(
          "Local memory aggregate usage (vectors/scalars)",
          localUsage.aggSize,
          localUsage.aggSize * options.arch.arraySize
        )
      if (accumulatorUsage.maxSize != 0)
        tb.addNamedLine(
          "Accumumator memory maximum usage (vectors/scalars)",
          accumulatorUsage.maxSize,
          accumulatorUsage.maxSize * options.arch.arraySize
        )
      if (accumulatorUsage.aggSize != 0)
        tb.addNamedLine(
          "Accumumator memory aggregate usage (vectors/scalars)",
          accumulatorUsage.aggSize,
          accumulatorUsage.aggSize * options.arch.arraySize
        )
      tb.addNamedLine("Number of layers", layerSchedulerResults.size)
      if (!layerSchedulerResults.isEmpty) {
        tb.addNamedLine(
          "Maximum number of stages",
          layerSchedulerResults.map(_.numberOfStages).max
        )
        tb.addNamedLine(
          "Maximum number of partitions",
          layerSchedulerResults.map(_.numberOfPartitions).max
        )
      }
      Stats.printSummary(
        backendStats,
        tb,
        options.arch,
        Some(macs)
      )
      tb.addNamedLine(
        "Total number of instructions",
        backend.instructionsCount
      )
      tb.addNamedLine(
        "Compilation time (seconds)",
        (endTime - startTime).toFloat / 1e9f
      )
      tb.addNamedLine("True consts scalar size", mmPass1.constsScalarSize)
      tb.addNamedLine(
        "Consts utilization (%)",
        mmPass1.constsUtilization * 100f
      )
      val (macsLetter, macsDivisor) =
        Stats.getUnitsLetterAndDivisor(macs)
      tb.addNamedLine(
        s"True MACs (${macsLetter}MAC)",
        macs.toFloat / macsDivisor
      )
      tb.addNamedLine("MAC efficiency (%)", macEfficiency * 100f)
      print(tb)
    }

    if (options.printLayersSummary) {
      val layerSchedulerResultsWithIndex =
        layerSchedulerResults.zipWithIndex

      for (
        groupResultsWithIndex <- layerSchedulerResultsWithIndex.grouped(32)
      ) {
        val tb = new TablePrinter(Some("LAYERS SUMMARY"), true)
        tb.addLine(
          new TableLine(
            List("Layer:") ++ groupResultsWithIndex.map(_._2)
          )
        )
        tb.addLine(
          new TableLine(
            List(
              "Number of stages:"
            ) ++ groupResultsWithIndex
              .map(_._1.numberOfStages)
          )
        )
        tb.addLine(
          new TableLine(
            List(
              "Number of partitions:"
            ) ++ groupResultsWithIndex.map(_._1.numberOfPartitions)
          )
        )
        val (cyclesLetter, cyclesDivisor) =
          Stats.getUnitsLetterAndDivisor(
            groupResultsWithIndex
              .map(_._1.cycles)
              .max
          )
        tb.addLine(
          new TableLine(
            List(
              s"Latency (${cyclesLetter}Cycles):"
            ) ++ groupResultsWithIndex
              .map(_._1.cycles.toFloat)
              .map(_ / cyclesDivisor)
              .map(f => f"$f%.3f")
          )
        )
        val (energyLetter, energyDivisor) =
          Stats.getUnitsLetterAndDivisor(
            groupResultsWithIndex
              .map(_._1.energy)
              .max
          )
        tb.addLine(
          new TableLine(
            List(
              s"Energy (${energyLetter}Units):"
            ) ++ groupResultsWithIndex
              .map(_._1.energy.toFloat)
              .map(_ / energyDivisor)
              .map(f => f"$f%.3f")
          )
        )
        val (macsLetter, macsDivisor) =
          Stats.getUnitsLetterAndDivisor(
            groupResultsWithIndex
              .map(_._1.macs)
              .max
          )
        tb.addLine(
          new TableLine(
            List(
              s"True MACs (${macsLetter}MAC):"
            ) ++ groupResultsWithIndex
              .map(_._1.macs.toFloat)
              .map(_ / macsDivisor)
              .map(f => f"$f%.3f")
          )
        )
        tb.addLine(
          new TableLine(
            List(
              "MAC efficiency (%):"
            ) ++ groupResultsWithIndex
              .map(_._1.macEfficiency)
              .map(_ * 100f)
              .map(f => f"$f%.1f")
          )
        )
        tb.addLine(
          new TableLine(
            List(
              "Accumulator utilization (%):"
            ) ++ groupResultsWithIndex
              .map(
                _._1.accumulatorUsage.maxSize.toFloat / options.arch.accumulatorDepth.toFloat
              )
              .map(_ * 100f)
              .map(f => f"$f%.1f")
          )
        )
        tb.addLine(
          new TableLine(
            List("Local utilization (%):") ++ groupResultsWithIndex
              .map(
                _._1.localUsage.maxSize.toFloat / options.arch.threadLocalDepth.toFloat
              )
              .map(_ * 100f)
              .map(f => f"$f%.1f")
          )
        )
        print(tb)
      }

      if (options.printInstructionsSummary) {
        Stats.printCompositionSummary("TOTAL", backendStats)
        Stats.printCyclesSummary("TOTAL", backendStats)
        Stats.printEnergySummary("TOTAL", backendStats)
      }

      if (options.printStridesSummary) {
        def printStrideStats(
            title: String,
            select: StrideStats => Any
        ): Unit = {
          val tb = new TablePrinter(Some(title), true)
          Stats.printStrideStats(
            options.arch.stride0Depth,
            options.arch.stride1Depth,
            backendStats,
            select,
            tb
          )
          print(tb)
        }

        printStrideStats(
          "TOTAL STRIDES COUNT SUMMARY",
          stats => stats.count
        )
        printStrideStats(
          "TOTAL STRIDES MAX SIZE SUMMARY",
          stats => stats.maxSize
        )
        printStrideStats(
          "TOTAL STRIDES AVERAGE SIZE SUMMARY",
          stats => Math.round(stats.totalSize.toFloat / stats.count.toFloat)
        )
      }

      options.arch.dataType.reportAndResetOverUnderflowStats()
    }

    CompilerResult(
      arch = options.arch,
      inputObjects = mmPass2.inputObjects,
      outputObjects = mmPass2.outputObjects,
      stats = stats
    )
  }
}
