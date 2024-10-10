package tensil.tools

import tensil.common.Architecture
import tensil.common.TablePrinter

import java.io.File
import java.nio.file.{Paths, Files}

case class Args(
                 archFile: File = new File("."),
                 modelFile: File = new File("."),
                 outputNodes: Seq[String] = Seq("Identity"),
                 inputShapes: String = "[1]",
                 verbose: Boolean = false,
                 summary: Boolean = false,
                 layersSummary: Boolean = false,
                 schedulerSummary: Boolean = false,
                 partitionsSummary: Boolean = false,
                 stridesSummary: Boolean = false,
                 instructionsSummary: Boolean = false,
                 writeGraph: Boolean = false,
                 writeProgramAssembly: Boolean = false,
                 targetDir: File = new File("."),
                 strategy: CompilerStrategy.Kind = CompilerStrategy.LocalIsolated,
               )


object Generator extends App{
  // Todo update the file path (can generate)
  val genArg = Args(
    archFile = new File("/home/xie/Betsy/software/src/tensil/tools/arch/normal.tarch"),
    modelFile = new File("/home/xie/Betsy/model/checkpoint/onnx/Linear_64_256_10_op10.onnx"),
    outputNodes = Seq("Identity"),
    targetDir = new File("/home/xie/Betsy/software/src/tensil/tools/gen"),
    summary = true,
    instructionsSummary = true,
    writeProgramAssembly = true
  )

  val arch = Architecture.read(genArg.archFile)

  val archName = genArg.archFile.getName().split("\\.")(0)
  val modelName =
    s"${genArg.modelFile.getName().replaceAll("[^a-zA-Z\\d\\s:]", "_")}_${archName}"
  val targetDir = genArg.targetDir.getCanonicalPath()
  Files.createDirectories(Paths.get(targetDir))

  val options = CompilerOptions(
    arch = arch,
    strategy = genArg.strategy,
    inputShapes = CompilerInputShapes.parse(genArg.inputShapes),
    printProgress = genArg.verbose,
    printSummary = genArg.summary,
    printLayersSummary = genArg.layersSummary,
    printSchedulerSummary = genArg.schedulerSummary,
    printPartitionsSummary = genArg.partitionsSummary,
    printStridesSummary = genArg.stridesSummary,
    printInstructionsSummary = genArg.instructionsSummary,
    printGraph = genArg.writeGraph,
    printProgramAssembly = genArg.writeProgramAssembly,
    targetPath = Some(targetDir)
  )

  try {
    val r = Compiler.compile(
      modelName,
      genArg.modelFile.getPath(),
      genArg.outputNodes,
      options
    )

    val tb = new TablePrinter(Some("ARTIFACTS"))

    for (artifact <- r.artifacts)
      tb.addNamedLine(
        artifact.kind,
        new File(artifact.fileName).getAbsolutePath()
      )

    print(tb)
  } catch {
    case e: CompilerException =>
      println(s"Error: ${e.getMessage()}")
      sys.exit(1)
  }
  
}
