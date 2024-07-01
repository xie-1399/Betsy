package tensil.tools

import tensil.common.Architecture
import tensil.common.TablePrinter

import java.io.File
import java.nio.file.{Paths, Files}

object generator extends App{
  // Todo update the file path (can generate)
  val genArg = Args(
    archFile = new File("/media/xxl/Betsy/software/src/tensil/tools/arch.tarch"),
    modelFile = new File("/media/xxl/Betsy/model/onnx/resnet20v2_cifar.onnx"),
    outputNodes = Seq("Identity:0"),
    targetDir = new File("/media/xxl/Betsy/software/src/tensil/tools/gen"),
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
