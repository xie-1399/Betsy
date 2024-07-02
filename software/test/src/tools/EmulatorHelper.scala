/* SPDX-License-Identifier: Apache-2.0 */
/* Copyright © 2019-2022 Tensil AI Company */

package tensil.tools

import java.io._
import scala.reflect.ClassTag
import tensil.tools.emulator.{Emulator, ExecutiveTraceContext, ExecutiveTrace}
import scala.collection.mutable
import tensil.{
  Architecture,
  ArchitectureDataType,
  ArchitectureDataTypeWithBase,
  NumericWithMAC,
  FloatAsIfIntegralWithMAC
}
import tensil.tools.model.Model

object EmulatorHelper {
  def test(
      modelName: String,
      inputBatchSize: Int = 1,
      localConsts: Boolean = false,
      traceContext: ExecutiveTraceContext = ExecutiveTraceContext.default
  ): Unit = {
    val modelStream = new FileInputStream(s"$modelName.tmodel")
    val model       = upickle.default.read[Model](modelStream)
    modelStream.close()

    model.arch.dataType.name match {
      case ArchitectureDataType.FLOAT32.name =>
        implicit val numericWithMAC = FloatAsIfIntegralWithMAC

        doTest(
          ArchitectureDataType.FLOAT32,
          model,
          inputBatchSize,
          localConsts,
          traceContext
        )

      case ArchitectureDataType.FP32BP16.name =>
        doTest(
          ArchitectureDataType.FP32BP16,
          model,
          inputBatchSize,
          localConsts,
          traceContext
        )

      case ArchitectureDataType.FP18BP10.name =>
        doTest(
          ArchitectureDataType.FP18BP10,
          model,
          inputBatchSize,
          localConsts,
          traceContext
        )

      case ArchitectureDataType.FP16BP8.name =>
        doTest(
          ArchitectureDataType.FP16BP8,
          model,
          inputBatchSize,
          localConsts,
          traceContext
        )

      case ArchitectureDataType.FP8BP4.name =>
        doTest(
          ArchitectureDataType.FP8BP4,
          model,
          inputBatchSize,
          localConsts,
          traceContext
        )
    }
  }

  val yoloPattern = "yolov4_tiny_([0-9]+)_.*".r

  private def prepareInputStream(
      modelName: String,
      dataType: ArchitectureDataType,
      arraySize: Int,
      count: Int = 1
  ): InputStream =
    if (modelName.startsWith("xor"))
      Xor.prepareInputStream(dataType, arraySize, count)
    else if (modelName.startsWith("mlp_mnist"))
      Mnist.prepareInputStream(arraySize, count, false)
    else if (modelName.startsWith("maxpool"))
      MaxPool.prepareInputStream(dataType, arraySize)
    else if (modelName.startsWith("conv2d_non_square"))
      Conv2D.prepareInputStream(dataType, arraySize, 3, 5)
    else if (modelName.startsWith("conv2d"))
      Conv2D.prepareInputStream(dataType, arraySize, 3, 3)
    else if (modelName.startsWith("cnn_mnist"))
      Mnist.prepareInputStream(arraySize, count, true)
    else if (modelName.startsWith("resnet20v2"))
      Cifar.prepareInputStream(dataType, arraySize, count)
    else if (
      modelName.startsWith("resnet50v2") || modelName.startsWith("mobilenetv2")
    )
      ImageNet.prepareInputStream(dataType, arraySize, count)
    else if (modelName.startsWith("reshape_1d_4d"))
      Reshape.prepareInputStream(dataType, arraySize, count)
    else if (yoloPattern.findFirstIn(modelName).isDefined) {
      val yoloPattern(yoloSize) = modelName
      TinyYolo(yoloSize.toInt, onnx = modelName.endsWith("onnx"))
        .prepareInputStream(
          dataType,
          arraySize,
          count
        )
    } else if (modelName.startsWith("speech_commands"))
      SpeechCommands.prepareInputStream(dataType, arraySize, count)
    else
      throw new IllegalArgumentException()

  private def assertOutput(
      modelName: String,
      outputName: String,
      dataType: ArchitectureDataType,
      arraySize: Int,
      bytes: Array[Byte],
      count: Int = 1
  ): Unit =
    if (modelName.startsWith("xor"))
      Xor.assertOutput(dataType, arraySize, bytes, count)
    else if (modelName.startsWith("maxpool"))
      MaxPool.assertOutput(dataType, arraySize, bytes)
    else if (modelName.matches("conv2d_4x4_valid((_tiled)|(_oversized))?"))
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.ValidStride1Pixels)
    else if (
      modelName.matches("conv2d_non_square_4x4_valid((_tiled)|(_oversized))?")
    )
      Conv2D.assertOutput(
        dataType,
        arraySize,
        bytes,
        Conv2D.ValidNonSquareStride1Pixels
      )
    else if (
      modelName.matches("conv2d_4x4_valid_stride_2((_tiled)|(_oversized))?")
    )
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.ValidStride2Pixels)
    else if (modelName.matches("conv2d_4x4_same((_tiled)|(_oversized))?"))
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.SameStride1Pixels)
    else if (
      modelName.matches("conv2d_non_square_4x4_same((_tiled)|(_oversized))?")
    )
      Conv2D.assertOutput(
        dataType,
        arraySize,
        bytes,
        Conv2D.SameNonSquareStride1Pixels
      )
    else if (
      modelName.matches("conv2d_4x4_same_stride_2((_tiled)|(_oversized))?")
    )
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.SameStride2Pixels)
    else if (modelName.matches("conv2d_4x4_valid((_tiled)|(_oversized))?"))
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.ValidStride1Pixels)
    else if (modelName.matches("conv2d_4x4_valid((_tiled)|(_oversized))?"))
      Conv2D.assertOutput(dataType, arraySize, bytes, Conv2D.ValidStride1Pixels)
    else if (modelName == "conv2d_4x4_same_relu_2x2_maxpool_valid_stride_2")
      Conv2D.assertOutput(
        dataType,
        arraySize,
        bytes,
        Conv2D.SameReluMaxPoolValidStride2Pixels
      )
    else if (modelName == "conv2d_4x4_same_relu_2x2_maxpool_valid_stride_1")
      Conv2D.assertOutput(
        dataType,
        arraySize,
        bytes,
        Conv2D.SameReluMaxPoolValidStride1Pixels
      )
    else if (
      modelName.startsWith("mlp_mnist") || modelName.startsWith("cnn_mnist")
    )
      Mnist.assertOutput(dataType, arraySize, bytes, count)
    else if (modelName.startsWith("resnet20v2"))
      Cifar.assertOutput(dataType, arraySize, bytes, count)
    else if (modelName.startsWith("resnet50v2"))
      ImageNet.assertOutput(
        dataType,
        arraySize,
        bytes,
        count,
        Seq(
          386, // African_elephant
          248, // Eskimo_dog
          285  // Egyptian_cat
        )
      )
    else if (modelName.startsWith("mobilenetv2"))
      ImageNet.assertOutput(
        dataType,
        arraySize,
        bytes,
        count,
        Seq(
          386, // African_elephant
          273, // dingo
          285  // Egyptian_cat
        )
      )
    else if (modelName.startsWith("reshape_1d_4d"))
      Reshape.assertOutput(dataType, arraySize, bytes, count)
    else if (yoloPattern.findFirstIn(modelName).isDefined) {
      val yoloPattern(yoloSize) = modelName
      TinyYolo(yoloSize.toInt, onnx = modelName.endsWith("onnx"))
        .assertOutput(outputName, dataType, arraySize, bytes)
    } else if (modelName.startsWith("speech_commands"))
      SpeechCommands.assertOutput(dataType, arraySize, bytes, count)
    else
      throw new IllegalArgumentException()

  private def minimumInputCount(modelName: String): Int =
    if (modelName.startsWith("xor"))
      4
    else if (
      modelName.startsWith("resnet50v2") || modelName.startsWith("mobilenetv2")
    )
      3
    else if (
      modelName
        .startsWith("resnet20v2") || modelName.startsWith("speech_commands")
    )
      10
    else
      1

  private def doTest[T : NumericWithMAC : ClassTag](
      dataType: ArchitectureDataTypeWithBase[T],
      model: Model,
      inputBatchSize: Int,
      localConsts: Boolean,
      traceContext: ExecutiveTraceContext
  ): Unit = {
    val emulator = new Emulator(
      dataType = dataType,
      arch = model.arch
    )

    val constsStream = new FileInputStream(model.consts(0).fileName)

    if (localConsts)
      emulator.writeLocal(
        model.consts(0).size,
        constsStream
      )
    else
      emulator.writeDRAM1(
        model.consts(0).size,
        constsStream
      )

    constsStream.close()

    val outputs = mutable.Map.empty[String, ByteArrayOutputStream]
    val count   = Math.max(inputBatchSize, minimumInputCount(model.name))

    val input = model.inputs(0)
    val inputStream =
      prepareInputStream(model.name, dataType, model.arch.arraySize, count)

    for (_ <- 0 until count / inputBatchSize) {
      emulator.writeDRAM0(
        input.base until input.base + input.size,
        new DataInputStream(inputStream)
      )

      val trace         = new ExecutiveTrace(traceContext)
      val programStream = new FileInputStream(model.program.fileName)

      emulator.run(programStream, trace)

      programStream.close()
      trace.printTrace()

      for (output <- model.outputs) {
        val outputStream =
          outputs.getOrElseUpdate(output.name, new ByteArrayOutputStream())
        emulator.readDRAM0(
          output.base until output.base + output.size,
          new DataOutputStream(outputStream)
        )
      }
    }

    for ((name, outputStream) <- outputs)
      assertOutput(
        model.name,
        name,
        dataType,
        model.arch.arraySize,
        outputStream.toByteArray,
        count
      )
  }
}
