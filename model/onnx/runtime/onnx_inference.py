# inference the onnx model with onnx-runtime

import onnxruntime as ort

# Load the model and create InferenceSession
model_path = "../resnet20v2_cifar.onnx"
session = ort.InferenceSession(model_path)


# "Load and preprocess the input image inputTensor"




# Run inference
outputs = session.run(None, {"input": inputTensor})
print(outputs)