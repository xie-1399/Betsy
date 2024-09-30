# here provide some functions to convert the model to the onnx format
# the onnx version and onnx runtime version is at
# https://onnxruntime.ai/docs/reference/compatibility.html#onnx-opset-support


import torch
import torch.nn as nn
import torch.nn.functional as F
import onnxruntime

'''
pytorch model to the onnx demo 
'''


class MyModel(nn.Module):

    def __init__(self):
        super(MyModel, self).__init__()
        self.conv1 = nn.Conv2d(1, 6, 5)
        self.conv2 = nn.Conv2d(6, 16, 5)
        self.fc1 = nn.Linear(16 * 5 * 5, 120)
        self.fc2 = nn.Linear(120, 84)
        self.fc3 = nn.Linear(84, 10)

    def forward(self, x):
        x = F.max_pool2d(F.relu(self.conv1(x)), (2, 2))
        x = F.max_pool2d(F.relu(self.conv2(x)), 2)
        x = torch.flatten(x, 1)
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)
        return x


# use Netron can explain the onnx format
def save_onnx(model, torch_input):
    onnx_program = torch.onnx.dynamo_export(model, torch_input)
    onnx_program.save("my_image_classifier.onnx")


def run(onnx_program, torch_input):
    onnx_input = onnx_program.adapt_torch_inputs_to_onnx(torch_input)
    print(f"Input length: {len(onnx_input)}")
    print(f"Sample input: {onnx_input}")

    ort_session = onnxruntime.InferenceSession("./my_image_classifier.onnx", providers=['CPUExecutionProvider'])

    def to_numpy(tensor):
        return tensor.detach().cpu().numpy() if tensor.requires_grad else tensor.cpu().numpy()

    onnxruntime_input = {k.name: to_numpy(v) for k, v in zip(ort_session.get_inputs(), onnx_input)}
    onnxruntime_outputs = ort_session.run(None, onnxruntime_input)
    return onnxruntime_outputs


if __name__ == '__main__':
    torch_model = MyModel()
    torch_input = torch.randn(1, 1, 32, 32)

    save_onnx(torch_model, torch_input)  # save the onnx format

    onnx_program = torch.onnx.dynamo_export(torch_model, torch_input)
    onnxruntime_outputs = run(onnx_program, torch_input)  # run the onnx-runtime

    torch_outputs = torch_model(torch_input)
    torch_outputs = onnx_program.adapt_torch_outputs_to_onnx(torch_outputs)

    assert len(torch_outputs) == len(onnxruntime_outputs)
    for torch_output, onnxruntime_output in zip(torch_outputs, onnxruntime_outputs):
        torch.testing.assert_close(torch_output, torch.tensor(onnxruntime_output))

    print("PyTorch and ONNX Runtime output matched!")
    print(f"Output length: {len(onnxruntime_outputs)}")
    print(f"Sample output: {onnxruntime_outputs}")

