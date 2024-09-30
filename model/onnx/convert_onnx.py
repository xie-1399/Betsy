# some tools to convert the model with weight to onnx format

import torch
import torch.onnx
from pathlib import Path
import sys
import onnx
from onnx import version_converter, helper


wd = Path(__file__).parent.parent.resolve()
sys.path.append(str(wd))

from mlp import FeedForwardNetwork

"""
convert pytorch model -> onnx
the input_data can be randomly the input shape
"""


def convert(torch_model, input_data, weight_file,
            onnx_file, opset_version: int = 10, forward: bool = True):
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    weight = torch.load(weight_file)

    input_data = input_data.to(device)
    torch_model.load_state_dict(weight)
    torch_model = torch_model.to(device)

    torch_model.eval()
    # export the onnx model out
    torch.onnx.export(torch_model,
                      input_data,
                      onnx_file,
                      export_params=True,
                      opset_version=opset_version,
                      do_constant_folding=True
                      )
    print("convert the onnx finish")

    if forward:
        output = torch_model(input_data)
        print(f"the output is {output} ")
    return 0


# opset 9 -> ir version 3
# opset 10 -> ir version 5
def opset_version_convert(onnx_file, opset_version, ir_version, new_onnx_file):

    original_model = onnx.load(onnx_file)
    print("origin version : " + str(original_model.opset_import[0].version))

    original_model.opset_import[0].version = opset_version
    original_model.ir_version = ir_version

    onnx.save(original_model, new_onnx_file)
    print("convert version : " + str(original_model.opset_import[0].version))


# a demo for the feed forward network to convert the onnx format
def feed_forward_network_demo():
    images = torch.randn(1, 28 * 28)
    input_size = 784
    hidden_size = 500
    num_classes = 10
    opset_version = 10  # raw compiler only support opset_version [9,10]

    model = FeedForwardNetwork.NeuralNet(input_size, hidden_size, num_classes)
    print(model)
    convert(model, images, weight_file="../checkpoint/FeedForwardNetwork.pth",
            onnx_file="./FeedForwardNetwork.onnx", opset_version=opset_version)


if __name__ == '__main__':

    # feed_forward_network_demo()
    opset_version_convert("../checkpoint/onnx/FeedForwardNetwork.onnx", 10, 5, "./new_FeedForwardNetwork.onnx")
