import torch
import torch.nn as nn
from torch.nn import Parameter
from qtorch import FixedPoint, FloatingPoint
from qtorch.quant import Quantizer,fixed_point_quantize, block_quantize, float_quantize

import sys
sys.path.append("..")
from untils.convert_onnx import convert, opset_version_convert
from onnx import version_converter, helper
import onnxruntime

'''
A simple linear layer inference (FP16 and Fixed Point)
'''

# simple linear class demo with FP16 and fixed point value
# the quantization supports different float points way
# loss : show the fixed point value compare to the float point
# the output check for the linear / using the fixed point
# onnx == 1.15.0 onnx_runtime == 1.17.0


'''
(1) define the linear model with pytorch

(2) define the model with FixedPoint for test

(3) save the weight and input data / convert the model to onnx

(4) compare 3 results for raw/FixedPoint/onnx

'''

class linearLayerFP(nn.Module):
    def __init__(self, quantization: bool = True, loss: bool = False, exponent_bits=8, mantissa_bits=8):
        super().__init__()
        self.quantization = quantization
        self.loss = loss
        self.wl = exponent_bits + mantissa_bits
        self.fl = mantissa_bits
        self.hidden = nn.Linear(64, 256, bias=False)
        self.output = nn.Linear(256, 10, bias=False)
    # the weight and activation are all to be fixed point
    def forward(self, x):
        if self.loss:
           print("raw results:")
           print(self.output(self.hidden(x)))
        if self.quantization:
            self.hidden.weight = Parameter(fixed_point_quantize(self.hidden.weight, wl=self.wl, fl=self.fl, rounding="nearest"))
            self.output.weight = Parameter(fixed_point_quantize(self.output.weight, wl=self.wl, fl=self.fl, rounding="nearest"))

        x = fixed_point_quantize(x, wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else x
        hidden = fixed_point_quantize(self.hidden(x), wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else self.hidden(x)
        out = fixed_point_quantize(self.output(hidden), wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else self.output(hidden)
        return out


# the simple linear layer
class linearLayer(nn.Module):
    def __init__(self):
        super().__init__()
        self.hidden = nn.Linear(64, 256, bias=False)
        self.output = nn.Linear(256, 10, bias=False)
    def forward(self, x):
        hidden = self.hidden(x)
        out = self.output(hidden)
        return out


# convert the onnx to the onnx
def linear_onnx():
    fp_data = torch.load("../checkpoint/Linear_64_256_10.pt")
    opset_version = 10  # raw compiler only support opset_version [9,10]
    model = linearLayer()
    convert(model, fp_data, weight_file="../checkpoint/Linear_64_256_10.pth",
            onnx_file="../checkpoint/onnx/Linear_64_256_10.onnx", opset_version=opset_version)
    opset_version_convert("../checkpoint/onnx/Linear_64_256_10.onnx", 10, 5, "../checkpoint/onnx/Linear_64_256_10_op10.onnx")


def to_numpy(tensor):
    return tensor.detach().cpu().numpy() if tensor.requires_grad else tensor.cpu().numpy()


# running with the onnx model
def onnx_running():
    onnx_file = "../checkpoint/onnx/Linear_64_256_10_op10.onnx"
    return


if __name__ == '__main__':

    exponent_bits = 7
    mantissa_bits = 8
    Path = "../checkpoint/Linear_64_256_10.pth"
    # (1) save the model random weight to the fixed_point
    model = linearLayer()
    print("saving the model weight ...")
    state_dict = model.state_dict()
    for param_name in state_dict:
        state_dict[param_name] = fixed_point_quantize(state_dict[param_name], wl=exponent_bits + mantissa_bits, fl=mantissa_bits, rounding="nearest")
    model.load_state_dict(state_dict)
    torch.save(model.state_dict(), Path)
    print("saving the model weight to the fixed point ...")

    # (2) save the input as pt
    fp_data = fixed_point_quantize(torch.randint(1, 16, (1, 64)).to(torch.float32), wl=exponent_bits + mantissa_bits, fl=mantissa_bits, rounding="nearest")
    torch.save(fp_data, "../checkpoint/Linear_64_256_10.pt")

    # (3) inference and compare the fp result with fixed point result
    print("generate linear layer and compare with the fixed_point...")
    with torch.no_grad():
        model = linearLayerFP(loss=True, exponent_bits=exponent_bits, mantissa_bits=mantissa_bits)
        model.load_state_dict(torch.load(Path))
        fp_data = torch.load("../checkpoint/Linear_64_256_10.pt")
        if torch.cuda.is_available():
            model = model.to("cuda")
            fp_data = fp_data.to("cuda")
          # show weight value
#         for name in model.state_dict():
#           print(name)
#           print(model.state_dict()[name])
        result = model(fp_data)
        with open("linear_result.txt", "w") as f:
            print("write the result to the file")
            f.write(str(result))
        print("quantization results:" + str(result))

    # (4) convert it to the onnx
    linear_onnx()

