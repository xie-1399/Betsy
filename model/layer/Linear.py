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
import argparse
import os
'''
A simple linear layer inference (FP16 and Fixed Point)
'''
# simple linear class demo with FP16 and fixed point value
# the quantization supports different float points way
# loss : show the fixed point value compare to the float point
# the output check for the linear / using the fixed point
# onnx == 1.15.0 onnx_runtime == 1.17.0


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
def linear_onnx(activation_path, weight_path, onnx_path):
    fp_data = torch.load(activation_path)
    opset_version = 10  # raw compiler only support opset_version [9,10]
    model = linearLayer()
    convert(model, fp_data, weight_file=weight_path,
            onnx_file=onnx_path, opset_version=opset_version)
    opset_version_convert(onnx_path, 10, 5, onnx_path)


def to_numpy(tensor):
    return tensor.detach().cpu().numpy() if tensor.requires_grad else tensor.cpu().numpy()

'''
python3 Linear.py --exp 7 --man 8 --benchmark
'''

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="linear layer test parameters")
    parser.add_argument('--exp', type=int, default=7, help="the fixed point exp bit width")
    parser.add_argument('--man', type=int, default=8, help="the fixed point mantissa bit width")
    parser.add_argument('--path', type=str, default="./temp/", help="all generate checkpoint and files path")
    parser.add_argument('--benchmark', action='store_true', help="running the float answers")

    args = parser.parse_args()

    exponent_bits = args.exp
    mantissa_bits = args.man
    path = args.path
    weight_path = path + "Linear_64_256_10.pth"
    activation_path = path + "Linear_64_256_10.pt"
    result_path = path + "linear_result.txt"
    onnx_path = path + "Linear_64_256_10.onnx"

    os.makedirs(os.path.dirname(path), exist_ok=True)

    # (1) save the model random weight to the fixed_point
    model = linearLayer()
    print("saving the model weight...")
    state_dict = model.state_dict()
    for param_name in state_dict:
    # saving the weight as fixed point
        state_dict[param_name] = fixed_point_quantize(state_dict[param_name], wl=exponent_bits + mantissa_bits, fl=mantissa_bits, rounding="nearest")
        torch.set_printoptions(threshold=float('Inf'))
        with open(f"{path + param_name}.txt", "w") as file:
            file.write(str(state_dict[param_name]))
            file.close()
    model.load_state_dict(state_dict)
    torch.save(model.state_dict(), weight_path)

    # (2) save the input as pt
    fp_data = fixed_point_quantize(torch.arange(0, 64, dtype=torch.float32).reshape(1, 64),
                                   wl=exponent_bits + mantissa_bits, fl=mantissa_bits, rounding="nearest")
    torch.save(fp_data, activation_path)
    with open(f"{path}activation.txt", "w") as file:
        file.write(str(fp_data))
        file.close()

    # (3) inference and compare the fp result with fixed point result
    print("generate linear layer and compare with the fixed_point...")
    with torch.no_grad():
        model = linearLayerFP(loss=args.benchmark, exponent_bits=exponent_bits, mantissa_bits=mantissa_bits)
        model.load_state_dict(torch.load(weight_path))
        fp_data = torch.load(activation_path)
        if torch.cuda.is_available():
            model = model.to("cuda")
            fp_data = fp_data.to("cuda")
        result = model(fp_data)
        with open(result_path, "w") as f:
            f.write(str(result))
        print("quantization results:" + str(result))

    # (4) convert it to the onnx
    linear_onnx(activation_path, weight_path, onnx_path)

