import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.nn import Parameter
from qtorch import FixedPoint, FloatingPoint
from qtorch.quant import Quantizer,fixed_point_quantize, block_quantize, float_quantize
import sys
sys.path.append("..")
from untils.convert_onnx import convert, opset_version_convert
import argparse
import os
'''
A simple Conv layer inference (FP16 and Fixed Point) with Relu Function
'''

class Conv(nn.Module):
    def __init__(self):
        super(Conv, self).__init__()
        self.conv1 = nn.Conv2d(in_channels=1, out_channels=3, kernel_size=3, stride=1)
#         self.fc1 = nn.Linear(8 * 14 * 14, 10)

    def forward(self, x):
        # Convolution -> Pooling -> Fully connected
        x = F.relu(self.conv1(x))
#         x = F.max_pool2d(x, 2)
#         x = x.view(-1, 8 * 14 * 14)
#         x = self.fc1(x)
        return x

class ConvFP(nn.Module):
    def __init__(self, quantization: bool = True, loss: bool = False, exponent_bits=8, mantissa_bits=8):
        super().__init__()
        self.quantization = quantization
        self.loss = loss
        self.wl = exponent_bits + mantissa_bits
        self.fl = mantissa_bits
        self.conv1 = nn.Conv2d(in_channels=1, out_channels=3, kernel_size=3, stride=1)
#         self.fc1 = nn.Linear(8 * 14 * 14, 10)

    # the weight and activation are all to be fixed point
    def forward(self, x):
        if self.loss:
           print("raw results:")
           x = F.relu(self.conv1(x))
#            x = F.max_pool2d(x, 2)
#            x = x.view(-1, 8 * 14 * 14)
#            x = self.fc1(x)
           print(x)
        if self.quantization:
            self.conv1.weight = Parameter(fixed_point_quantize(self.conv1.weight, wl=self.wl, fl=self.fl, rounding="nearest"))
#             self.fc1.weight = Parameter(fixed_point_quantize(self.fc1.weight, wl=self.wl, fl=self.fl, rounding="nearest"))
        x = fixed_point_quantize(x, wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else x
        out = fixed_point_quantize(F.relu(self.conv1(x)), wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else F.relu(self.conv1(x))
#         poolingOut = fixed_point_quantize(F.max_pool2d(conv1Out, 2), wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else F.max_pool2d(conv1Out, 2)
#         poolingOut = poolingOut.view(-1, 8 * 14 * 14)
#         out = fixed_point_quantize(self.fc1(poolingOut), wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else self.fc1(poolingOut)
        return out

# convert the onnx to the onnx
def conv_onnx(activation_path, weight_path, onnx_path):
    fp_data = torch.load(activation_path)
    opset_version = 10  # raw compiler only support opset_version [9,10]
    model = Conv()
    convert(model, fp_data, weight_file=weight_path,
            onnx_file=onnx_path, opset_version=opset_version)
    opset_version_convert(onnx_path, 10, 5, onnx_path)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="convolution layer test parameters")
    parser.add_argument('--exp', type=int, default=7, help="the fixed point exp bit width")
    parser.add_argument('--man', type=int, default=8, help="the fixed point mantissa bit width")
    parser.add_argument('--path', type=str, default="../../temp/conv/", help="all generate checkpoint and files path")
    parser.add_argument('--benchmark', action='store_true', help="running the float answers")

    args = parser.parse_args()

    exponent_bits = args.exp
    mantissa_bits = args.man
    path = args.path
    weight_path = path + "Conv_3_8_196.pth"
    activation_path = path + "Conv_3_8_196.pt"
    result_path = path + "Conv_result.txt"
    onnx_path = path + "Conv_3_8_196.onnx"

    os.makedirs(os.path.dirname(path), exist_ok=True)

    # (1) save the model random weight to the fixed_point
    model = Conv()
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
    # if set all activation to 1
    input_tensor = torch.arange(1 * 10, 7 * 10, 10).repeat(6, 1).float()
    input_tensor = input_tensor.unsqueeze(0).unsqueeze(0)
    fp_data = fixed_point_quantize(input_tensor, wl=exponent_bits + mantissa_bits,
                                   fl=mantissa_bits, rounding="nearest")
    print(fp_data)
    # fp_data = fixed_point_quantize(torch.ones(1,64), wl=exponent_bits + mantissa_bits, fl=mantissa_bits, rounding="nearest")
    torch.save(fp_data, activation_path)
    with open(f"{path}activation.txt", "w") as file:
        file.write(str(fp_data))
        file.close()

    # (3) inference and compare the fp result with fixed point result
    print("generate convolution layer and compare with the fixed_point...")
    with torch.no_grad():
        model = ConvFP(loss=args.benchmark, exponent_bits=exponent_bits, mantissa_bits=mantissa_bits)
        model.load_state_dict(torch.load(weight_path))
        fp_data = torch.load(activation_path)
        if torch.cuda.is_available():
            model = model.to("cuda")
            fp_data = fp_data.to("cuda")
        result = model(fp_data)
        with open(result_path, "w") as f:
            for value in result.flatten():
                f.write(str(value.item()) + "\n")
        print("quantization results:" + str(result))

    # (4) convert it to the onnx
    conv_onnx(activation_path, weight_path, onnx_path)
