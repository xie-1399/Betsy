import torch
import torch.nn as nn
from torch.nn import Parameter
from qtorch import FixedPoint, FloatingPoint
from qtorch.quant import Quantizer,fixed_point_quantize, block_quantize, float_quantize

'''
A simple linear layer inference (FP16 and Fixed Point) with Relu Function
'''

# simple linear class demo with FP16 and fixed point value
# the quantization supports different float points way
# loss : show the fixed point value compare to the float point
class linearLayer(nn.Module):
    def __init__(self, quantization:bool=True, loss:bool=False,exponent_bits=8, mantissa_bits=8):
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
           print("raw result:\n")
           print(self.output(self.hidden(x)))

        if self.quantization:
            self.hidden.weight = Parameter(fixed_point_quantize(self.hidden.weight, wl=self.wl, fl=self.fl, rounding="nearest"))
            self.output.weight = Parameter(fixed_point_quantize(self.output.weight, wl=self.wl, fl=self.fl, rounding="nearest"))

        x = fixed_point_quantize(x, wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else x
        out = self.output(self.hidden(x))
        out = fixed_point_quantize(out, wl=self.wl, fl=self.fl, rounding="nearest") if self.quantization else out
        return out



if __name__ == '__main__':
    print("generate linear layer ...")

    with torch.no_grad():
        fp_data = torch.randint(100,200,(1,64)).to(torch.float32)
        model_fix = linearLayer(quantization = True, loss = True, exponent_bits = 8 , mantissa_bits = 8)
        print("quantization result:" + str(model_fix(fp_data)))
