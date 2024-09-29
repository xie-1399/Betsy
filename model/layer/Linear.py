import torch
import torch.nn as nn

from qtorch import FixedPoint, FloatingPoint
from qtorch.quant import Quantizer

'''
A simple linear layer inference (FP16 and Fixed Point) with Relu Function
'''

# simple linear class demo with FP16 and fixed point value
# the quantization supports different float points way

class linearLayer(nn.Module):
    def __init__(self, quantization:bool=True, exponent_bits=8, mantissa_bits=8):
        super().__init__()
        self.hidden = nn.Linear(64, 256, bias=False)
        self.output = nn.Linear(256, 10, bias=False)
        self.quantization = quantization

        wl = exponent_bits + mantissa_bits
        forward_num = FixedPoint(wl, fl=mantissa_bits)
        backward_num = FloatingPoint(exp=exponent_bits, man=mantissa_bits)
        # a quantize function
        self.Q = Quantizer(forward_number=forward_num, backward_number=backward_num,
                      forward_rounding="nearest", backward_rounding="stochastic")

    def forward(self, x):

        if self.quantization:
            self.hidden.weight = self.Q(self.hidden.weight)
            self.out.weight = self.Q(self.out.weight)

        out = self.output(self.hidden(x))
        out = self.Q(out) if self.quantization else out
        return out


if __name__ == '__main__':
    print("generate linear layer ...")
    with torch.no_grad():
        fp_data = torch.randn((1,64))
        model_fp = linearLayer(quantization = False)
        model_fix = linearLayer(quantization = True, exponent_bits = 8 , mantissa_bits = 8)
        print("raw result:" + str(model_fp(fp_data)))
        print("quantization result:" + str(model_fix(fp_data)))