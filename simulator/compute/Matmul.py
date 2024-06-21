"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.19
"""

# compute the activation and weight and get the cycles
# the v1 version use the numpy to compute

import numpy as np


class Matmul:
    def __init__(self, activation, multiZero, arraySize):
        self.activation = activation
        self.opcode = None
        self.startLatency = 1
        self.arraySize = arraySize
        self.multiZero = multiZero
        self.load_cycles = activation.shape[1] if multiZero else activation.shape[1] + self.startLatency

    def compute(self, weight):
        assert weight.shape[1] == self.activation.shape[0], "the matrix matmul size not match"
        if self.multiZero:
            return np.zeros(shape=(weight.shape[0], self.activation.shape[1]))
        else:
            return np.matmul(weight, self.activation)

    def set_opcode(self):
        self.opcode = 1

    # assume the PE array is square -> (start cycle + height + width + height - 1 )
    def get_cycles(self):
        return self.load_cycles + self.arraySize * 2

    def get_activation(self):
        return self.activation
