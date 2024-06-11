"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.11
"""


class Load:
    def __init__(self, loadZero, arraySize, loadSize, readPortLatency, weight):
        """
        :param loadZero: load value is zero ?
        :param arraySize: the array size
        :param loadSize:  load size
        :param readPortLatency: read port sync latency
        :param weight:  weight value -> a matrix
        """
        self.opcode = None
        self.zeroLatency = None
        self.startLatency = 1
        self.loadZero = loadZero

        self.readPortLatency = readPortLatency
        self.loadSize = loadSize
        self.arraySize = arraySize
        self.weight = weight

    def get_weight(self):
        return self.weight

    def set_zeroLatency(self):
        self.zeroLatency = 1

    def load_zeroLatency(self, size):
        self.set_zeroLatency()
        return size * self.zeroLatency

    def set_opcode(self):
        self.opcode = 3

    def read_latency(self):
        readCycles = self.startLatency + self.readPortLatency * self.loadSize  # depends read port latency
        return readCycles

    # load instruction + load zero + nop -> finish the load weight
    def load_systolicArrayLatency(self, nop):
        self.set_zeroLatency()
        return self.zeroLatency + self.read_latency() + nop

    # load instruction
    def get_latency(self):
        if self.loadZero:
            return self.load_zeroLatency(self.loadSize)
        else:
            return self.read_latency()
