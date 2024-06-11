"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.11
"""


# the nop instruction do nothing and takes one cycle

class Nop:
    def __init__(self):
        self.opcode = None
        self.latency = 1

    def set_params(self, latency):
        self.latency = latency

    def get_latency(self):
        return self.latency

    def set_opcode(self):
        self.opcode = 0

