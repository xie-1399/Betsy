"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.11
"""


# configure the betsy registers takes one cycle

class Configure:
    def __init__(self):
        self.opcode = None
        self.latency = 1

    def set_opcode(self):
        self.opcode = 15

    def get_latency(self):
        return self.latency
