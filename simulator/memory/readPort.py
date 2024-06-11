"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.8
"""


class readPort:

    def __init__(self, read_Async=False):
        self.latency = 1  # the Buffer read is Sync like
        if read_Async:
            self.latency = 0

    def set_params(self, latency):
        self.latency = latency

    def get_latency(self):
        return self.latency
    
