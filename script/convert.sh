#!/bin/bash

cp ../software/src/tensil/tools/gen/Linear_64_256_10_op10_onnx_normal.tdata Linear_64_256_10_op10_onnx_normal.bin
python tdata2fix.py
