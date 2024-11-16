#!/bin/bash
# Betsy follow the MiT Licence.(c) xxl, All rights reserved
# run the conv layer test in the Betsy
# Author zhangtr  Time:2024.6.5  Version:1.0

set -e

parent_dir=$(dirname "$(pwd)")
cd $parent_dir/model/layer
python3 Conv.py --exp 7 --man 8 --path ./temp/conv/
rm -rf $parent_dir/temp
mv temp $parent_dir/

# then generate the instruction files from the linear layer
cd $parent_dir
sbt 'runMain tensil.tools.Generator normal conv/Conv_3_8_196.onnx'

# convert it to the txt
cd script
python3 binaryhandler.py --input ../temp/conv/Conv_3_8_196_onnx_normal.tprog --output ../temp/conv/Conv_3_8_196_onnx_normal.txt --len 64
echo "convert the instruction"
python3 binaryhandler.py --input ../temp/conv/Conv_3_8_196_onnx_normal.tdata --output ../temp/conv/tdata.txt --len 16

# convert the tdata to the binary
cd $parent_dir
cp temp/conv/Conv_3_8_196_onnx_normal.tdata temp/conv/Conv_3_8_196_onnx_normal.bin
cd script
python3 tdata2fix.py --input ../temp/conv/Conv_3_8_196_onnx_normal.bin --output ../temp/conv/tdata.txt


# running the simulation
cd $parent_dir
echo "running the Convolution simulation"
sbt 'testOnly Betsy.TopSim -- -t Convolution'
