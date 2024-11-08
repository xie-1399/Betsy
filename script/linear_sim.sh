#!/bin/bash

# Betsy follow the MiT Licence.(c) xxl, All rights reserved
# run the linear layer test in the Betsy
# Author xxl  Time:2024.6.5  Version:1.0

parent_dir=$(dirname "$(pwd)")
cd $parent_dir/model/layer
python3 Linear.py --exp 7 --man 8 --path ./temp/ --benchmark
rm -rf $parent_dir/temp
mv temp $parent_dir/

# then generate the instruction files from the linear layer
cd $parent_dir
sbt 'runMain tensil.tools.Generator normal Linear_64_256_10.onnx'

# convert it to the txt
cd script
python3 binaryhandler.py --input ../temp/Linear_64_256_10_onnx_normal.tprog --output ../temp/Linear_64_256_10_onnx_normal.txt --len 64
echo "convert the instruction"

# convert the tdata to the binary
# cd $parent_dir
# cp temp/Linear_64_256_10_onnx_normal.tdata temp/Linear_64_256_10_onnx_normal.bin
# cd script
# python tdata2fix.py


# running the simulation
cd $parent_dir
echo "running the linear simulation"
sbt 'testOnly Betsy.TopSim -- -t Linear'
