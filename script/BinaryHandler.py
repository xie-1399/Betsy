# Betsy follow the MiT Licence.(c) xxl, All rights reserved
# run all the unit test in the Betsy
# Author xxl  Time:2024.6.5  Version:1.0
# the file is used to convert the instruction binary to the txt format -> test for the simulation

import argparse


# read the binary file
def read_binary_file(input_file):
    with open(input_file, 'rb') as f:
        content = f.read()
    f.close()
    return content


# write the binary file with 0/1 format
def write_binary_as_01_strings(data, output_file, width):
    with open(output_file, 'w') as f:
        # Convert each byte to its binary representation and join into a string
        bin_string = ''.join(format(byte, '08b') for byte in data)
        # Split the string into chunks of 64 bits (8 bytes) and write each chunk to a new line
        for i in range(0, len(bin_string), width):
            binaryStr = bin_string[i:i + width]
            groups = [binaryStr[i:i + 8] for i in range(0, len(binaryStr), 8)]
            instruction = "".join(list(reversed(groups)))
            f.write(instruction + '\n')
        f.close()


# read the binary file and convert
def process_binary_file(input_file_path, output_file_path, width):
    binary_data = read_binary_file(input_file_path)
    write_binary_as_01_strings(binary_data, output_file_path, width)
    print(f"Process {input_file_path} File to {output_file_path} Successfully !")


# here is a useful example for convert file...


if __name__ == '__main__':
    # demo : python3 BinaryHandler.py --input XXX --output XXX --len XXX
    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str,help='Llama model to load')
    parser.add_argument('--output', type=str,help='evaluation dataset')
    parser.add_argument('--len', type=int,help='model sequence length')  #instruction length bits(default is 64)
    args = parser.parse_args()
    process_binary_file(args.input, args.output, args.len)