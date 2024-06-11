"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.8
"""


def read_binary_file(input_file):
    with open(input_file, 'rb') as f:
        content = f.read()
    f.close()
    return content


def write_binary_as_01_strings(data, output_file, width):
    with open(output_file, 'w') as f:
        # Convert each byte to its binary representation and join into a string
        bin_string = ''.join(format(byte, '08b') for byte in data)
        # Split the string into chunks of 64 bits (8 bytes) and write each chunk to a new line
        for i in range(0, len(bin_string), width):
            f.write(bin_string[i:i + width] + '\n')
        f.close()


def process_files(input_file_path, output_file_path, width):
    binary_data = read_binary_file(input_file_path)
    write_binary_as_01_strings(binary_data, output_file_path, width)
    print("Process File Successfully !")


def Instruction_statistics(input_file):
    with open(input_file, 'r') as file:
        lines = file.readlines()
        nop = 0
        matmul = 0
        data_move = 0
        load = 0
        simd = 0
        configure = 0
        print(f"Total Instruction number {len(lines)}")
        for line in lines:
            opcode = str(line)[-9] + str(line)[-8] + str(line)[-7] + str(line)[-6]
            if opcode == "0000":
                nop += 1
            elif opcode == "0001":
                matmul += 1
            elif opcode == "0010":
                data_move += 1
            elif opcode == "0011":
                load += 1
            elif opcode == "0100":
                simd += 1
            elif opcode == "1111":
                configure += 1
            else:
                print(line)
                print("No Define !!!")
        print(f"nop operation number: {nop}")
        print(f"matmul operation number: {matmul}")
        print(f"data_move operation number: {data_move}")
        print(f"load operation number: {load}")
        print(f"simd operation number: {simd}")
        print(f"configure operation number: {configure}")
        print(f"all operation number: {configure + nop + matmul + data_move + load + simd}")


if __name__ == '__main__':
    # input_file = '../model/resnet20v2_cifar_onnx_pynqz1.tprog'
    # output_file = './out.txt'
    # process_files(input_file, output_file, 64)
    Instruction_statistics("./out.txt")
