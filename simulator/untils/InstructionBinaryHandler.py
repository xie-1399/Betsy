"""
    Betsy follow the MiT Licence.(c) xxl, All rights reserved
    Author : xxl    update time：2024.6.8
"""


# deal the instruction file
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
            match opcode:
                case "0000": nop += 1
                case "0001": matmul += 1
                case "0010": data_move += 1
                case "0011": load += 1
                case "0100": simd += 1
                case "1111": configure += 1
                case _: print("No Define !!!")
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
