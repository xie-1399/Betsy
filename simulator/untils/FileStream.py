
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
            f.write(bin_string[i:i + width] + '\n')
        f.close()


# read the binary file and convert
def process_binary_file(input_file_path, output_file_path, width):
    binary_data = read_binary_file(input_file_path)
    write_binary_as_01_strings(binary_data, output_file_path, width)
    print(f"Process {input_file_path} File to {output_file_path} Successfully !")
