def read_fixed_point_numbers(filename):
    fixed_point_values = []
    with open(filename, 'rb') as file:
        while True:
            data = file.read(2)  # Read 2 bytes (16 bits) at a time
            if not data:
                break

            # Splice the data: data0 is the first byte, data1 is the second
            data0 = data[0]  # First byte (lower 8 bits)
            data1 = data[1]  # Second byte (upper 8 bits)

            # Combine into a 16-bit integer
            combined_value = (data1 << 8) | (data0 & 0xFF)  # Shift data1 to upper byte and combine with data0

            # Determine if the combined value is negative
            if combined_value > 32767:  # 2^15 - 1
                combined_value -= 65536  # Convert to negative using two's complement

            # Convert to fixed-point (Q8.8)
            integer_part = combined_value >> 8  # Get the integer part (upper 8 bits)
            fractional_part = combined_value & 0xFF  # Get the fractional part (lower 8 bits)
            fractional_value = fractional_part / 256.0

            # Combine the integer and fractional parts
            fixed_point_value = integer_part + fractional_value
            fixed_point_values.append(fixed_point_value)

    return fixed_point_values

def write_to_text_file(fixed_point_values, output_filename):
    with open(output_filename, 'w') as file:
        for value in fixed_point_values:
            file.write(f"{value}\n")

# Example usage

input_filename = 'Linear_64_256_10_onnx_normal.bin'  # Replace with your file path
output_filename = 'fixed_point_numbers.txt'  # Output text file

fixed_point_numbers = read_fixed_point_numbers(input_filename)
write_to_text_file(fixed_point_numbers, output_filename)

print(f"Fixed-point numbers have been written to {output_filename}")
