
import argparse

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
if __name__ == '__main__':
    # python3 tdata2fix.py --input XX --output XX

    parser = argparse.ArgumentParser()
    parser.add_argument('--input', type=str,help='inst file')
    parser.add_argument('--output', type=str,help='output file')
    args = parser.parse_args()

    fixed_point_numbers = read_fixed_point_numbers(args.input)
    write_to_text_file(fixed_point_numbers, args.output)
    print(f"Fixed-point numbers have been written to {args.output}")


