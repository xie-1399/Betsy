def read_fixed_point_numbers(filename):
    with open(filename, 'r') as file:
        # Read all lines as float values
        numbers = [float(line.strip()) for line in file]
    return numbers

def process_and_sum_first_of_64_blocks(numbers, block_size=256):
    # Collect the first number from each of the first 64 blocks
    # first_numbers = [(numbers[i]) for i in range(0, len(numbers), block_size)][:64]

    # Multiply each first number by its corresponding index and sum all results
    # total_sum = sum(first_numbers[i] * (i + 1) for i in range(64))
    total_sum = sum(numbers[i] * i for i in range(64))


    # Print results for clarity
    # for i, num in enumerate(first_numbers):
        # print(f"First number of block {i + 1}: {num}, Multiplied by {i + 1}: {num * (i + 1)}")

    print(f"Total sum of all multiplied values: {total_sum}")
    return total_sum

def write_total_sum_to_file(total_sum, output_filename):
    with open(output_filename, 'w') as file:
        file.write(f"Total sum of multiplied values from 64 blocks: {total_sum}\n")

# File paths
input_filename = 'fixed_point_numbers.txt'  # Input file with fixed-point numbers
output_filename = 'total_sum.txt'  # Output file to store the total sum

# Process, multiply, and sum
numbers = read_fixed_point_numbers(input_filename)
total_sum = process_and_sum_first_of_64_blocks(numbers)
write_total_sum_to_file(total_sum, output_filename)

print(f"Total sum has been written to {output_filename}")
