import argparse
import sys
from graphviz import Digraph

def parse_circuit_file(filepath):
    """
    Parses a circuit definition file.

    Skips all lines until a blank line is found, then parses the rest
    of the file into a dictionary representing the circuit connections.

    Args:
        filepath (str): The path to the circuit definition file.

    Returns:
        dict: A dictionary where keys are output wires and values are
              the instructions to produce them.
    """
    circuit = {}
    start_parsing = False
    try:
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()

                if not start_parsing:
                    if not line: # Found the blank line
                        start_parsing = True
                    continue # Skip until we are ready to parse

                if not line: # Skip any other blank lines in the input
                    continue

                parts = line.split(' -> ')
                if len(parts) != 2:
                    print(f"Warning: Skipping malformed line: {line}")
                    continue

                output_wire = parts[1]
                instruction = parts[0].split(' ')

                # Logic for different instruction types
                if len(instruction) == 1: # Direct assignment: e.g., 123 -> x
                    circuit[output_wire] = {'op': 'ASSIGN', 'in1': instruction[0]}
                elif len(instruction) == 2: # Unary operator: e.g., NOT x -> y
                    circuit[output_wire] = {'op': instruction[0], 'in1': instruction[1]}
                elif len(instruction) == 3: # Binary operator: e.g., x AND y -> z
                    circuit[output_wire] = {'op': instruction[1], 'in1': instruction[0], 'in2': instruction[2]}
                else:
                    print(f"Warning: Skipping unrecognized instruction format: {line}")

    except FileNotFoundError:
        print(f"Error: Input file not found at '{filepath}'")
        sys.exit(1)

    return circuit

def visualize_circuit(circuit, output_filename):
    """
    Generates a visual representation of the parsed circuit using Graphviz.

    Args:
        circuit (dict): The parsed circuit dictionary.
        output_filename (str): The path for the output image file (without extension).
    """
    dot = Digraph(comment='Logic Circuit')
    dot.attr(rankdir='LR', splines='ortho', concentrate='true')
    dot.attr('node', shape='ellipse', style='filled', fillcolor='lightblue')
    dot.attr('edge', arrowhead='vee')

    # Keep track of all nodes to ensure they are all created
    all_nodes = set()

    # First pass: identify all wires and literals
    for out_wire, instr in circuit.items():
        all_nodes.add(out_wire)
        all_nodes.add(instr['in1'])
        if 'in2' in instr:
            all_nodes.add(instr['in2'])

    # Create all nodes with appropriate shapes
    for node_name in all_nodes:
        if node_name.isdigit():
            # Literals are constants
            dot.node(node_name, shape='plaintext', fontsize='12')
        else:
            # Wires
            dot.node(node_name, node_name)


    # Second pass: create operator nodes and edges
    for output_wire, instruction in circuit.items():
        op = instruction['op']

        if op == 'ASSIGN':
            dot.edge(instruction['in1'], output_wire)
        else:
            # Create a unique node for the operation itself
            op_node_id = f"op_{output_wire}"
            dot.node(op_node_id, label=op, shape='diamond', style='filled', fillcolor='lightcoral')

            # Edges from inputs to the operation
            dot.edge(instruction['in1'], op_node_id)
            if 'in2' in instruction:
                dot.edge(instruction['in2'], op_node_id)

            # Edge from the operation to the output wire
            dot.edge(op_node_id, output_wire)

    print(f"[*] Rendering graph to '{output_filename}.png'...")
    try:
        dot.render(output_filename, format='png', cleanup=True)
        print("[+] Visualization complete!")
    except Exception as e:
        print(f"\n--- Graphviz Error ---")
        print(f"Could not render the graph. Please ensure Graphviz is installed")
        print(f"and that its 'bin' directory is in your system's PATH.")
        print(f"Original error: {e}")
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(description="Parse a logic circuit file and visualize it.")
    parser.add_argument("input_file", help="The path to the circuit definition file.")
    args = parser.parse_args()

    # Determine output filename from input
    output_filename = args.input_file.rsplit('.', 1)[0]

    print(f"[*] Parsing circuit from '{args.input_file}'...")
    circuit_data = parse_circuit_file(args.input_file)
    
    if not circuit_data:
        print("[!] No valid circuit instructions found after the initial blank line. Exiting.")
        return

    print(f"[+] Parsed {len(circuit_data)} instructions.")
    visualize_circuit(circuit_data, output_filename)


if __name__ == "__main__":
    main()
