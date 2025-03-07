# PL-Project2 (Binary Logic and Arithmetic Operator)

**Name:** Fernando Martinez

**Prog. Language:** OCaml

## Description
- 8-bit signed arithmetic operations (-128 to 127)
- 8-bit unsigned logical operations (0x00 to 0xFF)
- Interactive command-line interface
- Decimal ↔ Two's complement binary conversions
- Hexadecimal ↔ Binary conversions (for fun, it works with or without prefix)

## Getting Started

### Compilation
```bash
ocamlopt -o BinaryLogicANDArithmeticOperator BinaryLogicANDArithmeticOperator.ml
```

or 

```bash
ocamlc -o BinaryLogicANDArithmeticOperator BinaryLogicANDArithmeticOperator.ml
```

### Execution
```bash
./BinaryLogicANDArithmeticOperator
```

or 

### Execution
```bash
ocaml BinaryLogicANDArithmeticOperator.ml
```

## Testing
All processed values, as well as the arithmetic and hex results, were verified using:
[Binary/Hex Converter Tool](https://www.binaryconvert.com/convert_signed_char.html)

## Screenshots (Outputs using both slide examples and assignment's operations)
Please check **results.pdf** and the **media** directory.


## Implementation Details
- Functional programming paradigm
- Tail recursion for iterative processes
- Strict 8-bit boundary enforcement
- Separate handling of signed/unsigned interpretations
