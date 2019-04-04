# Polynomial Simplifier (Bonus 3)

## Description

Given a rational polynomial in multiple variables, the output is a fully expanded and simplified polynomial in the following form:

p(x)/q(x) = (a_nx^n + ... + a_2x^2 + a_1x + a_0)/(a_mx^m + ... + a_2x^2 + a_1x + a_0)

## Testing

To simplify the testing process we provided a bash script `run_all.sh` that automatically runs `make tests` and then compares the output files byte by byte with the corresponding expected output. The expected output files will conventionally have `.expect` extension and the same base name as the input file. If a test fails, the script will display `git diff` over the two files.

## Known issues

- Please beware that different versions of OCaml and the standard library provide dissimilar versions of `List.stable_sort`. If such error occurs on compilation, please have a look at the indicated line (likely to be 131 and 141) and invert the order of the arguments (ie, `List.stable_sort l compareDeg` turns to `List.stable_sort compareDeg l`). Stable sorting was required to prevent reordering from creating an infinite loop by always changing the polynomial on the simplification step.

- Since we are using only integers, the polynomial equality check fails in some cases due to division precision. It was disabled and the equalities were confirmed manually for all test cases. However, the current implementation, aside from those edge cases, is capable of handling undefined values and more.

- If in the process of simplification, the program detects a division by zero, **will throw an exception**.

## Ocaml version
4.05.0

##### Authors:
**Nimit Patel and David Cabrera**
