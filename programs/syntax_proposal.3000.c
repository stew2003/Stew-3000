
/*
 * Multiline comments
 */
void main() {
  // Number literals
  10;

  // Binary operators
  2 + 5;
  10 - 4;
  5 * 7;
  6 / 2;
  10 % 2;
  2 & 4;  // bitwise and
  7 | 8;  // bitwise or
  10 ^ 2; // bitwise xor
  1 && 0; // logical and
  0 || 0; // logical or
  10 > 2;
  4 < 7;
  2 >= 3;
  1 <= -5;
  7 == 9;
  10 != 11;

  // Unary operators
  ~10; // bitwise not
  !1;  // logical not

  int a = 1;
  a++; // increment
  a--; // decrement

  // Function call
  fact(5);

  // Assignment
  a = 7;

  // Single-arm if
  if (a > 10) {}

  // If-else
  if (a == 1) {
    1;
  } else {
    0;
  }

  // Block (creates new scope)
  {
    int x = 7;
  }

  // While loops
  while (a > 0) {
    a--;
  }

  // Print (to decimal display)
  print(10);

  // Halts the program, outputs expression to dec display
  exit(0);
}

// Function definition
int fact(int n) {
  if (n == 0) {
    return 1;
  } else {
    return n * fact(n - 1);
  }
}