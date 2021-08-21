
#define MAX_FIB 13

/**
 * This program computes fibonacci numbers, using recursion.
 * 
 * NOTE: run this with the decimal display in unsigned mode.
 */
void main() {
    unsigned n = 0;

    // calculate each fibonacci number recursively
    while (n < MAX_FIB) {
        print((int)fib_rec(n++));
    }
}

/**
 * Calculates the nth fibonacci number, recursively.
 * (n is 0-indexed).
 */
unsigned fib_rec(unsigned n) {
    if (n == 0 || n == 1) {
        return 1;
    }
    return fib_rec(n - 1) + fib_rec(n - 2);
}
