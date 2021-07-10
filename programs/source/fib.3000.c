
#define MAX_FIB 11

/**
 * This program computes fibonacci numbers, using both recursive
 * and iterative approaches.
 */
void main() {
    int n = 0;

    // calculate each fibonacci number recursively
    while (n < MAX_FIB) {
        print(fib_rec(n));
        n++;
    }

    int a = 0;
    int b = 1;
    n = 0;

    // now do it iteratively
    while (n < MAX_FIB) {
        print(b);
        
        int tmp = a;
        a = b;
        b = tmp + b;

        n++;
    }
}

/**
 * Calculates the nth fibonacci number, recursively.
 * (n is 0-indexed).
 */
int fib_rec(int n) {
    assert(n >= 0);
    if (n == 0 || n == 1) {
        return 1;
    }
    return fib_rec(n - 1) + fib_rec(n - 2);
}
