/**
 * This program computes Fibonacci numbers, using iteration.
 * 
 * NOTE: run this with the decimal display in unsigned mode.
 */
void main() {
    unsigned n = 0;
    unsigned a = 0;
    unsigned b = 1;

    // a will be <= b until b overflows
    while (a <= b) {
        print(b);

        unsigned tmp = a;
        a = b;
        b = tmp + b;
    }
}
