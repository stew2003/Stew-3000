
#define MAX_FIB 13

/**
 * This program computes fibonacci numbers, using iteration.
 * 
 * NOTE: run this with the decimal display in unsigned mode.
 */
void main() {
    // calculate each fibonacci number recursively
    unsigned n = 0;
    unsigned a = 0;
    unsigned b = 1;

    // now do it iteratively
    while (n++ < MAX_FIB) {
        print((int)b);
        
        unsigned tmp = a;
        a = b;
        b = tmp + b;
    }
}
