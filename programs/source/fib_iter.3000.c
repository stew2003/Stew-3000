/**
 * This program computes Fibonacci numbers, using iteration.
 * 
 * NOTE: run this with the decimal display in unsigned mode.
 */
void main() {
    unsigned n = 0;
    unsigned prev = 0;
    unsigned cur = 1;

    // prev will be <= cur until cur overflows
    while (prev <= cur) {
        print(cur);

        unsigned tmp = prev;
        prev = cur;
        cur = tmp + cur;
    }
}