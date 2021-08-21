
#define MAX_FACT 5

/**
 * This program computes factorials for the range [0, 5], since
 * after that we would overflow. It does so iteratively
 * and recursively.
 */
void main() {
    int n = MAX_FACT;

    // recursive:
    while (n >= 0) {
        print(fact_rec(n--));
    }
}

/**
 * Computes n! with recursion.
 */
int fact_rec(int n) {
    assert(n >= 0);
    if (n == 0) {
        return 1;
    }

    return n * fact_rec(n - 1);
}
