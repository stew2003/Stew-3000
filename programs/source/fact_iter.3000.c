
#define MAX_FACT 5

/**
 * This program computes factorials for the range [0, 5], since
 * after that we would overflow. It does so iteratively
 * and recursively.
 */
void main() {
    int n = MAX_FACT;

    // iterative:
    while (n >= 0) {
        print(fact_iter(n--));
    }
}

/**
 * Computes n! iteratively.
 */
int fact_iter(int n) {
    assert(n >= 0);
    int product = 1;

    while (n > 0) {
        product = product * n;
        n--;
    }

    return product;
}
