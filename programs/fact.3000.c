
/**
 * This program computes factorials for the range [0, 5], since
 * after that we would overflow. It does so iteratively
 * and recursively.
 */
void main() {
    int MAX_FACT = 5;
    int n = MAX_FACT;

    // iterative:
    while (n >= 0) {
        print(fact_iter(n));
        n--;
    }

    n = MAX_FACT;
    // recursive:
    while (n >= 0) {
        print(fact_rec(n));
        n--;
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
