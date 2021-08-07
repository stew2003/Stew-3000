
/**
 * Finds the prime factorization of a given number, printing
 * each factor to the decimal display.
 */
void main() {
    // this is the number whose prime factors will be found.
    int to_factorize = 118;

    while (to_factorize != 1) {
        int factor = find_single_prime_factor(to_factorize);
        print(factor);
        to_factorize = to_factorize / factor;
    }
}

/**
 * Finds a single prime factor of a number n, by searching the
 * range [2, n] for numbers that are prime and divide n.
 */
int find_single_prime_factor(int n) {
    int k = 2;
    while (k < n) {
        if (is_prime(k) && n % k == 0) {
            return k;
        }
        k++;
    }
    // nothing divided n, n is prime
    return n;
}

/**
 * Checks that a given number is prime by attempting to
 * divide it by every number in the range [2, n-1].
 */
int is_prime(int n) {
    int k = n - 1;

    while (k > 1) {
        // k divides n, so n is not prime
        if (n % k == 0) {
            return 0;
        }
        k--;
    }

    // nothing divided n, so it is prime
    return 1;
}
