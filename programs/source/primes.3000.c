
/**
 * Prints out the primes that are <= 127. 
 * 
 * These are the primes we expect to see: (courtesy of https://oeis.org/A000040)
 *  2, 3, 5, 7, 11, 13, 17, 19, 
 *  23, 29, 31, 37, 41, 43, 47, 
 *  53, 59, 61, 67, 71, 73, 79, 
 *  83, 89, 97, 101, 103, 107, 
 *  109, 113, 127
 */
void main() {
    int n = 2;

    while (n > 0) {
        if (is_prime(n)) {
            print(n);
        }
        n++;
    }
}

/**
 * Checks that a given number is prime by attempting to
 * divide it by every number in the range [2, n-1].
 */
int is_prime(int n) {
    assert(n >= 0);

    int k = n / 2;

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
