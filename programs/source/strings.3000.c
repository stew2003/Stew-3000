
/**
 * Prints the ASCII representation of a string to the decimal display.
 */
void print_string(char *s) {
    while (*s) {
        print((int)*s);
        s++;
    }
}

void main() {
    char str[] = "neat";
    print_string(str);
}