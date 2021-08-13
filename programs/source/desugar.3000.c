
void main() {
    int i = 0;
    // char s[3];
    // s[0]; // ok
    // s[i]; // should break
    ((int **)3 + 4)[i];
}