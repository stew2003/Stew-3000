
#define A 5
#define B (A * A)
#define C (B + B + B)

void main() {
    while (C & (5 - 5) || A) {
        print(50);
    } 
    // else {
    //     print(100);
    // }
}