// unused function
unsigned f(int x) {
    return (unsigned)x;
}

void main() {
    int count = 0;
    while (1) {
        print(count++);
    }
    char done[] = "done";
    print_lcd(done);
}