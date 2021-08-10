
void swap(int *px, int *py) {
    int tmp = *px;
    *px = *py;
    *py = tmp;
}

void main() {
    int x = 0;
    int y = 1;
    swap(&x, &y);
    print(x);
    print(y);
}