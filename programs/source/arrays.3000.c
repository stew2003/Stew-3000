
#define MAGIC 0xa
#define ARRAY_SIZE (50 + MAGIC)

void main() {
    int arr[5] = {
        1,
        2,
        3,
        4,
        5
    };

    unsigned my_array[ARRAY_SIZE * (5 - 5)];
}