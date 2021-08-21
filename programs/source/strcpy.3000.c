void strcpy(char* dest, char* source) {
    while (*dest++ = *source++) {}
}


void main() {
    char source[] = "strnooo";
    char dest[8];

    strcpy(dest, source);
    print_lcd(dest);
}