char* itoa (int num, char* str) {
   int cur = 0;
   int isNeg = 0;

   if (num < 0) {
       print(-1);
       isNeg = 1;
       print(isNeg);
   }

    print(isNeg);

   return str;
}

void main() {
    char buffer[5];
    itoa(-3, buffer);
    print_lcd(buffer);
}