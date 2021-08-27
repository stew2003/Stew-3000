char* itoa (int num, char* str) {
   int cur = 0;
   int isNeg = 0;

   if (num == 0) {
       str[cur++] = '0';
       str[cur] = 0; 
       return str;
   }

   if (num < 0) {
       isNeg = 1;
       num = num * -1; 
   }

   while (num != 0) {
       int digit = num % 10;
       str[cur++] = (char) (48 + digit);
       num = num / 10;
   }

   if (isNeg) {
       str[cur++] = '-';   
   }

   str[cur] = 0;

   int start = 0;
   int end = cur - 1;

   while (start < end) {
       char temp = str[start];
       str[start++] = str[end];
       str[end--] = temp;
   }

   return str;
}

void main() {
    char buffer[5];
    itoa(-120, buffer);
    print_lcd(buffer);
}