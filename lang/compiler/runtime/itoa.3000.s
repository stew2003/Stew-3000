; runtime_itoa converts an integer in register A, 
; to a string to be stored in a buffer pointed to by register B
;
; :: Implementation ::
; char* itoa (int num, char* str) {
;   int cur = 0;
;   int isNeg = 0;
;
;   if (num == 0) {
;       str[cur++] = '0';
;       str[cur] = '\0'; 
;       return str;
;   }
;
;   if (num <  0) {
;       isNeg = 1;
;       num = -num; 
;   }
;
;   while (num != 0) {
;       int digit = num % 10;
;       str[cur++] = 48 + digit;
;       num = num/base;
;   }
;
;   if (isNeg) {
;       str[cur++] = '-';   
;   }
;   str[cur] = 0;
;
;   int start = 0;
;   int end = --cur;
;
;   while (start < end) {
;       int temp = str[start];
;       str[start++] = str[end];
;       str[end--] = temp;
;   }
;
;   return str;
; }
runtime_itoa:
    sts b, 1;
    mvi 10, b;
    call runtime_divide;
    
