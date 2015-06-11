#include <stdio.h>
#include <stdlib.h>

void crash_getc() {
    FILE *f;
    int i;
    f = fopen("this_file_doesnt_exists", "r");
    i = getc(f);
    printf("i =%i\n", i);
    fclose(f);
}

void nocrash_getc() {
    FILE *f;
    int i;
    f = fopen("this_file_doesnt_exists", "r");
    if (f) {
      i = getc(f);
      printf("i =%i\n", i);
      fclose(f);
    }
}

void crash_fgetc() {
    FILE *f;
    int i;
    f = fopen("this_file_doesnt_exists", "r");
    i = fgetc(f);
    printf("i =%i\n", i);
    fclose(f);
}

void nocrash_fgetc() {
    FILE *f;
    int i;
    f = fopen("this_file_doesnt_exists", "r");
    if (f) {
      i = fgetc(f);
      printf("i =%i\n", i);
      fclose(f);
    }
}


