#include <stdio.h>
#include <stdlib.h>



void crashgetc() {

    
    FILE *f;
    int i;
    
    f=fopen("this_file_doesnt_exists", "r");
      i =getc(f);
      printf("i =%i\n", i);
    fclose(f);
}

void nocrashgetc() {
    
    
    FILE *f;
    int i;
    
    f=fopen("this_file_doesnt_exists", "r");
    
    
    if (f) {
      i =getc(f);
      printf("i =%i\n", i);
      fclose(f);
    }
}

void crashfgetc() {

    FILE *f;
    int i;
    
    f=fopen("this_file_doesnt_exists", "r");
    i =fgetc(f);
    printf("i =%i\n", i);
    fclose(f);
}

void nocrashfgetc() {

    FILE *f;
    int i;
    
    f=fopen("this_file_doesnt_exists", "r");
    if (f) {
      i =fgetc(f);
      printf("i =%i\n", i);
        fclose(f);
    }
}


