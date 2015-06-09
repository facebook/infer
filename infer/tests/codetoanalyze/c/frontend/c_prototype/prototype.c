/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

/*
 A simple example of a function prototype allowing the function
 to be used before it is defined.
 */

int sum (int, int);

int main (void) {
    int total;

    total = sum (2, 3);

    return 0;
}

int sum (int a, int b) {
    return a + b;
}
