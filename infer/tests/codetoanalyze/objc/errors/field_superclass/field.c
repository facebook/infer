/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#include "B.h"


int main() {
    B *b = [B alloc];
    b->x = 5;
    b->a = b; // create cycle --> leak
    return 0;
}
