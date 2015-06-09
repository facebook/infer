/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#include "IvarExample.h"

int main() {
    IvarExample *i = [IvarExample alloc];
    int a = i.aProperty;
    int *x = malloc(sizeof(int));
    return 0;
}
