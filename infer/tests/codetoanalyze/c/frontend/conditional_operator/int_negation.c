/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int identity(int x) {
    return x;
}


int bar(int x) {
    if (identity(x)) {
        return 1;
    } else {
        return 0;
    }
}

int baz(int x) {

    if (identity(!x)) {
        return 1;
    } else {
        return 0;
    }
}

int neg(int x) {
    return !x;
}
