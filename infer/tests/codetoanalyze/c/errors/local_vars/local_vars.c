/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/


int m(int z) {
    int y = 0;
    int x = 5;
    if (z < 10) {
        int x = 7;
        if (x == 7) return x/y;
    }
    if (x == 5)
        return x/y;
    else return 0;
}

int mm() {
    int y = 0;
    int x = 0;
    {
        int x = 5;
        if (x == 5) return x/y;
    }
    if (x == 0)
        return x/y;
    else return 0;
}

int t() {
    int y = 0;
    int x = 1;
    int z = 0;

    for (int x = 0; x < 10; x++) {
        int x = 9;
        if (x == 9) return x/y;
        else return 0;
    }
    return 0;
}
