/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main() {
    int x = 0;
    while(1) {
        while(2) {
            x += 1;
            if (x > 5) {
                break;
            }
        }
        if (x == 2) {
            continue;
        }
    }
    return 0;
}
