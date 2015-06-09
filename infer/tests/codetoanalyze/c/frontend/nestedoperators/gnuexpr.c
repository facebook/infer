/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

int main() {
    int y = 3;

    y = ({int X = 4; X;});
    return 0;
}
