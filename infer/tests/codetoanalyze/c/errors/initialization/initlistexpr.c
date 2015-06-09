/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */
int divide_by_zero() {
    int t[2][3][2] = {{{1,1},{2,2},{3,3}},{{4,4},{5,5},{1,0}}};
    return t[0][1][0]/t[1][2][1];
}
