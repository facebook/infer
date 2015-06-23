/*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 */
struct l2 {
    int b;
    struct l2 *a;
};

int add2(struct l2 *l) {
    int r = 0;
    for (; l; l = l->a) {
        r += l->b;
    }
    return r;
}

/* Divide by zero error shows that we get a spec for add2 */
int main() {
    int res = add2(0);
    return 5/res;
}
