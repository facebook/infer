/*
 * Copyright (c) 2014 - Facebook.
 * All rights reserved.
 */

#import "BlockVar.m"

int main(int argc, const char * argv[])
{
        int res = [BlockVar navigateToURLInBackground:nil resolver:nil];
        NSLog(@"Hello, World! The result is %d" , res);
        if (res == 8) {
            return 1/0;
        }
    return 0;
}
