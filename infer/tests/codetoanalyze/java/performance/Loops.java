/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

public class Loops{

  static int do_while_independent_of_p (int p) {
   int a = 0;
   do {
         if( p == 15) {
            p = p + 1;
         }
         a++;
   } while( a < 25 );

   return 0;
  }

  /* can't handle nested loops yet, but control vars of both loops must
     be {a, b} */
  static void nested_do_while_FP (int p) {
    int a = 10;
    int b = 0;
    do {
      do{
        if( p == 15) {
            p = p + 1;
         }
        b++;
      }while ( b < 10);
      a++;
    } while( a < 20 );
  }
}
