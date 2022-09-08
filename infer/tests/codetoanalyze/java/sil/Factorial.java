//
// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
//

class Factorial{

  public static void main(String args[]){
    int n=5;
    System.out.println("Factorial of "+n+" is: "+fact(n));
  }

  private static int fact(int n) {
    int i,fact = 1;
    for(i=1;i<=n;i++){
      fact=fact*i;
    };
    return fact;
  }
}
