/*
 * Copyright (c) 2015 - Facebook.
 * All rights reserved.
 */

#include <stdlib.h>

struct delicious {
  int yum;
};

struct delicious *bake(struct delicious **cake) {
  int *zero = NULL;
  *zero = 3;
  return NULL;
}

struct delicious *skip_function_with_no_spec(void) {
  struct delicious *cake = NULL;
  int i;

  if(bake(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}

extern struct delicious *bakery(struct delicious **cake);

struct delicious *skip_external_function(void) {
  struct delicious *cake = NULL;
  int i;

  if(bakery(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}
