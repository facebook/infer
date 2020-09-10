/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "../ATDWriter.h"

typedef ATDWriter::BiniouWriter<std::ostream> BiniouWriter;
typedef BiniouWriter::ObjectScope ObjectScope;
typedef BiniouWriter::ArrayScope ArrayScope;
typedef BiniouWriter::VariantScope VariantScope;
typedef BiniouWriter::TupleScope TupleScope;

int main(int argc, char **argv) {
  {
    BiniouWriter OF(std::cout);
    OF.emitInteger(-100000);
  }
  {
    BiniouWriter OF(std::cout);
    int64_t min_ocaml = -4611686018427387904;
    OF.emitInteger(min_ocaml);
  }
  {
    BiniouWriter OF(std::cout);
    int64_t max_ocaml = 4611686018427387903;
    OF.emitInteger(max_ocaml);
  }
  {
    BiniouWriter OF(std::cout);
    OF.emitString("Hello");
  }
  {
    BiniouWriter OF(std::cout);
    OF.emitBoolean(true);
  }
  {
    BiniouWriter OF(std::cout);
    ArrayScope Scope(OF, 0);
  }
  {
    BiniouWriter OF(std::cout);
    ArrayScope Scope(OF, 3);
    OF.emitString("Hello, how are you?");
    OF.emitString("I'm well, thank you; and you, how are you?");
    OF.emitString("I'm fine, thank you.");
  }
  {
    BiniouWriter OF(std::cout);
    ArrayScope Scope(OF, 3);
    {
      ArrayScope Scope(OF, 1);
      {
        ArrayScope Scope(OF, 1);
        OF.emitInteger(1);
      }
    }
    {
      ArrayScope Scope(OF, 2);
      { ArrayScope Scope(OF, 0); }
      {
        ArrayScope Scope(OF, 2);
        OF.emitInteger(2);
        OF.emitInteger(3);
      }
    }
    { ArrayScope Scope(OF, 0); }
  }
  {
    BiniouWriter OF(std::cout);
    ObjectScope Scope(OF, 12); // 12 is larger than the actual size on purpose
    OF.emitTag("string");
    OF.emitString("Hello");
    OF.emitTag("boolean");
    OF.emitBoolean(true);
    OF.emitTag("integer");
    OF.emitInteger(100000);
  }
  {
    BiniouWriter OF(std::cout);
    ObjectScope Scope(OF, 2);
    OF.emitTag("integer");
    OF.emitInteger(100000);
    OF.emitTag("array");
    {
      ArrayScope Scope(OF, 2);
      OF.emitInteger(1);
      OF.emitInteger(2);
    }
  }
  {
    BiniouWriter OF(std::cout);
    ObjectScope Scope(OF, 2);
    OF.emitTag("string");
    OF.emitString("multiply");
    OF.emitTag("array");
    {
      ArrayScope Scope(OF, 2);
      {
        ObjectScope Scope(OF, 1);
        OF.emitTag("integer");
        OF.emitInteger(32);
      }
      {
        ObjectScope Scope(OF, 2);
        OF.emitTag("integer");
        OF.emitInteger(52);
      }
    }
  }
  {
    BiniouWriter OF(std::cout);
    TupleScope Scope(OF, 2);
    OF.emitSimpleVariant("zero");
    {
      VariantScope Scope(OF, "succ");
      {
        VariantScope Scope(OF, "pred");
        OF.emitSimpleVariant("zero");
      }
    }
  }
  {
    BiniouWriter OF(std::cout);
    TupleScope Scope(OF, 2);
    OF.emitSimpleVariant("zero");
    {
      VariantScope Scope(OF, "succ");
      {
        VariantScope Scope(OF, "pred");
        {
          VariantScope Scope(OF, "eval");
          {
            TupleScope Scope(OF, 2);
            OF.emitString("f");
            OF.emitString("\"3\t4\n\"");
          }
        }
      }
    }
  }

  return 0;
}
