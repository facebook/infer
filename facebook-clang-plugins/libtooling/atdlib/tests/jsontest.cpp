/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "../ATDWriter.h"

typedef ATDWriter::JsonWriter<std::ostream> JsonWriter;
typedef JsonWriter::ObjectScope ObjectScope;
typedef JsonWriter::ArrayScope ArrayScope;
typedef JsonWriter::VariantScope VariantScope;
typedef JsonWriter::TupleScope TupleScope;

int main(int argc, char **argv) {
  const struct ATDWriter::ATDWriterOptions jsonWriterOptions = {
      .useYojson = false,
      .prettifyJson = true,
  };
  const struct ATDWriter::ATDWriterOptions yojsonWriterOptions = {
      .useYojson = true,
      .prettifyJson = true,
  };

  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    OF.emitInteger(100000);
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    OF.emitInteger(-100000);
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    OF.emitString("Hello");
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    OF.emitBoolean(true);
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    ArrayScope Scope(OF, 3);
    OF.emitString("Hello");
    OF.emitBoolean(true);
    OF.emitInteger(100000);
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
    ObjectScope Scope(OF, 4); // 4 is larger than the actual size on purpose
    OF.emitTag("string");
    OF.emitString("Hello");
    OF.emitTag("boolean");
    OF.emitBoolean(true);
    OF.emitTag("integer");
    OF.emitInteger(100000);
  }
  {
    JsonWriter OF(std::cout, yojsonWriterOptions);
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
    JsonWriter OF(std::cout, jsonWriterOptions);
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
    JsonWriter OF(std::cout, yojsonWriterOptions);
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
