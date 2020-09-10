/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <assert.h>
#include <functional>
#include <iostream>
#include <memory>
#include <vector>

namespace ATDWriter {

struct ATDWriterOptions {
  bool useYojson;
  bool prettifyJson;
};

// Symbols to be stacked
enum Symbol { SARRAY, STUPLE, SOBJECT, SVARIANT, STAG };

// whether the container has a {maximum,exact} size
enum ContainerSizeKind {
  CSKNONE, // no size info
  CSKEXACT, // the container expects exactly this number of items
  CSKMAX // the container expects at most this number of items
};

// Main class for writing ATD-like data
// - In NDEBUG mode this class is only a wrapper around an ATDEmitter
// - In DEBUG mode it acts as a validator: asserts will fire if the events do
// not correspond to a well-formed ATD/JSON value
template <class ATDEmitter>
class GenWriter {

 protected:
  ATDEmitter emitter_;

 private:
#ifdef DEBUG
  // State of the automaton
  std::vector<enum Symbol> stack_;

  // Objects want tagged values
  static bool needsTag(enum Symbol s) { return s == SOBJECT; }

  // How many elements are expected in the current container
  std::vector<int> containerSize_;
  std::vector<enum ContainerSizeKind> containerSizeKind_;
#endif

  void enterValue() {
#ifdef DEBUG
    if (stack_.empty()) {
      return;
    }
    assert(!needsTag(stack_.back()));
#endif
  }

  void leaveValue() {
#ifdef DEBUG
    switch (containerSizeKind_.back()) {
    case CSKEXACT:
    case CSKMAX:
      containerSize_.back() -= 1;
      break;
    case CSKNONE:
      break;
    }
    if (stack_.empty()) {
      return;
    }
    if (stack_.back() == STAG) {
      stack_.pop_back();
      assert(needsTag(stack_.back()));
    }
#endif
  }

  void emitValue() {
    enterValue();
    leaveValue();
  }

  void enterContainer(enum Symbol s,
                      enum ContainerSizeKind csk = CSKNONE,
                      int numElems = 0) {
#ifdef DEBUG
    enterValue();
    stack_.push_back(s);
    containerSizeKind_.push_back(csk);
    switch (csk) {
    case CSKEXACT:
    case CSKMAX:
      containerSize_.push_back(numElems);
      break;
    case CSKNONE:
      break;
    }
#endif
  }

  void leaveContainer(enum Symbol s) {
#ifdef DEBUG
    assert(stack_.back() == s);
    stack_.pop_back();
    switch (containerSizeKind_.back()) {
    case CSKEXACT:
      assert(containerSize_.back() == 0);
    case CSKMAX:
      assert(!containerSize_.empty());
      assert(containerSize_.back() >= 0);
      containerSize_.pop_back();
      break;
    case CSKNONE:
      break;
    }
    containerSizeKind_.pop_back();
    leaveValue();
#endif
  }

 public:
  GenWriter(ATDEmitter emitter) : emitter_(emitter) {
#ifdef DEBUG
    containerSizeKind_.push_back(CSKNONE);
#endif
  }

  ~GenWriter() {
#ifdef DEBUG
    assert(stack_.empty());
    assert(containerSizeKind_.size() == 1);
    assert(containerSizeKind_.back() == CSKNONE);
#endif
    emitter_.emitEOF();
  }

  void emitNull() {
    emitValue();
    emitter_.emitNull();
  }
  void emitBoolean(bool val) {
    emitValue();
    emitter_.emitBoolean(val);
  }
  void emitInteger(int64_t val) {
    emitValue();
    emitter_.emitInteger(val);
  }
  void emitFloat(float val) {
    emitValue();
    emitter_.emitFloat(val);
  }
  void emitString(const std::string &val) {
    emitValue();
    emitter_.emitString(val);
  }
  void emitTag(const std::string &val) {
#ifdef DEBUG
    assert(needsTag(stack_.back()));
    stack_.push_back(STAG);
#endif
    emitter_.emitTag(val);
  }

  void enterArray(int numElems) {
    enterContainer(SARRAY, CSKEXACT, numElems);
    emitter_.enterArray(numElems);
  }
  void enterArray() {
    enterContainer(SARRAY);
    emitter_.enterArray();
  }
  void leaveArray() {
    leaveContainer(SARRAY);
    emitter_.leaveArray();
  }
  void enterObject(int numElems) {
    enterContainer(SOBJECT, CSKMAX, numElems);
    emitter_.enterObject(numElems);
  }
  void enterObject() {
    enterContainer(SOBJECT);
    emitter_.enterObject();
  }
  void leaveObject() {
    leaveContainer(SOBJECT);
    emitter_.leaveObject();
  }
  void enterTuple(int numElems) {
    enterContainer(STUPLE, CSKEXACT, numElems);
    emitter_.enterTuple(numElems);
  }
  void enterTuple() {
    enterContainer(STUPLE);
    emitter_.enterTuple();
  }
  void leaveTuple() {
    leaveContainer(STUPLE);
    emitter_.leaveTuple();
  }

  void enterVariant(const std::string &tag, bool hasArg = true) {
    // variants have at most one value, so we can safely use hasArg
    // as the number of arguments
    enterContainer(SVARIANT, CSKEXACT, hasArg);
    emitter_.enterVariant();
    emitter_.emitVariantTag(tag, hasArg);
  }
  void leaveVariant() {
    leaveContainer(SVARIANT);
    emitter_.leaveVariant();
  }
  void emitSimpleVariant(const std::string &tag) {
    if (emitter_.shouldSimpleVariantsBeEmittedAsStrings) {
      emitString(tag);
    } else {
      enterVariant(tag, false);
      leaveVariant();
    }
  }

  // convenient methods

  void emitFlag(const std::string &tag, bool val) {
    if (val) {
      emitTag(tag);
      emitBoolean(true);
    }
  }

  // convenient classes for automatically closing containers using C++ scoping

  class ArrayScope {
    GenWriter &f_;

   public:
    ArrayScope(GenWriter &f, int size) : f_(f) { f_.enterArray(size); }
    ArrayScope(GenWriter &f) : f_(f) { f_.enterArray(); }
    ~ArrayScope() { f_.leaveArray(); }
  };

  class ObjectScope {
    GenWriter &f_;

   public:
    ObjectScope(GenWriter &f, int size) : f_(f) { f_.enterObject(size); }
    ObjectScope(GenWriter &f) : f_(f) { f_.enterObject(); }
    ~ObjectScope() { f_.leaveObject(); }
  };

  class TupleScope {
    GenWriter &f_;

   public:
    TupleScope(GenWriter &f, int size) : f_(f) { f_.enterTuple(size); }
    TupleScope(GenWriter &f) : f_(f) { f_.enterTuple(); }
    ~TupleScope() { f_.leaveTuple(); }
  };

  class VariantScope {
    GenWriter &f_;

   public:
    VariantScope(GenWriter &f, const std::string &tag) : f_(f) {
      f_.enterVariant(tag, true);
    }
    ~VariantScope() { f_.leaveVariant(); }
  };
};

// Configure GenWriter for Yojson / Json textual outputs
template <class OStream = std::ostream>
class JsonEmitter {

  const char *QUOTE = "\"";
  const char *COMMA = ",";
  const char *TAB = "  ";
  const char *NEWLINE = "\n";
  const char *COLON = ":";
  const char *COLONWITHSPACES = " : ";
  const char *COMMAWITHSPACES = " , ";
  const char *NULLSTR = "null";
  const char *FALSESTR = "false";
  const char *TRUESTR = "true";
  const char LBRACKET = '[';
  const char RBRACKET = ']';
  const char LBRACE = '{';
  const char RBRACE = '}';
  const char LPAREN = '(';
  const char RPAREN = ')';
  const char LANGLE = '<';
  const char RANGLE = '>';

 private:
  OStream &os_;
  const ATDWriterOptions options_;
  unsigned indentLevel_;
  bool nextElementNeedsNewLine_;
  bool previousElementNeedsComma_;
  bool previousElementIsVariantTag_;

 public:
  bool shouldSimpleVariantsBeEmittedAsStrings;

  JsonEmitter(OStream &os, const ATDWriterOptions opts)
      : os_(os),
        options_(opts),
        indentLevel_(0),
        nextElementNeedsNewLine_(false),
        previousElementNeedsComma_(false),
        previousElementIsVariantTag_(false),
        shouldSimpleVariantsBeEmittedAsStrings(!opts.useYojson) {}

  void tab() {
    if (previousElementIsVariantTag_) {
      if (options_.prettifyJson) {
        os_ << (options_.useYojson ? COLONWITHSPACES : COMMAWITHSPACES);
      } else {
        os_ << (options_.useYojson ? COLON : COMMA);
      }
    } else if (previousElementNeedsComma_) {
      os_ << COMMA;
    }
    if (nextElementNeedsNewLine_ && options_.prettifyJson) {
      os_ << NEWLINE;
      for (size_t i = 0; i < indentLevel_; i++) {
        os_ << TAB;
      }
    }
  }

 private:
  // TODO: unicode and other control chars
  void write_escaped(const std::string &val) {
    for (std::string::const_iterator i = val.begin(), e = val.end(); i != e;
         i++) {
      char x = *i;
      switch (x) {
      case '\\':
        os_ << "\\\\";
        break;
      case '"':
        os_ << "\\\"";
        break;
      case '\n':
        os_ << "\\n";
        break;
      case '\t':
        os_ << "\\t";
        break;
      case '\b':
        os_ << "\\b";
        break;
      case '\f':
        os_ << "\\f";
        break;
      case '\r':
        os_ << "\\r";
        break;
      default:
        os_ << x;
        break;
      }
    }
  }

  void enterContainer(char c) {
    tab();
    os_ << c;
    indentLevel_++;
    previousElementNeedsComma_ = false;
    nextElementNeedsNewLine_ = true;
    previousElementIsVariantTag_ = false;
  }

  void leaveContainer(char c) {
    indentLevel_--;
    // suppress the last comma or variant separator
    previousElementNeedsComma_ = false;
    previousElementIsVariantTag_ = false;
    tab();
    os_ << c;
    previousElementNeedsComma_ = true;
    nextElementNeedsNewLine_ = true;
  }

 public:
  void emitEOF() { os_ << NEWLINE; }

  void emitNull() {
    tab();
    os_ << NULLSTR;
    previousElementNeedsComma_ = true;
    nextElementNeedsNewLine_ = true;
    previousElementIsVariantTag_ = false;
  }
  void emitBoolean(bool val) {
    tab();
    os_ << (val ? TRUESTR : FALSESTR);
    previousElementNeedsComma_ = true;
    nextElementNeedsNewLine_ = true;
    previousElementIsVariantTag_ = false;
  }
  void emitInteger(int64_t val) {
    tab();
    os_ << val;
    previousElementNeedsComma_ = true;
    nextElementNeedsNewLine_ = true;
    previousElementIsVariantTag_ = false;
  }
  void emitString(const std::string &val) {
    tab();
    os_ << QUOTE;
    write_escaped(val);
    os_ << QUOTE;
    previousElementNeedsComma_ = true;
    nextElementNeedsNewLine_ = true;
    previousElementIsVariantTag_ = false;
  }
  void emitTag(const std::string &val) {
    tab();
    os_ << QUOTE;
    write_escaped(val);
    os_ << QUOTE;
    if (options_.prettifyJson) {
      os_ << COLONWITHSPACES;
    } else {
      os_ << COLON;
    }
    previousElementNeedsComma_ = false;
    nextElementNeedsNewLine_ = false;
    previousElementIsVariantTag_ = false;
  }
  void emitVariantTag(const std::string &val, bool hasArgs) {
    tab();
    os_ << QUOTE;
    write_escaped(val);
    os_ << QUOTE;
    previousElementNeedsComma_ = false;
    nextElementNeedsNewLine_ = false;
    previousElementIsVariantTag_ = true;
  }

  void enterArray() { enterContainer(LBRACKET); }
  void enterArray(int size) { enterArray(); }
  void leaveArray() { leaveContainer(RBRACKET); }
  void enterObject() { enterContainer(LBRACE); }
  void enterObject(int size) { enterObject(); }
  void leaveObject() { leaveContainer(RBRACE); }
  void enterTuple() { enterContainer(options_.useYojson ? LPAREN : LBRACKET); }
  void enterTuple(int size) { enterTuple(); }
  void leaveTuple() { leaveContainer(options_.useYojson ? RPAREN : RBRACKET); }
  void enterVariant() {
    enterContainer(options_.useYojson ? LANGLE : LBRACKET);
    // cancel indent
    indentLevel_--;
    nextElementNeedsNewLine_ = false;
  }
  void leaveVariant() {
    nextElementNeedsNewLine_ = false;
    leaveContainer(options_.useYojson ? RANGLE : RBRACKET);
    indentLevel_++;
  }
};

const uint8_t bool_tag = 0;
const uint8_t int8_tag = 1;
const uint8_t int16_tag = 2;
const uint8_t int32_tag = 3;
const uint8_t int64_tag = 4;
const uint8_t float64_tag = 12;
const uint8_t uvint_tag = 16;
const uint8_t svint_tag = 17;
const uint8_t string_tag = 18;
const uint8_t ARRAY_tag = 19;
const uint8_t TUPLE_tag = 20;
const uint8_t RECORD_tag = 21;
const uint8_t NUM_VARIANT_tag = 22;
const uint8_t VARIANT_tag = 23;
const uint8_t unit_tag = 24;
const uint8_t TABLE_tag = 25;
const uint8_t SHARED_tag = 26;

const int SIZE_NOT_NEEDED = -1;

// Configure GenWriter for Biniou binary output
template <class OStream = std::ostream>
class BiniouEmitter {

 private:
  OStream &os_;

  // Opened container, writing in progress.
  struct ATDContainer {
    uint8_t tag;
    int size;
    int count;

    ATDContainer(uint8_t tag, int size) : tag(tag), size(size), count(0) {}
  };

  // The full stack of opened containers
  std::vector<ATDContainer> atdContainers;

 public:
  const bool shouldSimpleVariantsBeEmittedAsStrings = false;

  BiniouEmitter(OStream &os) : os_(os) {}

 private:
  bool isValueTagNeeded() {
    if (!atdContainers.empty()) {
      const ATDContainer &obj = atdContainers.back();
      return obj.tag != ARRAY_tag || obj.count == 0;
    }
    return true;
  }

  void markWrite() {
    if (!atdContainers.empty()) {
      atdContainers.back().count++;
    }
  }

  void enterContainer(uint8_t tag, int size) {
    bool needTag = isValueTagNeeded();
    atdContainers.emplace_back(tag, size);
    writeValueTag(os_, needTag, tag);
    if (size != SIZE_NOT_NEEDED) {
      writeUvint(os_, size);
    }
  }

  void leaveContainer() {
    atdContainers.pop_back();
    markWrite();
  }

  // string hash algorithm from the biniou spec
  static uint32_t biniou_hash(const std::string &str) {
    uint32_t hash = 0;
    for (const char &c : str) {
      hash = 223 * hash + c;
    }
    hash %= 1 << 31;
    return hash;
  }

  static void write8(OStream &os, uint8_t c) { os.write((const char *)&c, 1); }

  static void write32(OStream &os, int32_t x) {
    write8(os, x >> 24);
    write8(os, x >> 16);
    write8(os, x >> 8);
    write8(os, x);
  }

  static void writeUvint(OStream &os, uint64_t x) {
    while (x > 127) {
      write8(os, x | 128);
      x >>= 7;
    }

    write8(os, (uint8_t)x);
  }

  static void writeSvint(OStream &os, int64_t x) {
    if (x >= 0) {
      uint64_t t = x;
      t = t * 2;
      writeUvint(os, t);
    } else {
      uint64_t t = -x;
      t = t * 2 - 1;
      writeUvint(os, t);
    }
  }

  static void writeValueTag(OStream &os, bool needTag, uint8_t tag) {
    if (needTag) {
      write8(os, tag);
    }
  }

  void emitDummyRecordField() {
    emitTag("!!DUMMY!!");
    markWrite();
    // unit is the smallest value (2 bytes)
    write8(os_, unit_tag);
    write8(os_, 0);
  }

 public:
  void emitEOF() {}

  void emitBoolean(bool val) {
    bool needTag = isValueTagNeeded();
    markWrite();
    writeValueTag(os_, needTag, bool_tag);
    write8(os_, val);
  }

  void emitInteger(int64_t val) {
    bool needTag = isValueTagNeeded();
    markWrite();
    writeValueTag(os_, needTag, svint_tag);
    writeSvint(os_, val);
  }

  void emitString(const std::string &val) {
    bool needTag = isValueTagNeeded();
    markWrite();
    writeValueTag(os_, needTag, string_tag);
    writeUvint(os_, val.length());
    for (const char &c : val) {
      write8(os_, c);
    }
  }

  void emitTag(const std::string &val) {
    int32_t hash = biniou_hash(val);
    // set first bit of hash
    hash |= 1 << 31;
    markWrite();
    write32(os_, hash);
  }

  void emitVariantTag(const std::string &val, bool hasArg) {
    int32_t hash = biniou_hash(val);
    // set first bit of hash if the variant has an argument
    if (hasArg) {
      hash |= 1 << 31;
    }
    markWrite();
    write32(os_, hash);
  }

  void enterArray(int size) { enterContainer(ARRAY_tag, size); }
  // unsupported:
  // void enterArray() { enterContainer(ARRAY_tag); }
  void leaveArray() { leaveContainer(); }
  void enterObject(int size) { enterContainer(RECORD_tag, size); }
  // unsupported:
  // void enterObject() { enterContainer(RECORD_tag); }
  void leaveObject() {
    const ATDContainer &obj = atdContainers.back();
    // Container's size was already written -> must fill in for missing
    // records.
    for (int i = obj.count / 2; i < obj.size; i++) {
      emitDummyRecordField();
    }
    leaveContainer();
  }
  void enterTuple(int size) { enterContainer(TUPLE_tag, size); }
  // unsupported:
  // void enterTuple() { enterContainer(TUPLE_tag); }
  void leaveTuple() { leaveContainer(); }
  void enterVariant() { enterContainer(VARIANT_tag, SIZE_NOT_NEEDED); }
  void leaveVariant() { leaveContainer(); }
};

// The full class for JSON and YOJSON writing
template <class OStream = std::ostream>
class JsonWriter : public GenWriter<JsonEmitter<OStream>> {
  typedef JsonEmitter<OStream> Emitter;

 public:
  JsonWriter(OStream &os, const ATDWriterOptions opts)
      : GenWriter<Emitter>(Emitter(os, opts)) {}
};

// The full class for biniou writing
template <class OStream>
class BiniouWriter : public GenWriter<BiniouEmitter<OStream>> {
  typedef BiniouEmitter<OStream> Emitter;

 public:
  BiniouWriter(OStream &os) : GenWriter<Emitter>(Emitter(os)) {}

  BiniouWriter(OStream &os, const ATDWriterOptions opts)
      : GenWriter<Emitter>(Emitter(os)) {}
};
} // namespace ATDWriter
