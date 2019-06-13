/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct A {
  virtual void foo();
  virtual void bar();
  void bazoo();
};

struct B : public A {
  void foo() override;
  void bar(int i);
  void bar() /* override */;
  void bazoo();
};

namespace Foo {

class SvIf {
 public:
  virtual ~SvIf() {}
  virtual void async_tm_poke() {}
  virtual void future_poke() {}
  virtual void semifuture_poke() {}
};

namespace Bar {

class SvIf : public Foo::SvIf {
 public:
  virtual ~SvIf() {}
  virtual void poke() {}
  virtual void async_tm_poke() override {}
  virtual void future_poke() override {}
  virtual void semifuture_poke() override {}
};

} // namespace Bar
} // namespace Foo

class ServiceBase {};

class FooBarService : public ServiceBase, public Foo::Bar::SvIf {
 public:
  explicit FooBarService() {}
  virtual void poke() override {}
  virtual void async_tm_poke() override {}
  virtual void future_poke() override {}
  virtual void semifuture_poke() override {}
};
