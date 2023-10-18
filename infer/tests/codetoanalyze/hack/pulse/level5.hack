// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

// The setup:
// * Dao (data access object) captures a context which can be later accessed by calling getContext.
// * If the captured context is SuperContext, getContext can leak elevated privilege context into
// the rest of the system which is dangerous.
//
// This test exercises inheritance, dynamic dispatch, static late binding. Later on we should split
// it into several files and add async/await.

class Context {}
class SuperContext extends Context {}

class DaoBase {
  private Context $context;

  public function __construct(Context $ctx) {
    $this->context = $ctx;
  }

  public function getContext(): Context {
    return $this->context;
  }
}

abstract class DaoQueries {
  public final static function queryA(Context $ctx): string {
    return static::query($ctx, "A");
  }

  public final static function queryB(Context $ctx): string {
    return static::query($ctx, "B");
  }

  protected abstract static function query(Context $ctx, string $query): string;
}

class Level5DaoQueries extends DaoQueries {
  <<__Override>>
  protected static function query(Context $ctx, string $query): string {
    return "level5";
  }
}

class Dao extends DaoBase {
  public function useContext(): string {
    $a = Level5DaoQueries::queryA($this->getContext());
    $b = Level5DaoQueries::queryB($this->getContext());
    return $a.$b;
  }
}

class Level5Base {
  protected static function getSuperContext(string $caller): Context {
    return new SuperContext();
  }

  protected static function getRegularContext(string $caller): Context {
    return new Context();
  }
}

final class Level5 extends Level5Base {
  /** Internall will use the Context attached to `$dao. */
  private static function useDaoContext(Dao $dao): void {
    $dao->useContext();
  }

  /** Return a Dao which has an attached SuperContext. */
  private static function loadUsingSuperContext(): Dao {
    $ctx = self::getSuperContext(__METHOD__);
    return new Dao($ctx);
  }

  /** Return a Dao which has an attached SuperContext. */
  private static function loadUsingRegularContext(): Dao {
    $ctx = self::getRegularContext(__METHOD__);
    return new Dao($ctx);
  }

  protected function runBad(): int {
    $tainted_dao = self::loadUsingSuperContext();
    self::useDaoContext($tainted_dao);
    return 0;
  }

  protected function runOk(): int {
    $untainted_dao = self::loadUsingRegularContext();
    self::useDaoContext($untainted_dao);
    return 0;
  }
}
