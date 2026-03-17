fn main() {
      let tunit = Types::Unit;
      let tvalue = Types::Value(10);
      let ttype = Types::Tuple(20,30);
      let tname = Types::Named{id:40};
    }

    enum Types {
      Unit,
      Value(i32),
      Tuple(i32,i32),
      Named{id:i32}
    }