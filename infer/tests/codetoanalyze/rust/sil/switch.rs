fn get_value(t: Types) -> i32 {
        match t {
            Types::Unit                => 0,
            Types::Value(v)            => v,
            Types::Tuple(a, b)         => a + b,
            Types::Named { id }        => id,
        }
    }

    fn main() {
        let tunit  = Types::Unit;
        let tvalue = Types::Value(10);
        let ttype  = Types::Tuple(20, 30);
        let tname  = Types::Named { id: 40 };

        let _ = get_value(tunit);   // 0
        let _ = get_value(tvalue);  // 10
        let _ = get_value(ttype);   // 50
        let _ = get_value(tname);   // 40
    }

    enum Types {
      Unit,
      Value(i32),
      Tuple(i32,i32),
      Named{id:i32}
    }