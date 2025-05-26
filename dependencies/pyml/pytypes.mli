type pyobject

type compare = LT | LE | EQ | NE | GT | GE

type input = Single | File | Eval

val int_of_compare: compare -> int

val compare_of_int: int -> compare

val input_of_int: int -> input

type 'a file = Filename of string | Channel of 'a

val file_map: ('a -> 'b) -> 'a file -> 'b file
