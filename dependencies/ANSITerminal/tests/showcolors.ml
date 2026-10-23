(* Draw a simple table showing ANSI colors and font effects.

   Run this in "xterm -fg gray60 -bg gray20".
*)

module T = ANSITerminal
open Printf

let colors =
  [T.Black; T.Red; T.Green; T.Yellow; T.Blue; T.Magenta; T.Cyan;
   T.White; T.Default]

let color_to_string = function
  | T.Black -> "black"
  | T.Red -> "red"
  | T.Green -> "green"
  | T.Yellow -> "yellow"
  | T.Blue -> "blue"
  | T.Magenta -> "magent"
  | T.Cyan -> "cyan"
  | T.White -> "white"
  | T.Default -> "def"

let () =
  (* Table *)
  let print_line fore =
    printf "%6s " (color_to_string fore);
    List.iter (fun back ->
                 T.print_string [T.Foreground fore; T.Background back; ]
                 " !Text! ";
              ) colors;
    print_string "\n" in

  T.erase T.Screen;
  T.set_cursor 1 1;
  print_string "        ";
  List.iter (fun back -> printf "%6s  " (color_to_string back)) colors;
  print_string "\n";
  List.iter print_line colors;
  (* Effects *)
  T.print_string [T.Bold] "Bold ";
  T.print_string [T.Underlined] "Underlined ";
  T.print_string [T.Blink] "Blink ";
  T.print_string [T.Inverse] "Inverse ";
  T.print_string [T.Hidden] "Hidden";
  print_string "<-- hidden\n"
