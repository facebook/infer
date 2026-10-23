
open Printf
module T = ANSITerminal

let () =
  printf "Testing ANSITerminal...\n%!";
  printf "In 1sec the screen will be cleared and cursor put at (1,1).%!";
  Unix.sleep 1;
  T.erase T.Screen;
  T.set_cursor 1 1;
  Unix.sleep 1;

  let x, y = T.size() in
  printf "The size of the terminal is (%i,%i).%!" x y;
  let x, y = T.pos_cursor() in
  printf "\nThe cursor position was (%i,%i).\n%!" x y;
  T.set_cursor 3 5;
  printf "*<--- set_cursor 3 5";
  T.set_cursor 1 8;
  printf "Press ENTER to temporarily move the cursor to (3,6)%!";
  ignore(read_line());
  T.save_cursor();
  T.set_cursor 3 6;
  printf "*<--- set_cursor 3 6";
  T.restore_cursor();

  for i = 5 downto 1 do
    printf "%i%!" i;
    Unix.sleep 1;
    T.move_bol();
  done
