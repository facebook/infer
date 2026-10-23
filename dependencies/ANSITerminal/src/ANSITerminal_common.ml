(* Copyright 2004 by Troestler Christophe
   Christophe.Troestler(at)umons.ac.be

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

let autoreset = ref true

let set_autoreset b = autoreset := b


type color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

type style =
  | Reset | Bold | Underlined | Blink | Inverse | Hidden
  | Foreground of color
  | Background of color

let black = Foreground Black
let red = Foreground Red
let green = Foreground Green
let yellow = Foreground Yellow
let blue = Foreground Blue
let magenta = Foreground Magenta
let cyan = Foreground Cyan
let white = Foreground White
let default = Foreground Default

let on_black = Background Black
let on_red = Background Red
let on_green = Background Green
let on_yellow = Background Yellow
let on_blue = Background Blue
let on_magenta = Background Magenta
let on_cyan = Background Cyan
let on_white = Background White
let on_default = Background Default


type loc = Eol | Above | Below | Screen
