(**************************************************************************)
(* Sample use of ParMap,  a simple library to perform Map computations on *)
(* a multi-core                                                           *)
(*                                                                        *)
(*  Author(s):  Marco Danelutto and Roberto Di Cosmo                      *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as               *)
(*  published by the Free Software Foundation, either version 2 of the    *)
(*  License, or (at your option) any later version.                       *)
(**************************************************************************)

(* for the toplevel

#use "topfind";;
#require "graphics";;
#require "parmap";;

*)

open Graphics;;

let n   = ref 1000;; (* the size of the square screen windows in pixels      *)
let res = ref 1000;; (* the resolution: maximum number of iterations allowed *)

(* scale factor and offset of the picture *)

let scale = ref 1.;;
let ofx = ref 0.;;
let ofy = ref 0.;;

(* convert an integer in the range 0..res into a screen color *)

let color_of c res = truncate
      (((float c)/.(float res))*.(float Graphics.white));;

(* compute the color of a pixel by iterating z_n+1=z_n^2+c *)
(* j,k are the pixel coordinates                           *)

let pixel (j,k,n) = 
  let zr = ref 0.0 in
  let zi = ref 0.0 in
  let cr = ref 0.0 in
  let ci = ref 0.0 in
  let zrs = ref 0.0 in
  let zis = ref 0.0 in
  let d   = ref (2.0 /. ((float  n) -. 1.0)) in
  let colour = Array.make n (Graphics.black) in

  for s = 0 to (n-1) do
    let j1 = ref (((float  j.(s)) +. !ofx) /. !scale) in
    let k1 = ref (((float  k) +. !ofy) /. !scale) in
    begin
      zr := !j1 *. !d -. 1.0;
      zi := !k1 *. !d -. 1.0;
      cr := !zr;
      ci := !zi;
      zrs := 0.0;
      zis := 0.0;
      for i=0 to (!res-1) do
	begin
	  if(not((!zrs +. !zis) > 4.0))
	  then 
	    begin
	      zrs := !zr *. !zr;
	      zis := !zi *. !zi;
	      zi  := 2.0 *. !zr *. !zi +. !ci;
	      zr  := !zrs -. !zis +. !cr;
	      Array.set colour s (color_of i !res);
	    end;
    	end
      done
    end
  done;
  (colour,k);;

(* draw a line on the screen using fast image functions *)

let show_a_result r =
  match r with
    (col,j) ->
      draw_image (make_image [| col |]) 0 j;;

(* generate the initial configuration *)

let initsegm n = 
  let rec aux acc = function 0 -> acc | n -> aux (n::acc) (n-1) in
  aux [] n
;;

let tasks = 
  let ini = Array.make !n 0 in
  let iniv = 
    for i=0 to (!n-1) do
      Array.set ini i i
    done; ini in
  List.map (fun seed -> (iniv,seed,!n)) (initsegm !n)
;;

let draw res = List.iter show_a_result res;;


(*** fork compute back-end *)

type signal = 
    Compute of int*float*float*float*int*int 
              (* resolution, ofx, ofy, scale, ncores, chunksize *)
  | Exit (* finished computation *)
;;

let cmdpipe_rd,cmdpipe_wr=Unix.pipe ();;
let respipe_rd,respipe_wr=Unix.pipe ();;

match Unix.fork() with  
  0 -> 
    begin (* compute back-end *)
      Unix.close cmdpipe_wr;
      Unix.close respipe_rd;
      let ic = Unix.in_channel_of_descr cmdpipe_rd in
      let oc = Unix.out_channel_of_descr respipe_wr in
      while true do 
	let msg = try Marshal.from_channel ic with _ -> exit 0 in 
	match msg with
	  Compute (res',ofx',ofy',scale',nc',cs') -> 
	    Printf.eprintf "Got task...\n%!";
	    res:=res';ofx:=ofx';ofy:=ofy';scale:=scale';
	    let m = Parmap.parmap ~ncores:nc' ~chunksize: cs' pixel (Parmap.L tasks)
	    in (Marshal.to_channel oc m []; flush oc)
	| Exit -> exit 0
      done
    end
| -1 ->  Printf.eprintf "fork error: pid %d" (Unix.getpid()); 
| pid -> ()
;;

(*** the main continues here *)

Unix.close cmdpipe_rd;;
Unix.close respipe_wr;;
let out,read = 
  let ic = Unix.in_channel_of_descr respipe_rd in 
  let oc = Unix.out_channel_of_descr cmdpipe_wr in
  (fun m -> Marshal.to_channel oc m []; flush oc),
  (fun () -> Marshal.from_channel ic)
;;

(* compute and draw *)

let compute () = 
  let _ = out (Compute (!res,!ofx,!ofy,!scale,4,1)) in
  read();;

let redraw () = 
  Printf.eprintf "Computing...%!";
  draw(compute()); 
  Printf.eprintf "done.\n%!";;

(* event loop for zooming into the picture *)

let rezoom x y w =
   let deltas = ((float !n)/.(float w)) in
   ofx := (!ofx +. (float x)) *. deltas;
   ofy := (!ofy +. (float y)) *. deltas;
   scale := !scale *. deltas;
   redraw();;
let reset () = scale:=1.; ofx:=0.; ofy:=0.;redraw();;
let refine () = res:=!res*2; redraw ();;
let unrefine () = res:=!res/2; redraw ();;
let zoom_in () = rezoom (!n/4) (!n/4) (!n/2);;
let zoom_out () = rezoom (-(!n/2)) (-(!n/2)) (!n*2);;
let dumpnum = ref 0;;
let dump () = 
  Printf.eprintf "Dumping image taken at ofx: %f ofy: %f scale: %f\n%!" !ofx !ofy !scale;
  let img = Graphics.dump_image (Graphics.get_image 0 0 !n !n) in
  let oc = open_out (Printf.sprintf "mandels.image.dump.%04d" !dumpnum) in
  Marshal.to_channel oc img []; dumpnum:=!dumpnum+1; close_out oc;;

(* encode state machine here *)

let rec init () =
  Printf.eprintf "Init...\n%!";	
  let s = wait_next_event [Button_down; Key_pressed] in
  if s.button then 
    track_rect s.mouse_x s.mouse_y None
  else if s.keypressed then
    match s.key with
      '+' -> let _ = zoom_in() in init ()
    | '-' -> let _ = zoom_out() in init ()
    | 'r' -> let _ = refine () in init ()
    | 'u' -> let _ = unrefine () in init ()
    | 'c' -> let _ = reset() in init ()
    | 'd' -> let _ = dump() in init ()
    | 'q' -> close_graph()
    | _ -> init ()

and track_rect x y oldimg =
  Printf.eprintf "Rect...\n%!";	
  let s = wait_next_event [Button_up; Mouse_motion] in
  let x'=s.mouse_x and y'=s.mouse_y in
  let bx,by,w,h = (min x x'), (min y y'), (abs (x'-x)), (abs (y'-y)) in
  if s.button then
    begin
      (* restore image if we are in the loop *)
      (match oldimg with 
	None -> ()
      | Some (i,x,y) -> draw_image i x y);
      if w>0 && h>0 then
	let i = get_image bx by w h in
	(* draw the border _inside_ the area *)
	draw_rect bx by (w-1) (h-1);
	track_rect x y (Some(i,bx,by))
      else
	track_rect x y oldimg
    end
  else
    (rezoom bx by (min w h); init())
;;

(*** Open the main graphics window and run the event loop *)

Graphics.set_window_title "Mandelbrot";;
Graphics.open_graph (" "^(string_of_int !n)^"x"^(string_of_int !n));;

redraw();;
init()

