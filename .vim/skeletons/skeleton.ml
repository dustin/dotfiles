(*
 * Copyright (c) 2007  Dustin Sallings <dustin@spy.net>
 *)

let main () =
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
