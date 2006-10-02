(*
 * Copyright (c) 2006  Dustin Sallings <dustin@spy.net>
 *
 * arch-tag: @UUIDGEN@
 *)

let main () =
;;

(* Start main unless we're interactive. *)
if !Sys.interactive then () else begin main() end
