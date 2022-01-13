(* Given a clock time in hh:mm format, determine, to the nearest degree, the angle between the hour and the minute hands. *)

let parse ts =
  match String.split_on_char ':' ts with
  | [hr; min] ->
      (int_of_string hr, int_of_string min)
  | _ ->
      failwith "unsupported format"

let solve ts =
  let hour, minute = parse ts in
  let hour_angle = (hour mod 12 * 30) + (minute / 2) in
  let minute_angle = minute * 6 in
  let diff = Int.abs (hour_angle - minute_angle) in
  if diff > 180 then 360 - diff else diff

let%test "12:30" = 165 = solve "12:30"

let%test "3:30" = 75 = solve "3:30"
