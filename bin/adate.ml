
(* This is a silly tool for printing out the date in the Coptic calender.

   Known bugs: it accepts invalid Gregorian dates
               it probably only works for 1901 -> 2099

   Author: Martin Keegan
   Licence: Apache 2.0
*)

module CopticCalendar : sig
  type t
  val of_date : CalendarLib.Date.t -> t
  val string_of : t -> string
end = struct
  type t = CalendarLib.Date.t

  let of_date d = 
    d

  let day_in_cycle cd =
    (CalendarLib.Date.to_mjd cd - 297) mod 1461

  let day_of cd =
    let dic = day_in_cycle cd in
      if dic = 0
      then 6
      else (((dic - 1) mod 365) mod 30 + 1)

  (* thout == 0; intercalary month == 12 *)                 
  let month_of cd =
    let dic = day_in_cycle cd in
      if dic = 0
      then 12
      else ((dic - 1) mod 365) / 30

  let month_name_of = function
    | 0 -> "Thout"
    | 1 -> "Paopi"
    | 2 -> "Hathor"
    | 3 -> "Koiak"
    | 4 -> "Tobi"
    | 5 -> "Meshir"
    | 6 -> "Paremhat"
    | 7 -> "Parmouti"
    | 8 -> "Pashons"
    | 9 -> "Paoni"
    | 10 -> "Epip"
    | 11 -> "Mesori"
    | 12 -> "Pi Kogi Enavot"
    | _  -> assert false

  let string_of cd = 
    let d = day_of cd |> string_of_int in
    let m = month_of cd |> month_name_of in
      d ^ " " ^ m
end

let usage () =
  print_endline "Usage: adate [YYYY-MM-DD]"

let version () =
  CalendarLib.Version.version
  |> print_endline

let handle d =
  let cd = CopticCalendar.of_date d in
    CopticCalendar.string_of cd |> print_endline

let specific date_str =
  CalendarLib.Printer.Date.from_fstring "%Y-%m-%d" date_str
  |> handle

let default () =
  CalendarLib.Date.today () |> handle

let main () =
  match Sys.argv with
    | [| _name ; "--cal-version" |] -> version ()
    | [| _name ; "--help" |]        -> usage ()
    | [| _name ; date_str |]        -> specific date_str
    | [| _name |]                   -> default ()
    | _                             -> usage ()

let () =
  main ()
