open Astring
open Sexplib0.Sexp_conv

type win10_release =
  [ `V1507
  | `Ltsc2015
  | `V1511
  | `V1607
  | `Ltsc2016
  | `V1703
  | `V1709
  | `V1803
  | `V1809
  | `Ltsc2019
  | `V1903
  | `V1909
  | `V2004
  | `V20H2
  | `V21H1
  | `Ltsc2022 ]
[@@deriving sexp]

let win10_releases =
  [
    `V1507;
    `Ltsc2015;
    `V1511;
    `V1607;
    `Ltsc2016;
    `V1703;
    `V1709;
    `V1809;
    `Ltsc2019;
    `V1903;
    `V1909;
    `V2004;
    `V20H2;
    `V21H1;
    `Ltsc2022;
  ]

type win10_lcu =
  [ `LCU
  | `LCU20220111
  | `LCU20211214
  | `LCU20211109
  | `LCU20211012
  | `LCU20210914
  | `LCU20210810
  | `LCU20210713
  | `LCU20210608 ]
[@@deriving sexp]

let win10_current_lcu = `LCU20220111

type win10_revision = win10_release * win10_lcu option [@@deriving sexp]

let win10_lcus : ('a * int * win10_release list) list =
  [
    (`LCU20220111, 5009555, [ `Ltsc2022 ]);
    (`LCU20220111, 5009543, [ `V20H2; `V21H1 ]);
    (`LCU20220111, 5009545, [ `V1909 ]);
    (`LCU20220111, 5009557, [ `V1809; `Ltsc2019 ]);
    (`LCU20220111, 5009546, [ `V1607; `Ltsc2016 ]);
    (`LCU20220111, 5009585, [ `V1507; `Ltsc2015 ]);
    (`LCU20211214, 5008223, [ `Ltsc2022 ]);
    (`LCU20211214, 5008212, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20211214, 5008206, [ `V1909 ]);
    (`LCU20211214, 5008218, [ `V1809; `Ltsc2019 ]);
    (`LCU20211214, 5008207, [ `V1607; `Ltsc2016 ]);
    (`LCU20211214, 5008230, [ `V1507; `Ltsc2015 ]);
    (`LCU20211109, 5007205, [ `Ltsc2022 ]);
    (`LCU20211109, 5007186, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20211109, 5007189, [ `V1909 ]);
    (`LCU20211109, 5007206, [ `V1809; `Ltsc2019 ]);
    (`LCU20211109, 5007192, [ `V1607; `Ltsc2016 ]);
    (`LCU20211109, 5007207, [ `V1507; `Ltsc2015 ]);
    (`LCU20211012, 5006699, [ `Ltsc2022 ]);
    (`LCU20211012, 5006670, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20211012, 5006667, [ `V1909 ]);
    (`LCU20211012, 5006672, [ `V1809; `Ltsc2019 ]);
    (`LCU20211012, 5006669, [ `V1607; `Ltsc2016 ]);
    (`LCU20211012, 5006675, [ `V1507; `Ltsc2015 ]);
    (`LCU20210914, 5005575, [ `Ltsc2022 ]);
    (`LCU20210914, 5005565, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20210914, 5005566, [ `V1909 ]);
    (`LCU20210914, 5005568, [ `V1809; `Ltsc2019 ]);
    (`LCU20210914, 5005573, [ `V1607; `Ltsc2016 ]);
    (`LCU20210914, 5005569, [ `V1507; `Ltsc2015 ]);
    (`LCU20210810, 5005039, [ `Ltsc2022 ]);
    (`LCU20210810, 5005033, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20210810, 5005031, [ `V1909 ]);
    (`LCU20210810, 5005030, [ `V1809; `Ltsc2019 ]);
    (`LCU20210810, 5005043, [ `V1607; `Ltsc2016 ]);
    (`LCU20210810, 5005040, [ `V1507; `Ltsc2015 ]);
    (`LCU20210713, 5004237, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20210713, 5004245, [ `V1909 ]);
    (`LCU20210713, 5004244, [ `V1809; `Ltsc2019 ]);
    (`LCU20210713, 5004238, [ `V1607; `Ltsc2016 ]);
    (`LCU20210713, 5004249, [ `V1507; `Ltsc2015 ]);
    (`LCU20210608, 5003637, [ `V2004; `V20H2; `V21H1 ]);
    (`LCU20210608, 5003635, [ `V1909 ]);
    (`LCU20210608, 5003646, [ `V1809; `Ltsc2019 ]);
    (`LCU20210608, 5003638, [ `V1607; `Ltsc2016 ]);
    (`LCU20210608, 5003687, [ `V1507; `Ltsc2015 ]);
  ]

let win10_lcu_to_kb : (win10_lcu * win10_release, int option) Hashtbl.t =
  let t = Hashtbl.create 63 in
  let f (lcu, kb, vs) =
    let g v =
      if lcu = win10_current_lcu then Hashtbl.add t (`LCU, v) (Some kb);
      Hashtbl.add t (lcu, v) (Some kb)
    in
    List.iter g vs
  in
  List.iter f win10_lcus;
  t

let win10_kb_to_lcu =
  let t = Hashtbl.create 63 in
  let f (lcu, kb, vs) =
    List.iter (fun v -> Hashtbl.add t (kb, v) (Some lcu)) vs
  in
  List.iter f win10_lcus;
  t

let win10_lcu_kb_number v lcu =
  try Hashtbl.find win10_lcu_to_kb (lcu, v) with Not_found -> None

let win10_kb_number_to_lcu (v : win10_release) kb =
  match Hashtbl.find win10_kb_to_lcu (kb, v) with
  | lcu -> Some (v, lcu)
  | exception Not_found -> None

type win10_release_status = [ `Deprecated | `Active ]

(* https://en.wikipedia.org/wiki/Windows_10_version_history#Channels *)
let win10_release_status v : win10_release_status =
  match v with
  | `V1507 -> `Deprecated
  | `Ltsc2015 -> `Active
  | `V1511 -> `Deprecated
  | `V1607 -> `Deprecated
  | `Ltsc2016 -> `Active
  | `V1703 | `V1709 | `V1803 | `V1809 -> `Deprecated
  | `Ltsc2019 -> `Active
  | `V1903 | `V1909 -> `Deprecated
  | `V2004 -> `Deprecated
  | `V20H2 | `V21H1 | `Ltsc2022 -> `Active

let win10_latest_release = `V21H1
let win10_latest_image = `Ltsc2022

let win10_release_to_string = function
  | `V1507 -> "1507"
  | `Ltsc2015 -> "ltsc2015"
  | `V1511 -> "1511"
  | `V1607 -> "1607"
  | `Ltsc2016 -> "ltsc2016"
  | `V1703 -> "1703"
  | `V1709 -> "1709"
  | `V1803 -> "1803"
  | `V1809 -> "1809"
  | `Ltsc2019 -> "ltsc2019"
  | `V1903 -> "1903"
  | `V1909 -> "1909"
  | `V2004 -> "2004"
  | `V20H2 -> "20H2"
  | `V21H1 -> "21H1"
  | `Ltsc2022 -> "ltsc2022"

let win10_release_of_string v : win10_release option =
  let v =
    match String.cut ~sep:"-KB" v with
    | Some (v, kb) -> if String.for_all Char.Ascii.is_digit kb then v else ""
    | None -> v
  in
  match v with
  | "1507" -> Some `V1507
  | "ltsc2015" -> Some `Ltsc2015
  | "1511" -> Some `V1511
  | "1607" -> Some `V1607
  | "ltsc2016" -> Some `Ltsc2016
  | "1703" -> Some `V1703
  | "1709" -> Some `V1709
  | "1803" -> Some `V1803
  | "1809" -> Some `V1809
  | "ltsc2019" -> Some `Ltsc2019
  | "1903" -> Some `V1903
  | "1909" -> Some `V1909
  | "2004" -> Some `V2004
  | "20H2" -> Some `V20H2
  | "21H1" -> Some `V21H1
  | "ltsc2022" -> Some `Ltsc2022
  | _ -> None

let rec win10_revision_to_string = function
  | v, None -> win10_release_to_string v
  | v, Some `LCU -> win10_revision_to_string (v, Some win10_current_lcu)
  | v, Some lcu -> (
      match win10_lcu_kb_number v lcu with
      | Some kb -> Printf.sprintf "%s-KB%d" (win10_release_to_string v) kb
      | None ->
          Fmt.invalid_arg "No KB for this Win10 %s revision"
            (win10_release_to_string v))

let win10_revision_of_string v =
  let v, lcu =
    match String.cut ~sep:"-KB" v with
    | Some (v, lcu) when String.for_all Char.Ascii.is_digit lcu ->
        (v, Some (int_of_string lcu))
    | _ -> (v, None)
  in
  match (win10_release_of_string v, lcu) with
  | None, _ -> None
  | Some v, None -> Some (v, None)
  | Some v, Some lcu -> win10_kb_number_to_lcu v lcu
