(** List Windows 10 versions, LCUs, and their status. *)

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
(** All Windows 10 release versions. LTSC versions are aliased to the
    semi-annual release they're based on. *)

val win10_releases : win10_release list
(** All Windows 10 releases versions. *)

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
(** Windows 10 Latest Cumulative Update. Out-of-band LCUs are not included
    as they aren't released with Docker images. [`LCU] always refers to the
    most recent LCU. *)

val win10_current_lcu : win10_lcu
(** Current Windows 10 Latest Cumulative Update; value used when [`LCU] is
    specified. *)

type win10_revision = win10_release * win10_lcu option [@@deriving sexp]
(** A Windows 10 version optionally with an LCU. *)

val win10_release_to_string : win10_release -> string
(** [win10_release_to_string update] converts a Windows 10 version name to
    string. *)

val win10_release_of_string : string -> win10_release option
(** [win10_release_of_string] converts a Windows 10 version name as
    string to its internal representation. Ignores any KB number. *)

val win10_revision_to_string : win10_revision -> string
val win10_revision_of_string : string -> win10_revision option

type win10_release_status = [ `Deprecated | `Active ]
(** Windows 10 release status. *)

val win10_release_status : win10_release -> win10_release_status
(** [win10_release_status v channel] returns the Microsoft support
   status of the specified Windows 10 release.
   @see <https://en.wikipedia.org/wiki/Windows_10_version_history#Channels> *)

val win10_latest_release : win10_release
(** Latest Windows 10 release. *)

val win10_latest_image : win10_release
(** Latest Windows 10 Docker image available. May differ from
    {!win10_latest_release} if the Docker repository hasn't been
    updated. *)
