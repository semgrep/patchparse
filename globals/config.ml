(* All of the various global variables and flags. *)

let git = ref true
let gittag = ref ""

let out_dir = ref "out"
let name_depth = ref 3

let file = ref ""
let filename = ref ""
let complete_filename = ref ""

(* at least 5 blocks must be the same *)
let same_threshold = ref 5
(*the same blocks must be in at least 3 different files*)
let file_threshold = ref 3
let version_threshold = ref 0
let directory_threshold = ref 0

(* ignore strings of more than this many non-whitespace characters *)
let length_threshold = 1400

let verbose = ref false
let notex = ref false
let noall = ref false
let nofilters = ref false
let noev = ref false
let page_width_threshold = ref 60 (* 1/2 num chars on a latex line *)

(* to have it in a suitably globally visible place *)
(* author and date only for git files *)
let version_table =
  (Hashtbl.create(50) : (int (* index *), string (* version *)) Hashtbl.t)

let get_version id = Hashtbl.find version_table id

(* prints output to stdout with some delimiters for easier parsing *)
let print_parsable = ref false

(* --------------------------------------------------------------- *)

type filter = ConstantsOnly | Anything

let filter = ref Anything
let set_constants_only _ = filter := ConstantsOnly
let set_anything _ = filter := Anything

(*let url = "http://grmso.net:8090/commit/"*)
let url = ref "http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=commit;h="

let set_git_url s = url := s ^ ";a=commit;h="
