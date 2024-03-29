(* All of the various global variables and flags. *)

let git = ref true
let gitcommitlist = ref false

let gitpatch = ref false
let gittag = ref ""
let git_restrict = ref ""

let out_dir = ref "out"  (* for .tex and .cocci files *)
let dest_dir = ref "." (* for files obtained from git *)
let name_depth = ref 3

let file = ref ""
let outfile = ref "" (* for use with all_, often the same as file *)
let filename = ref ""
let complete_filename = ref ""

(* at least 5 blocks must be the same *)
let same_threshold = ref 5
(*the same blocks must be in at least 3 different files*)
let file_threshold = ref 3
let version_threshold = ref 0
let directory_threshold = ref 0

(* pad: new config flags *)
let drop_substring_filename_from_ident = ref false

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
  (* pad: Patch.id -> Git.commitid *)
  (Hashtbl.create(50) : (int (* index *), string (* version *)) Hashtbl.t)

let get_version id = Hashtbl.find version_table id

(* prints output to stdout with some delimiters for easier parsing *)
let print_parsable = ref false
let print_sp = ref false

(* --------------------------------------------------------------- *)

type filter = ConstantsOnly | Anything

let filter = ref Anything
let set_constants_only _ = filter := ConstantsOnly
let set_anything _ = filter := Anything

(*let url = "http://grmso.net:8090/commit/"*)
let url = ref "http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=commit;h="

let gitdir = ref "" (* local directory containing the git repository *)
