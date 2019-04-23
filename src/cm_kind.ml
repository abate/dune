open Stdune

type t = Cmi | Cmo | Cmx | Cmj

let all = [Cmi; Cmo; Cmx; Cmj]

let choose cmi cmo cmx cmj = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx
  | Cmj -> cmj

let ext = choose ".cmi" ".cmo" ".cmx" ".cmj"

let pp fmt p = Format.pp_print_string fmt (ext p)

let source = choose Ml_kind.Intf Impl Impl Impl

module Dict = struct
  type 'a t =
    { cmi : 'a
    ; cmo : 'a
    ; cmx : 'a
    ; cmj : 'a
    }

  let get t = function
    | Cmi -> t.cmi
    | Cmo -> t.cmo
    | Cmx -> t.cmx
    | Cmj -> t.cmj

  let of_func f =
    { cmi = f ~cm_kind:Cmi
    ; cmo = f ~cm_kind:Cmo
    ; cmx = f ~cm_kind:Cmx
    ; cmj = f ~cm_kind:Cmj
    }

  let make_all x =
    { cmi = x
    ; cmo = x
    ; cmx = x
    ; cmj = x
    }
end

let to_sexp =
  let open Sexp.Encoder in
  function
  | Cmi -> string "cmi"
  | Cmo -> string "cmo"
  | Cmx -> string "cmx"
  | Cmj -> string "cmj"
