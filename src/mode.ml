open! Stdune
open! Import

type t = Byte | Native | Js

let all = [Byte; Native ; Js]

let decode =
  let open Dune_lang.Decoder in
  enum
    [ "byte"   , Byte
    ; "native" , Native
    ; "js"     , Js
    ]

let encode =
  let open Dune_lang.Encoder in
  function
  | Byte -> string "byte"
  | Native -> string "native"
  | Js -> string "js"

let pp fmt = function
  | Byte -> Format.pp_print_string fmt "byte"
  | Native -> Format.pp_print_string fmt "native"
  | Js -> Format.pp_print_string fmt "js"

let choose byte native js = function
  | Byte   -> byte
  | Native -> native
  | Js -> js

let compiled_unit_ext = choose ".cmo" ".cmx" "cmj"
let compiled_lib_ext = choose ".cma" ".cmxa" "cmj"
let plugin_ext = choose ".cma" ".cmxs" "cmj"

let variant = choose Variant.byte Variant.native Variant.js

let cm_kind = choose Cm_kind.Cmo Cmx Cmj

let exe_ext = choose ".bc" ".exe" ".bs.js"

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi | Cmo -> Byte
  | Cmx -> Native
  | Cmj -> Js

module Dict = struct
  type 'a t =
    { byte   : 'a
    ; native : 'a
    ; js     : 'a
    }

  let for_all { byte ; native ; js } ~f = f byte && f native && f js

  let pp pp fmt { byte; native ; js } =
    Fmt.record fmt
      [ "byte", Fmt.const pp byte
      ; "native", Fmt.const pp native
      ; "js", Fmt.const pp js
      ]

  let get t = function
    | Byte   -> t.byte
    | Native -> t.native
    | Js     -> t.js

  let of_func f =
    { byte   = f ~mode:Byte
    ; native = f ~mode:Native
    ; js     = f ~mode:Js
    }

  let map2 a b ~f =
    { byte   = f a.byte   b.byte
    ; native = f a.native b.native
    ; js = f a.js b.js
    }

  let map t ~f =
    { byte = f t.byte
    ; native = f t.native
    ; js = f t.js
    }

  let make_both x =
    { byte   = x
    ; native = x
    ; js     = x
    }

  let make ~byte ~native ~js = { byte ; native ; js }

  module Set = struct
    type nonrec t = bool t

    let all =
      { byte   = true
      ; native = true
      ; js     = true
      }

    let to_list t =
      let l = [] in
      let l = if t.native then Native :: l else l in
      let l = if t.byte   then Byte   :: l else l in
      let l = if t.js     then Js     :: l else l in
      l

    let of_list l =
      { byte   = List.mem Byte   ~set:l
      ; native = List.mem Native ~set:l
      ; js     = List.mem Js     ~set:l
      }

    let encode t = List.map ~f:encode (to_list t)

    let is_empty t = not (t.byte || t.native || t.js)

    let iter t ~f =
      if t.byte   then f Byte;
      if t.native then f Native;
      if t.js     then f Js
  end

  module List = struct
    type nonrec 'a t = 'a list t

    let empty = { byte = [] ; native = [] ; js = [] }

    let encode f { byte ; native ; js } =
      let open Dune_lang.Encoder in
      record_fields
        [ field_l "byte" f byte
        ; field_l "native" f native
        ; field_l "js" f js
        ]

    let decode f =
      let open Stanza.Decoder in
      record (
        let+ byte = field ~default:[] "byte" (list f)
        and+ native = field ~default:[] "native" (list f)
        and+ js = field ~default:[] "js" (list f)
        in
        { byte
        ; native
        ; js
        }
      )
  end
end
