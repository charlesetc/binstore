open! Core

module Key = struct
  type t = int [@@deriving sexp, bin_io]
end

module Value = struct
  type t = { cool : int; wow : string } [@@deriving sexp, bin_io]
end

module Values = Binstore.Keyed.M (struct
  module Key = Key
  module Value = Value

  let key ({ cool; wow = _ } : Value.t) = cool

  let file = "test.db"

  let name = "keys_to_values"
end)

let () =
  Values.insert ({ cool = 2; wow = "hi" } : Value.t);
  Values.read_all () |> [%sexp_of: Value.t list] |> Sexp.output stdout
