open! Core

module Keyed : sig
  module type A = sig
    module Key : sig
      type t [@@deriving bin_io]
    end

    module Value : sig
      type t [@@deriving bin_io]
    end

    val key : Value.t -> Key.t

    val file : string

    val name : string
  end

  module M (A : A) : sig
    include A with type Key.t = A.Key.t and type Value.t = A.Value.t

    val read_all : unit -> A.Value.t list

    val insert : A.Value.t -> unit

    val get : A.Key.t -> A.Value.t option

    val change : A.Key.t -> f:(A.Value.t -> A.Value.t Lwt.t) -> unit Lwt.t

    val delete_key : A.Key.t -> unit

    val delete : A.Value.t -> unit

    val delete_everything : unit -> unit
  end
end

module Singleton : sig
  module type A = sig
    type t [@@deriving bin_io]

    val file : string

    val name : string
  end

  module M (A : A) : sig
    val get : default:A.t -> A.t

    val peek : unit -> A.t option

    val set : A.t -> unit
  end
end
