open! Core
open! Import

module Sql = struct
  module type A = sig
    val file : string

    val name : string
  end

  module M (A : A) = struct
    let check = Sqlite3.Rc.check

    let create_table_if_not_exists conn =
      let stmt =
        Sqlite3.prepare
          conn
          [%string
            {| CREATE TABLE IF NOT EXISTS %{A.name} (
               key BLOB PRIMARY KEY,
               value BLOB NOT NULL
             ) |}]
      in
      Sqlite3.step stmt |> check ;
      Sqlite3.finalize stmt |> check


    let create_conn_and_table () =
      let conn = Sqlite3.db_open A.file in
      create_table_if_not_exists conn ;
      conn


    let conn = lazy (create_conn_and_table ())

    let create_table_if_not_exists () = create_table_if_not_exists !!conn

    let read_all () =
      let stmt =
        Sqlite3.prepare !!conn [%string {| SELECT * FROM %{A.name} |}]
      in
      let rc, all =
        Sqlite3.fold ~init:[] stmt ~f:(fun acc data ->
            let key = data.(0) |> Sqlite3.Data.to_string_exn in
            let value = data.(1) |> Sqlite3.Data.to_string_exn in
            (key, value) :: acc )
      in
      check rc ;
      Sqlite3.finalize stmt |> check ;
      all


    let insert_kv key value =
      let stmt =
        Sqlite3.prepare
          !!conn
          [%string {| REPLACE INTO %{A.name} (key, value) VALUES ( ?, ? ) |}]
      in
      Sqlite3.bind_blob stmt 1 key |> check ;
      Sqlite3.bind_blob stmt 2 value |> check ;
      Sqlite3.step stmt |> check ;
      Sqlite3.finalize stmt |> check


    let get_key key =
      let stmt =
        Sqlite3.prepare
          !!conn
          [%string {| SELECT * FROM %{A.name} WHERE key = ?  |}]
      in
      Sqlite3.bind_blob stmt 1 key |> check ;
      let rc, all =
        Sqlite3.fold ~init:[] stmt ~f:(fun acc data ->
            let value = data.(1) |> Sqlite3.Data.to_string_exn in
            value :: acc )
      in
      check rc ;
      Sqlite3.finalize stmt |> check ;
      List.hd all


    let delete_key key =
      let stmt =
        Sqlite3.prepare
          !!conn
          [%string {| DELETE FROM %{A.name} WHERE key = ? |}]
      in
      Sqlite3.bind_blob stmt 1 key |> check ;
      Sqlite3.step stmt |> check ;
      Sqlite3.finalize stmt |> check


    let drop_table () =
      let stmt = Sqlite3.prepare !!conn [%string {| DROP TABLE %{A.name} |}] in
      Sqlite3.step stmt |> check ;
      Sqlite3.finalize stmt |> check


    let recreate_table () =
      drop_table () ;
      create_table_if_not_exists ()
  end
end

module Keyed = struct
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

  module M (A : A) = struct
    module Sql = Sql.M (A)
    include A

    let read_all () =
      Sql.read_all ()
      |> List.map ~f:(fun (_key, value) ->
             Bin_prot.Reader.of_string A.Value.bin_reader_t value )
      |> List.rev


    let insert value =
      let key = A.key value in
      let key = Bin_prot.Writer.to_string A.Key.bin_writer_t key in
      let value = Bin_prot.Writer.to_string A.Value.bin_writer_t value in
      Sql.insert_kv key value


    let delete_key key =
      let key = Bin_prot.Writer.to_string A.Key.bin_writer_t key in
      Sql.delete_key key


    let get key =
      let key = Bin_prot.Writer.to_string A.Key.bin_writer_t key in
      let value = Sql.get_key key in
      Option.map value ~f:(Bin_prot.Reader.of_string A.Value.bin_reader_t)


    let delete value =
      let key = A.key value in
      delete_key key


    let change key ~f =
      let key = Bin_prot.Writer.to_string A.Key.bin_writer_t key in
      let value = Sql.get_key key |> Option.value_exn in
      let value = Bin_prot.Reader.of_string A.Value.bin_reader_t value in
      let+ value = f value in
      let value = Bin_prot.Writer.to_string A.Value.bin_writer_t value in
      (* Use replace instead *)
      Sql.insert_kv key value


    let delete_everything () = Sql.recreate_table ()
  end
end

module Singleton = struct
  module type A = sig
    type t [@@deriving bin_io]

    val file : string

    val name : string
  end

  module M (A : A) = struct
    module Inner_store = Keyed.M (struct
      module Key = Unit
      module Value = A
      include A

      let key _value = ()
    end)

    let peek () = Inner_store.read_all () |> List.hd

    let get ~default = peek () |> Option.value ~default

    let set value =
      Inner_store.delete_everything () ;
      Inner_store.insert value
  end
end
