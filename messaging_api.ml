[@@@ocaml.warning "-27-32-37-60"]

type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  module MessageWrapper : Capnp.RPC.S
  type 'cap message_t = 'cap MessageWrapper.Message.t
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t


  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t = ro MessageWrapper.Slice.t option
    val of_pointer : pointer_t -> 'a reader_t
    module Op : sig
      type struct_t = [`Op_e9d5c8e1ee60afb7]
      type t = struct_t reader_t
      module Write : sig
        type struct_t = [`Write_db16fd067ea60b09]
        type t = struct_t reader_t
        val has_value : t -> bool
        val value_get : t -> string
        val of_message : 'cap message_t -> t
        val of_builder : struct_t builder_t -> t
      end
      type unnamed_union_t =
        | Read
        | Write of Write.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val has_key : t -> bool
      val key_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module Command : sig
      type struct_t = [`Command_bed1c32b27533f00]
      type t = struct_t reader_t
      val has_op : t -> bool
      val op_get : t -> Op.t
      val op_get_pipelined : struct_t MessageWrapper.StructRef.t -> Op.struct_t MessageWrapper.StructRef.t
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module LogEntry : sig
      type struct_t = [`LogEntry_f0494224a2e91235]
      type t = struct_t reader_t
      val has_command : t -> bool
      val command_get : t -> Command.t
      val command_get_pipelined : struct_t MessageWrapper.StructRef.t -> Command.struct_t MessageWrapper.StructRef.t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module RequestVote : sig
      type struct_t = [`RequestVote_feff051f9889df8d]
      type t = struct_t reader_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val leader_commit_get : t -> int64
      val leader_commit_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module RequestVoteResp : sig
      type struct_t = [`RequestVoteResp_f0eb2779e4b73008]
      type t = struct_t reader_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val vote_granted_get : t -> bool
      val has_entries : t -> bool
      val entries_get : t -> (ro, LogEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> LogEntry.t list
      val entries_get_array : t -> LogEntry.t array
      val start_index_get : t -> int64
      val start_index_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module AppendEntries : sig
      type struct_t = [`AppendEntries_c59bcc152c6e44c7]
      type t = struct_t reader_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val prev_log_index_get : t -> int64
      val prev_log_index_get_int_exn : t -> int
      val prev_log_term_get : t -> int64
      val prev_log_term_get_int_exn : t -> int
      val has_entries : t -> bool
      val entries_get : t -> (ro, LogEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> LogEntry.t list
      val entries_get_array : t -> LogEntry.t array
      val leader_commit_get : t -> int64
      val leader_commit_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module AppendEntriesResp : sig
      type struct_t = [`AppendEntriesResp_84d8cf475f4b9f54]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Success of int64
        | Failure of int64
        | Undefined of int
      val get : t -> unnamed_union_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ClientRequest : sig
      type struct_t = [`ClientRequest_eb238de24963377c]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Read
        | Write of string
        | Undefined of int
      val get : t -> unnamed_union_t
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val has_key : t -> bool
      val key_get : t -> string
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ClientResponse : sig
      type struct_t = [`ClientResponse_94fe3f4a27ce94b8]
      type t = struct_t reader_t
      type unnamed_union_t =
        | Success
        | ReadSuccess of string
        | Failure
        | Undefined of int
      val get : t -> unnamed_union_t
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
    module ServerMessage : sig
      type struct_t = [`ServerMessage_d297ccb012332ee0]
      type t = struct_t reader_t
      type unnamed_union_t =
        | RequestVote of RequestVote.t
        | RequestVoteResp of RequestVoteResp.t
        | AppendEntries of AppendEntries.t
        | AppendEntriesResp of AppendEntriesResp.t
        | ClientRequest of ClientRequest.t
        | ClientResponse of ClientResponse.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : struct_t builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t
    module Op : sig
      type struct_t = [`Op_e9d5c8e1ee60afb7]
      type t = struct_t builder_t
      module Write : sig
        type struct_t = [`Write_db16fd067ea60b09]
        type t = struct_t builder_t
        val has_value : t -> bool
        val value_get : t -> string
        val value_set : t -> string -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> struct_t reader_t
        val init_root : ?message_size:int -> unit -> t
        val init_pointer : pointer_t -> t
      end
      type unnamed_union_t =
        | Read
        | Write of Write.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val read_set : t -> unit
      val write_init : t -> Write.t
      val has_key : t -> bool
      val key_get : t -> string
      val key_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module Command : sig
      type struct_t = [`Command_bed1c32b27533f00]
      type t = struct_t builder_t
      val has_op : t -> bool
      val op_get : t -> Op.t
      val op_set_reader : t -> Op.struct_t reader_t -> Op.t
      val op_set_builder : t -> Op.t -> Op.t
      val op_init : t -> Op.t
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val id_set : t -> int64 -> unit
      val id_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module LogEntry : sig
      type struct_t = [`LogEntry_f0494224a2e91235]
      type t = struct_t builder_t
      val has_command : t -> bool
      val command_get : t -> Command.t
      val command_set_reader : t -> Command.struct_t reader_t -> Command.t
      val command_set_builder : t -> Command.t -> Command.t
      val command_init : t -> Command.t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val term_set : t -> int64 -> unit
      val term_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module RequestVote : sig
      type struct_t = [`RequestVote_feff051f9889df8d]
      type t = struct_t builder_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val term_set : t -> int64 -> unit
      val term_set_int : t -> int -> unit
      val leader_commit_get : t -> int64
      val leader_commit_get_int_exn : t -> int
      val leader_commit_set : t -> int64 -> unit
      val leader_commit_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module RequestVoteResp : sig
      type struct_t = [`RequestVoteResp_f0eb2779e4b73008]
      type t = struct_t builder_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val term_set : t -> int64 -> unit
      val term_set_int : t -> int -> unit
      val vote_granted_get : t -> bool
      val vote_granted_set : t -> bool -> unit
      val has_entries : t -> bool
      val entries_get : t -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> LogEntry.t list
      val entries_get_array : t -> LogEntry.t array
      val entries_set : t -> (rw, LogEntry.t, array_t) Capnp.Array.t -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_set_list : t -> LogEntry.t list -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_set_array : t -> LogEntry.t array -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_init : t -> int -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val start_index_get : t -> int64
      val start_index_get_int_exn : t -> int
      val start_index_set : t -> int64 -> unit
      val start_index_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module AppendEntries : sig
      type struct_t = [`AppendEntries_c59bcc152c6e44c7]
      type t = struct_t builder_t
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val term_set : t -> int64 -> unit
      val term_set_int : t -> int -> unit
      val prev_log_index_get : t -> int64
      val prev_log_index_get_int_exn : t -> int
      val prev_log_index_set : t -> int64 -> unit
      val prev_log_index_set_int : t -> int -> unit
      val prev_log_term_get : t -> int64
      val prev_log_term_get_int_exn : t -> int
      val prev_log_term_set : t -> int64 -> unit
      val prev_log_term_set_int : t -> int -> unit
      val has_entries : t -> bool
      val entries_get : t -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_get_list : t -> LogEntry.t list
      val entries_get_array : t -> LogEntry.t array
      val entries_set : t -> (rw, LogEntry.t, array_t) Capnp.Array.t -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_set_list : t -> LogEntry.t list -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_set_array : t -> LogEntry.t array -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val entries_init : t -> int -> (rw, LogEntry.t, array_t) Capnp.Array.t
      val leader_commit_get : t -> int64
      val leader_commit_get_int_exn : t -> int
      val leader_commit_set : t -> int64 -> unit
      val leader_commit_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module AppendEntriesResp : sig
      type struct_t = [`AppendEntriesResp_84d8cf475f4b9f54]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Success of int64
        | Failure of int64
        | Undefined of int
      val get : t -> unnamed_union_t
      val success_set : t -> int64 -> unit
      val success_set_int : t -> int -> unit
      val failure_set : t -> int64 -> unit
      val failure_set_int : t -> int -> unit
      val term_get : t -> int64
      val term_get_int_exn : t -> int
      val term_set : t -> int64 -> unit
      val term_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ClientRequest : sig
      type struct_t = [`ClientRequest_eb238de24963377c]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Read
        | Write of string
        | Undefined of int
      val get : t -> unnamed_union_t
      val read_set : t -> unit
      val write_set : t -> string -> unit
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val id_set : t -> int64 -> unit
      val id_set_int : t -> int -> unit
      val has_key : t -> bool
      val key_get : t -> string
      val key_set : t -> string -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ClientResponse : sig
      type struct_t = [`ClientResponse_94fe3f4a27ce94b8]
      type t = struct_t builder_t
      type unnamed_union_t =
        | Success
        | ReadSuccess of string
        | Failure
        | Undefined of int
      val get : t -> unnamed_union_t
      val success_set : t -> unit
      val read_success_set : t -> string -> unit
      val failure_set : t -> unit
      val id_get : t -> int64
      val id_get_int_exn : t -> int
      val id_set : t -> int64 -> unit
      val id_set_int : t -> int -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
    module ServerMessage : sig
      type struct_t = [`ServerMessage_d297ccb012332ee0]
      type t = struct_t builder_t
      type unnamed_union_t =
        | RequestVote of RequestVote.t
        | RequestVoteResp of RequestVoteResp.t
        | AppendEntries of AppendEntries.t
        | AppendEntriesResp of AppendEntriesResp.t
        | ClientRequest of ClientRequest.t
        | ClientResponse of ClientResponse.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val request_vote_set_reader : t -> RequestVote.struct_t reader_t -> RequestVote.t
      val request_vote_set_builder : t -> RequestVote.t -> RequestVote.t
      val request_vote_init : t -> RequestVote.t
      val request_vote_resp_set_reader : t -> RequestVoteResp.struct_t reader_t -> RequestVoteResp.t
      val request_vote_resp_set_builder : t -> RequestVoteResp.t -> RequestVoteResp.t
      val request_vote_resp_init : t -> RequestVoteResp.t
      val append_entries_set_reader : t -> AppendEntries.struct_t reader_t -> AppendEntries.t
      val append_entries_set_builder : t -> AppendEntries.t -> AppendEntries.t
      val append_entries_init : t -> AppendEntries.t
      val append_entries_resp_set_reader : t -> AppendEntriesResp.struct_t reader_t -> AppendEntriesResp.t
      val append_entries_resp_set_builder : t -> AppendEntriesResp.t -> AppendEntriesResp.t
      val append_entries_resp_init : t -> AppendEntriesResp.t
      val client_request_set_reader : t -> ClientRequest.struct_t reader_t -> ClientRequest.t
      val client_request_set_builder : t -> ClientRequest.t -> ClientRequest.t
      val client_request_init : t -> ClientRequest.t
      val client_response_set_reader : t -> ClientResponse.struct_t reader_t -> ClientResponse.t
      val client_response_set_builder : t -> ClientResponse.t -> ClientResponse.t
      val client_response_init : t -> ClientResponse.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> struct_t reader_t
      val init_root : ?message_size:int -> unit -> t
      val init_pointer : pointer_t -> t
    end
  end
end

module MakeRPC(MessageWrapper : Capnp.RPC.S) = struct
  type 'a reader_t = 'a MessageWrapper.StructStorage.reader_t
  type 'a builder_t = 'a MessageWrapper.StructStorage.builder_t
  module CamlBytes = Bytes
  module DefaultsMessage_ = Capnp.BytesMessage

  let _builder_defaults_message =
    let message_segments = [
      Bytes.unsafe_of_string "\
      ";
    ] in
    DefaultsMessage_.Message.readonly
      (DefaultsMessage_.Message.of_storage message_segments)

  let invalid_msg = Capnp.Message.invalid_msg

  include Capnp.Runtime.BuilderInc.Make[@inlined](MessageWrapper)

  type 'cap message_t = 'cap MessageWrapper.Message.t

  module DefaultsCopier_ =
    Capnp.Runtime.BuilderOps.Make(Capnp.BytesMessage)(MessageWrapper)

  let _reader_defaults_message =
    MessageWrapper.Message.create
      (DefaultsMessage_.Message.total_size _builder_defaults_message)


  module Reader = struct
    type array_t = ro MessageWrapper.ListStorage.t
    type builder_array_t = rw MessageWrapper.ListStorage.t
    type pointer_t = ro MessageWrapper.Slice.t option
    let of_pointer = RA_.deref_opt_struct_pointer

    module Op = struct
      type struct_t = [`Op_e9d5c8e1ee60afb7]
      type t = struct_t reader_t
      module Write = struct
        type struct_t = [`Write_db16fd067ea60b09]
        type t = struct_t reader_t
        let has_value x =
          RA_.has_field x 1
        let value_get x =
          RA_.get_blob ~default:"" x 1
        let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
        let of_builder x = Some (RA_.StructStorage.readonly x)
      end
      let read_get x = ()
      let write_get x = RA_.cast_struct x
      type unnamed_union_t =
        | Read
        | Write of Write.t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> Read
        | 1 -> Write (write_get x)
        | v -> Undefined v
      let has_key x =
        RA_.has_field x 0
      let key_get x =
        RA_.get_blob ~default:"" x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module Command = struct
      type struct_t = [`Command_bed1c32b27533f00]
      type t = struct_t reader_t
      let has_op x =
        RA_.has_field x 0
      let op_get x =
        RA_.get_struct x 0
      let op_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let id_get x =
        RA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module LogEntry = struct
      type struct_t = [`LogEntry_f0494224a2e91235]
      type t = struct_t reader_t
      let has_command x =
        RA_.has_field x 0
      let command_get x =
        RA_.get_struct x 0
      let command_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let term_get x =
        RA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module RequestVote = struct
      type struct_t = [`RequestVote_feff051f9889df8d]
      type t = struct_t reader_t
      let term_get x =
        RA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let leader_commit_get x =
        RA_.get_int64 ~default:(0L) x 8
      let leader_commit_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (leader_commit_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module RequestVoteResp = struct
      type struct_t = [`RequestVoteResp_f0eb2779e4b73008]
      type t = struct_t reader_t
      let term_get x =
        RA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let vote_granted_get x =
        RA_.get_bit ~default:false x ~byte_ofs:8 ~bit_ofs:0
      let has_entries x =
        RA_.has_field x 0
      let entries_get x = 
        RA_.get_struct_list x 0
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let start_index_get x =
        RA_.get_int64 ~default:(0L) x 16
      let start_index_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (start_index_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module AppendEntries = struct
      type struct_t = [`AppendEntries_c59bcc152c6e44c7]
      type t = struct_t reader_t
      let term_get x =
        RA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let prev_log_index_get x =
        RA_.get_int64 ~default:(0L) x 8
      let prev_log_index_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (prev_log_index_get x)
      let prev_log_term_get x =
        RA_.get_int64 ~default:(0L) x 16
      let prev_log_term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (prev_log_term_get x)
      let has_entries x =
        RA_.has_field x 0
      let entries_get x = 
        RA_.get_struct_list x 0
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let leader_commit_get x =
        RA_.get_int64 ~default:(0L) x 24
      let leader_commit_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (leader_commit_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module AppendEntriesResp = struct
      type struct_t = [`AppendEntriesResp_84d8cf475f4b9f54]
      type t = struct_t reader_t
      let success_get x =
        RA_.get_int64 ~default:(0L) x 8
      let success_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (success_get x)
      let failure_get x =
        RA_.get_int64 ~default:(0L) x 8
      let failure_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (failure_get x)
      type unnamed_union_t =
        | Success of int64
        | Failure of int64
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 16 with
        | 0 -> Success (success_get x)
        | 1 -> Failure (failure_get x)
        | v -> Undefined v
      let term_get x =
        RA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ClientRequest = struct
      type struct_t = [`ClientRequest_eb238de24963377c]
      type t = struct_t reader_t
      let read_get x = ()
      let has_write x =
        RA_.has_field x 1
      let write_get x =
        RA_.get_blob ~default:"" x 1
      type unnamed_union_t =
        | Read
        | Write of string
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 8 with
        | 0 -> Read
        | 1 -> Write (write_get x)
        | v -> Undefined v
      let id_get x =
        RA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let has_key x =
        RA_.has_field x 0
      let key_get x =
        RA_.get_blob ~default:"" x 0
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ClientResponse = struct
      type struct_t = [`ClientResponse_94fe3f4a27ce94b8]
      type t = struct_t reader_t
      let success_get x = ()
      let has_read_success x =
        RA_.has_field x 0
      let read_success_get x =
        RA_.get_blob ~default:"" x 0
      let failure_get x = ()
      type unnamed_union_t =
        | Success
        | ReadSuccess of string
        | Failure
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 8 with
        | 0 -> Success
        | 1 -> ReadSuccess (read_success_get x)
        | 2 -> Failure
        | v -> Undefined v
      let id_get x =
        RA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
    module ServerMessage = struct
      type struct_t = [`ServerMessage_d297ccb012332ee0]
      type t = struct_t reader_t
      let has_request_vote x =
        RA_.has_field x 0
      let request_vote_get x =
        RA_.get_struct x 0
      let request_vote_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_request_vote_resp x =
        RA_.has_field x 0
      let request_vote_resp_get x =
        RA_.get_struct x 0
      let request_vote_resp_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_append_entries x =
        RA_.has_field x 0
      let append_entries_get x =
        RA_.get_struct x 0
      let append_entries_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_append_entries_resp x =
        RA_.has_field x 0
      let append_entries_resp_get x =
        RA_.get_struct x 0
      let append_entries_resp_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_client_request x =
        RA_.has_field x 0
      let client_request_get x =
        RA_.get_struct x 0
      let client_request_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      let has_client_response x =
        RA_.has_field x 0
      let client_response_get x =
        RA_.get_struct x 0
      let client_response_get_pipelined x =
        MessageWrapper.Untyped.struct_field x 0
      type unnamed_union_t =
        | RequestVote of RequestVote.t
        | RequestVoteResp of RequestVoteResp.t
        | AppendEntries of AppendEntries.t
        | AppendEntriesResp of AppendEntriesResp.t
        | ClientRequest of ClientRequest.t
        | ClientResponse of ClientResponse.t
        | Undefined of int
      let get x =
        match RA_.get_uint16 ~default:0 x 0 with
        | 0 -> RequestVote (request_vote_get x)
        | 1 -> RequestVoteResp (request_vote_resp_get x)
        | 2 -> AppendEntries (append_entries_get x)
        | 3 -> AppendEntriesResp (append_entries_resp_get x)
        | 4 -> ClientRequest (client_request_get x)
        | 5 -> ClientResponse (client_response_get x)
        | v -> Undefined v
      let of_message x = RA_.get_root_struct (RA_.Message.readonly x)
      let of_builder x = Some (RA_.StructStorage.readonly x)
    end
  end

  module Builder = struct
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t = rw MessageWrapper.Slice.t

    module Op = struct
      type struct_t = [`Op_e9d5c8e1ee60afb7]
      type t = struct_t builder_t
      module Write = struct
        type struct_t = [`Write_db16fd067ea60b09]
        type t = struct_t builder_t
        let has_value x =
          BA_.has_field x 1
        let value_get x =
          BA_.get_blob ~default:"" x 1
        let value_set x v =
          BA_.set_blob x 1 v
        let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
        let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
        let to_reader x = Some (RA_.StructStorage.readonly x)
        let init_root ?message_size () =
          BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
        let init_pointer ptr =
          BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
      end
      let read_get x = ()
      let read_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x
      let write_get x = BA_.cast_struct x
      let write_init x =
        let data = x.BA_.NM.StructStorage.data in
        let pointers = x.BA_.NM.StructStorage.pointers in
        let () = ignore data in
        let () = ignore pointers in
        let () = BA_.set_opt_discriminant data
          (Some {BA_.Discr.value=1; BA_.Discr.byte_ofs=0})
        in
        let () =
          let ptr = {
            pointers with
            MessageWrapper.Slice.start = pointers.MessageWrapper.Slice.start + 8;
            MessageWrapper.Slice.len = 8;
          } in
          let () = BA_.BOps.deep_zero_pointer ptr in
          MessageWrapper.Slice.set_int64 ptr 0 0L
        in
        BA_.cast_struct x
      type unnamed_union_t =
        | Read
        | Write of Write.t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> Read
        | 1 -> Write (write_get x)
        | v -> Undefined v
      let has_key x =
        BA_.has_field x 0
      let key_get x =
        BA_.get_blob ~default:"" x 0
      let key_set x v =
        BA_.set_blob x 0 v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:2
    end
    module Command = struct
      type struct_t = [`Command_bed1c32b27533f00]
      type t = struct_t builder_t
      let has_op x =
        BA_.has_field x 0
      let op_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:2 x 0
      let op_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 x 0 v
      let op_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:2 x 0 (Some v)
      let op_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:2 x 0
      let id_get x =
        BA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let id_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let id_set_int x v = id_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module LogEntry = struct
      type struct_t = [`LogEntry_f0494224a2e91235]
      type t = struct_t builder_t
      let has_command x =
        BA_.has_field x 0
      let command_get x =
        BA_.get_struct ~data_words:1 ~pointer_words:1 x 0
      let command_set_reader x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 v
      let command_set_builder x v =
        BA_.set_struct ~data_words:1 ~pointer_words:1 x 0 (Some v)
      let command_init x =
        BA_.init_struct ~data_words:1 ~pointer_words:1 x 0
      let term_get x =
        BA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let term_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let term_set_int x v = term_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
    module RequestVote = struct
      type struct_t = [`RequestVote_feff051f9889df8d]
      type t = struct_t builder_t
      let term_get x =
        BA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let term_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let term_set_int x v = term_set x (Int64.of_int v)
      let leader_commit_get x =
        BA_.get_int64 ~default:(0L) x 8
      let leader_commit_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (leader_commit_get x)
      let leader_commit_set x v =
        BA_.set_int64 ~default:(0L) x 8 v
      let leader_commit_set_int x v = leader_commit_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:0
    end
    module RequestVoteResp = struct
      type struct_t = [`RequestVoteResp_f0eb2779e4b73008]
      type t = struct_t builder_t
      let term_get x =
        BA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let term_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let term_set_int x v = term_set x (Int64.of_int v)
      let vote_granted_get x =
        BA_.get_bit ~default:false x ~byte_ofs:8 ~bit_ofs:0
      let vote_granted_set x v =
        BA_.set_bit ~default:false x ~byte_ofs:8 ~bit_ofs:0 v
      let has_entries x =
        BA_.has_field x 0
      let entries_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 0
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let entries_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 0 v
      let entries_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 0 n
      let entries_set_list x v =
        let builder = entries_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let entries_set_array x v =
        let builder = entries_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let start_index_get x =
        BA_.get_int64 ~default:(0L) x 16
      let start_index_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (start_index_get x)
      let start_index_set x v =
        BA_.set_int64 ~default:(0L) x 16 v
      let start_index_set_int x v = start_index_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:3 ~pointer_words:1
    end
    module AppendEntries = struct
      type struct_t = [`AppendEntries_c59bcc152c6e44c7]
      type t = struct_t builder_t
      let term_get x =
        BA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let term_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let term_set_int x v = term_set x (Int64.of_int v)
      let prev_log_index_get x =
        BA_.get_int64 ~default:(0L) x 8
      let prev_log_index_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (prev_log_index_get x)
      let prev_log_index_set x v =
        BA_.set_int64 ~default:(0L) x 8 v
      let prev_log_index_set_int x v = prev_log_index_set x (Int64.of_int v)
      let prev_log_term_get x =
        BA_.get_int64 ~default:(0L) x 16
      let prev_log_term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (prev_log_term_get x)
      let prev_log_term_set x v =
        BA_.set_int64 ~default:(0L) x 16 v
      let prev_log_term_set_int x v = prev_log_term_set x (Int64.of_int v)
      let has_entries x =
        BA_.has_field x 0
      let entries_get x = 
        BA_.get_struct_list ~data_words:1 ~pointer_words:1 x 0
      let entries_get_list x =
        Capnp.Array.to_list (entries_get x)
      let entries_get_array x =
        Capnp.Array.to_array (entries_get x)
      let entries_set x v =
        BA_.set_struct_list ~data_words:1 ~pointer_words:1 x 0 v
      let entries_init x n =
        BA_.init_struct_list ~data_words:1 ~pointer_words:1 x 0 n
      let entries_set_list x v =
        let builder = entries_init x (List.length v) in
        let () = List.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let entries_set_array x v =
        let builder = entries_init x (Array.length v) in
        let () = Array.iteri (fun i a -> Capnp.Array.set builder i a) v in
        builder
      let leader_commit_get x =
        BA_.get_int64 ~default:(0L) x 24
      let leader_commit_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (leader_commit_get x)
      let leader_commit_set x v =
        BA_.set_int64 ~default:(0L) x 24 v
      let leader_commit_set_int x v = leader_commit_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:4 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:4 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:4 ~pointer_words:1
    end
    module AppendEntriesResp = struct
      type struct_t = [`AppendEntriesResp_84d8cf475f4b9f54]
      type t = struct_t builder_t
      let success_get x =
        BA_.get_int64 ~default:(0L) x 8
      let success_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (success_get x)
      let success_set x v =
        BA_.set_int64 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=16} ~default:(0L) x 8 v
      let success_set_int x v = success_set x (Int64.of_int v)
      let failure_get x =
        BA_.get_int64 ~default:(0L) x 8
      let failure_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (failure_get x)
      let failure_set x v =
        BA_.set_int64 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=16} ~default:(0L) x 8 v
      let failure_set_int x v = failure_set x (Int64.of_int v)
      type unnamed_union_t =
        | Success of int64
        | Failure of int64
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 16 with
        | 0 -> Success (success_get x)
        | 1 -> Failure (failure_get x)
        | v -> Undefined v
      let term_get x =
        BA_.get_int64 ~default:(0L) x 0
      let term_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (term_get x)
      let term_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let term_set_int x v = term_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:3 ~pointer_words:0 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:3 ~pointer_words:0 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:3 ~pointer_words:0
    end
    module ClientRequest = struct
      type struct_t = [`ClientRequest_eb238de24963377c]
      type t = struct_t builder_t
      let read_get x = ()
      let read_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=8} x
      let has_write x =
        BA_.has_field x 1
      let write_get x =
        BA_.get_blob ~default:"" x 1
      let write_set x v =
        BA_.set_blob ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=8} x 1 v
      type unnamed_union_t =
        | Read
        | Write of string
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 8 with
        | 0 -> Read
        | 1 -> Write (write_get x)
        | v -> Undefined v
      let id_get x =
        BA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let id_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let id_set_int x v = id_set x (Int64.of_int v)
      let has_key x =
        BA_.has_field x 0
      let key_get x =
        BA_.get_blob ~default:"" x 0
      let key_set x v =
        BA_.set_blob x 0 v
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:2 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:2 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:2
    end
    module ClientResponse = struct
      type struct_t = [`ClientResponse_94fe3f4a27ce94b8]
      type t = struct_t builder_t
      let success_get x = ()
      let success_set x =
        BA_.set_void ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=8} x
      let has_read_success x =
        BA_.has_field x 0
      let read_success_get x =
        BA_.get_blob ~default:"" x 0
      let read_success_set x v =
        BA_.set_blob ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=8} x 0 v
      let failure_get x = ()
      let failure_set x =
        BA_.set_void ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=8} x
      type unnamed_union_t =
        | Success
        | ReadSuccess of string
        | Failure
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 8 with
        | 0 -> Success
        | 1 -> ReadSuccess (read_success_get x)
        | 2 -> Failure
        | v -> Undefined v
      let id_get x =
        BA_.get_int64 ~default:(0L) x 0
      let id_get_int_exn x =
        Capnp.Runtime.Util.int_of_int64_exn (id_get x)
      let id_set x v =
        BA_.set_int64 ~default:(0L) x 0 v
      let id_set_int x v = id_set x (Int64.of_int v)
      let of_message x = BA_.get_root_struct ~data_words:2 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:2 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:2 ~pointer_words:1
    end
    module ServerMessage = struct
      type struct_t = [`ServerMessage_d297ccb012332ee0]
      type t = struct_t builder_t
      let has_request_vote x =
        BA_.has_field x 0
      let request_vote_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:0 x 0
      let request_vote_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 v
      let request_vote_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:0 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let request_vote_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:0 ~discr:{BA_.Discr.value=0; BA_.Discr.byte_ofs=0} x 0
      let has_request_vote_resp x =
        BA_.has_field x 0
      let request_vote_resp_get x =
        BA_.get_struct ~data_words:3 ~pointer_words:1 x 0
      let request_vote_resp_set_reader x v =
        BA_.set_struct ~data_words:3 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 v
      let request_vote_resp_set_builder x v =
        BA_.set_struct ~data_words:3 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let request_vote_resp_init x =
        BA_.init_struct ~data_words:3 ~pointer_words:1 ~discr:{BA_.Discr.value=1; BA_.Discr.byte_ofs=0} x 0
      let has_append_entries x =
        BA_.has_field x 0
      let append_entries_get x =
        BA_.get_struct ~data_words:4 ~pointer_words:1 x 0
      let append_entries_set_reader x v =
        BA_.set_struct ~data_words:4 ~pointer_words:1 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 v
      let append_entries_set_builder x v =
        BA_.set_struct ~data_words:4 ~pointer_words:1 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let append_entries_init x =
        BA_.init_struct ~data_words:4 ~pointer_words:1 ~discr:{BA_.Discr.value=2; BA_.Discr.byte_ofs=0} x 0
      let has_append_entries_resp x =
        BA_.has_field x 0
      let append_entries_resp_get x =
        BA_.get_struct ~data_words:3 ~pointer_words:0 x 0
      let append_entries_resp_set_reader x v =
        BA_.set_struct ~data_words:3 ~pointer_words:0 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 v
      let append_entries_resp_set_builder x v =
        BA_.set_struct ~data_words:3 ~pointer_words:0 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let append_entries_resp_init x =
        BA_.init_struct ~data_words:3 ~pointer_words:0 ~discr:{BA_.Discr.value=3; BA_.Discr.byte_ofs=0} x 0
      let has_client_request x =
        BA_.has_field x 0
      let client_request_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:2 x 0
      let client_request_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 v
      let client_request_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let client_request_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:2 ~discr:{BA_.Discr.value=4; BA_.Discr.byte_ofs=0} x 0
      let has_client_response x =
        BA_.has_field x 0
      let client_response_get x =
        BA_.get_struct ~data_words:2 ~pointer_words:1 x 0
      let client_response_set_reader x v =
        BA_.set_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 v
      let client_response_set_builder x v =
        BA_.set_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0 (Some v)
      let client_response_init x =
        BA_.init_struct ~data_words:2 ~pointer_words:1 ~discr:{BA_.Discr.value=5; BA_.Discr.byte_ofs=0} x 0
      type unnamed_union_t =
        | RequestVote of RequestVote.t
        | RequestVoteResp of RequestVoteResp.t
        | AppendEntries of AppendEntries.t
        | AppendEntriesResp of AppendEntriesResp.t
        | ClientRequest of ClientRequest.t
        | ClientResponse of ClientResponse.t
        | Undefined of int
      let get x =
        match BA_.get_uint16 ~default:0 x 0 with
        | 0 -> RequestVote (request_vote_get x)
        | 1 -> RequestVoteResp (request_vote_resp_get x)
        | 2 -> AppendEntries (append_entries_get x)
        | 3 -> AppendEntriesResp (append_entries_resp_get x)
        | 4 -> ClientRequest (client_request_get x)
        | 5 -> ClientResponse (client_response_get x)
        | v -> Undefined v
      let of_message x = BA_.get_root_struct ~data_words:1 ~pointer_words:1 x
      let to_message x = x.BA_.NM.StructStorage.data.MessageWrapper.Slice.msg
      let to_reader x = Some (RA_.StructStorage.readonly x)
      let init_root ?message_size () =
        BA_.alloc_root_struct ?message_size ~data_words:1 ~pointer_words:1 ()
      let init_pointer ptr =
        BA_.init_struct_pointer ptr ~data_words:1 ~pointer_words:1
    end
  end

  module Client = struct
  end

  module Service = struct
  end
  module MessageWrapper = MessageWrapper
end [@@inline]

module Make(M:Capnp.MessageSig.S) = MakeRPC[@inlined](Capnp.RPC.None(M)) [@@inline]
