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

module MakeRPC(MessageWrapper : Capnp.RPC.S) : sig
  include S with module MessageWrapper = MessageWrapper

  module Client : sig
  end

  module Service : sig
  end
end

module Make(M : Capnp.MessageSig.S) : module type of MakeRPC(Capnp.RPC.None(M))
