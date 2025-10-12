import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub type NodeRef =
  Subject(Msg)

pub type Msg {
  JoinRing(NodeRef)
  FindSuccessor(key: Int, reply_to: NodeRef, hops: Int)
  FoundSuccessor(key: Int, succ_ref: NodeRef, succ_id: Int, hops: Int)
  Stabilize
  GetPredecessor(reply_to: NodeRef)
  ReplyPredecessor(pred: Option(NodeRef), pred_id: Option(Int))
  Notify(n: NodeRef, n_id: Int)
  FixFingers(i: Int)
  CheckPredecessor
  BeginLookups(num: Int)
  LookupKey(key: Int, req_id: Int)
  Tick
}

pub type Purpose {
  User
  Maint
  Join
}

// -------------------
// Helpers
// -------------------

pub fn ipow(base: Int, exp: Int) -> Int {
  case exp {
    0 -> 1
    _ -> base * ipow(base, exp - 1)
  }
}

pub fn finger_start(n: Int, i: Int, m: Int) -> Int {
  let size_val = ipow(2, m)
  let base_offset = ipow(2, i - 1)
  let _offset = base_offset * n % size_val
}
