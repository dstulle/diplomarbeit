---- MODULE BufferActor ----
\****
\* This file is automatically generated from the buffer.ral

EXTENDS ActorEnvironment

----
\****
\* Constants

CONSTANTS BufferActorConstant_limit

ASSUME /\ BufferActorConstant_limit \in NatType


ASSUME BufferActorConstant_limit > 0

----
\****
\* Variables

Buffer_PossibleState ==
   [data : SeqType(NatType)]

Buffer_PossibleNewActors ==
   [type : {"Buffer"},
    state : Buffer_PossibleState,
    id : ActorIDs]

Buffer_Invariant(state) ==
   /\ state \in Buffer_PossibleState
   /\ Len(state.data) <= BufferActorConstant_limit#!void

----
\****
\* Public State

Buffer_PublicState(state) == TRUE \* TODO

----
\****
\* Acquaintances

Buffer_Acquaintances(state) == 
   {}

----
\****
\* Init Predicate

Buffer_InitPredicate(state) ==
   state.data = << >>

----
\****
\* Operations

Buffer_Operations ==
   {"write", "read"}

LOCAL Buffer_OperationWrite_Condition(state, message) ==
   Len(state.data) < BufferActorConstant_limit#!void

LOCAL Buffer_OperationWrite_PossibleNewActors(id, post, pre, message) ==
   {}

LOCAL Buffer_OperationWrite_PossibleNewMessages(id, post, pre, message) ==
   {}

LOCAL Buffer_OperationWrite_Predicate(id, post, new, out, pre, message) ==
   /\ post.data = Append(pre.data, message.body.val)
   /\ new = {}
   /\ out = {}

LOCAL Buffer_OperationRead_Condition(state, message) ==
   Len(state.data) >= 0

LOCAL Buffer_OperationRead_PossibleNewActors(id, post, pre, message) ==
   {}

LOCAL Buffer_OperationRead_PossibleNewMessages(id, post, pre, message) ==
   {[name |-> "buffer-value",
              destination |-> message.body.cons,
              body |-> [buf |-> id,
                        value |-> pre.head],
              amount |-> 1][name |-> "buffer-is-empty",
              destination |-> message.body.cons,
              body |-> [buf |-> id],
              amount |-> 1]}

LOCAL Buffer_OperationRead_Predicate(id, post, new, out, pre, message) ==
   \/ /\ Len(pre.data) > 0
      /\ new = {}
      /\ out = {[name |-> "buffer-value",
              destination |-> message.body.cons,
              body |-> [buf |-> id,
                        value |-> pre.head],
              amount |-> 1]}
      /\ post.data = Tail(pre.data)
   \/ /\ Len(pre.data) = 0
      /\ new = {}
      /\ out = {[name |-> "buffer-is-empty",
              destination |-> message.body.cons,
              body |-> [buf |-> id],
              amount |-> 1]}
      /\ post.data = pre.data


LOCAL Buffer_Operation_Condition(name, state, message) ==
   CASE name = "write" ->
      Buffer_OperationWrite_Condition(state, message)
     [] name = "read" ->
      Buffer_OperationRead_Condition(state, message)

LOCAL Buffer_Operation_PossibleNewActors(name, id, post, pre, message) ==
   CASE name = "write" ->
      Buffer_OperationWrite_PossibleNewActors(id, post, pre, message)
     [] name = "read" ->
      Buffer_OperationRead_PossibleNewActors(id, post, pre, message)

LOCAL Buffer_Operation_PossibleNewMessages(name, id, post, pre, message) ==
   CASE name = "write" ->
      Buffer_OperationWrite_PossibleNewMessages(id, post, pre, message)
     [] name = "read" ->
      Buffer_OperationRead_PossibleNewMessages(id, post, pre, message)

LOCAL Buffer_Operation_Predicate(name, id, post, new, out, pre, message) ==
   CASE name = "write" ->
      Buffer_OperationWrite_Predicate(id, post, new, out, pre, message)
     [] name = "read" ->
      Buffer_OperationRead_Predicate(id, post, new, out, pre, message)


----
\****
\* Events

Buffer_Events ==
   {"none"}

LOCAL Buffer_EventNone_Condition(state) ==
   FALSE

LOCAL Buffer_EventNone_PossibleNewActors(id, post, pre) ==
   {}

LOCAL Buffer_EventNone_PossibleNewMessages(id, post, pre) ==
   {}

LOCAL Buffer_EventNone_Predicate(id, post, new, out, pre) ==
   FALSE


LOCAL Buffer_Event_Condition(name, state) ==
   CASE name = "none" ->
      Buffer_EventNone_Condition(state)

LOCAL Buffer_Event_PossibleNewActors(name, id, post, pre) ==
   CASE name = "none" ->
      Buffer_EventNone_PossibleNewActors(id, post, pre)

LOCAL Buffer_Event_PossibleNewMessages(name, id, post, pre) ==
   CASE name = "none" ->
      Buffer_EventNone_PossibleNewMessages(id, post, pre)

LOCAL Buffer_Event_Predicate(name, id, post, new, out, pre) ==
   CASE name = "none" ->
      Buffer_EventNone_Predicate(id, post, new, out, pre)


----

Buffer_Action_Condition(kind, name, state, message) ==
   IF kind = "event"
   THEN
      Buffer_Event_Condition(name, state)
   ELSE \* kind = "operation"
      Buffer_Operation_Condition(name, state, message)

Buffer_Action_PossibleNewActors(kind, name, id, post, pre, message) ==
   IF kind = "event"
   THEN
      Buffer_Event_PossibleNewActors(name, id, post, pre)
   ELSE \* kind = "operation"
      Buffer_Operation_PossibleNewActors(name, id, post, pre, message)

Buffer_Action_PossibleNewMessages(kind, name, id, post, pre, message) ==
   IF kind = "event"
   THEN
      Buffer_Event_PossibleNewMessages(name, id, post, pre)
   ELSE \* kind = "operation"
      Buffer_Operation_PossibleNewMessages(name, id, post, pre, message)

Buffer_Action_Predicate(kind, name, id, post, new, out, pre, message) ==
   IF kind = "event"
   THEN
      Buffer_Event_Predicate(name, id, post, new, out, pre)
   ELSE \* kind = "operation"
      /\ name = message.name
      /\ Buffer_Operation_Predicate(name, id, post, new, out, pre, message)

====
