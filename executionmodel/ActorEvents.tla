---- MODULE ActorEvents ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorEnvironment describes the Activities and Operations as Events.
\* All Possible new messages, states and creations are generated and checked against
\* the given properties of the actors.

EXTENDS ActorLifecycle,
        ActorMessages

LOCAL ActorsWithReplacedState(as,id,state) ==
   [as EXCEPT ![id].state = CombineFunctions(state,as[id].state)]

LOCAL EventHelper(gActors, type, kind, name, id, post, new) ==
   \/ /\ Event_Predicate(type, kind, name, id, post, new, {}, StateOf(id))
      /\ actors' = CombineFunctions(ActorsWithReplacedState(gActors, id, post),
                                    CreateActors(new))
      /\ UNCHANGED<<messages>>
   \/ \E newMessage \in PossibleNewMessages:
         /\ Event_Predicate(type, kind, name, id, post, new, {newMessage}, StateOf(id))
         /\ actors' = CombineFunctions(ActorsWithReplacedState(gActors, id, post),
                                    CreateActors(new))
         /\ messages' = messages (+) CreateMessages({newMessage})
\****
\* This should enable the sending of more than one message in one step, but has not been tested do far
\* WARNING: this could take TLC a very, very long time
(*
   \/ \E newMessages \in SUBSET PossibleNewMessages:
         /\ Event_Predicate(type, kind, name, id, post, new, {newMessage}, StateOf(id))
         /\ actors' = CombineFunctions(ActorsWithReplacedState(gActors, id, post),
                                    CreateActors(new))
         /\ messages' = messages (+) CreateMessages({newMessage})
*)

LOCAL Event(gActors, kind, type, name, id) ==
/\ actors[id].type = type
/\ Event_Condition(kind, type, name, StateOf(id))
/\ \E post \in PossibleState(type):
   /\ \/ EventHelper(gActors, type, kind, name, id, post, {})
      \/ \E newActor \in PossibleNewActors :
         EventHelper(gActors, type, kind, name, id, post, {newActor})
\****
\* This should enable the creation of more than one actor in one step, but has not been tested so far
\* WARNING: this could take TLC a very, very long time
(*
      \/ \E newActors \in SUBSET PossibleNewActors : 
         EventHelper(gActors, type, kind, name, id, post, newActors)
*)

----
(* Activities *)

Activity(type, name, id) ==
   Event(actors, "activity", type, name, id)

----
(* Operations *)

LOCAL ActorsWithFirstMessageRemoved(as,id) ==
   [as EXCEPT ![id].inbox = Tail(as[id].inbox)]

LOCAL HasMessagePending(id) == Len(actors[id].inbox) > 0

Operation(type, name, id) ==
   /\ HasMessagePending(id)
   /\ Event(ActorsWithFirstMessageRemoved(actors,id), "operation", type, name, id)

----

ActorEvent(id) ==
   \E type \in ActorTypes :
      \/ \E activity \in ActorActivities(type) :
         Activity(type,activity,id)
      \/ \E operation \in ActorOperations(type) :
         Operation(type,operation,id)

====
