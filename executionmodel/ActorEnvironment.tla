---- MODULE ActorEnvironment ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorEnvironment describes the Environment of the Actor Execution
\* Model. Besides the declaration of constants and variables this includes
\* anything dealing with data types, shortcuts to some actor properties and
\* anything related to ActorIDs

EXTENDS Utils, Naturals, Sequences, Bags, FiniteSets, TLC

CONSTANT ActorIDs, \* The set of actor identifiers
         MaximumLengthOfMessageQueue, \****
                                      \* The Maximum length of the message queue
         FNat \* A finite Set of natural Numbers to restrain the Exploration.

VARIABLE actors, \* A function to reference actors (ActorID -> Actor)
         messages \* A bag of messages to be send to the Actors

ASSUME /\ \A x \in ActorIDs : x \in Nat
       /\ MaximumLengthOfMessageQueue \in Nat
       /\ MaximumLengthOfMessageQueue >= 0
       /\ FNat \subseteq Nat

----
(* Available types for actor variables *)

NatType == FNat
BoolType == {TRUE, FALSE}
RefType == ActorIDs

AnyNat == CHOOSE nat \in NatType : TRUE
AnyRef == CHOOSE id \in RefType : TRUE
AnyBool == CHOOSE bool \in BoolType : TRUE

----
(* Shortcuts to the Type and State of an actor *)
TypeOf(id) == actors[id].type

StateOf(id) == actors[id].state

----
(* Shortcuts to the name and the body of a the next pending message *)
NextMessageName(id) == Head(actors[id].inbox).name

NextMessageBody(id) == Head(actors[id].inbox).body

----

Stateless == EmptyFunction \* Describes the state of an actor with no state.

EmptyMessageBody == EmptyFunction \****
                                  \* Describes the body of a message wich is empty.

----
(* Actor IDs *)

ActiveActorIDs == \* a set of actor IDs that are currently in use.
   DOMAIN actors

ActiveActorIDsByType(type) == \****
                              \* a set of actor IDs of a certain kind of actor-types that are currently active
   {id \in ActiveActorIDs : TypeOf(id) = type}

FreeIDs == \* a set of actor IDs that are currently not in use.
   ActorIDs \ (DOMAIN actors)

EnvironmentActorID ==  \* The Id of the EnvironmentActor.
   CHOOSE id \in ActorIDs: TRUE

LOCAL NFreeIDs(n) == \****
                     \* a set of n free IDs: NFreeIDs(n) \subset NFreeIDs(n+1).
   IF Cardinality(FreeIDs) >= n
   THEN LET FI[x \in 0..Cardinality(FreeIDs)] ==
              IF x = 0
                 THEN {}
                 ELSE {CHOOSE id \in FreeIDs: id \notin FI[x-1]} \cup
                      FI[x-1]
        IN FI[n]
   ELSE {}

NextFreeID(n) == \****
                 \* the nth next free ID if there is such an IDs otherwise FALSE (the @ predicate in ReActor)
   CHOOSE id \in FreeIDs: id \notin NFreeIDs(n-1)

====
