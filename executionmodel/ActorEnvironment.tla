---- MODULE ActorEnvironment ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorEnvironment describes the Environment of the Actor Execution
\* Model. Besides the declaration of constants and variables this includes
\* anything dealing with data types, shortcuts to some actor properties and
\* anything related to ActorIDs.

EXTENDS Utils, Naturals, Sequences, Bags, FiniteSets, TLC

CONSTANTS ActorIDs, \****
                    \* The set of actor identifiers
          MaximumSizeOfMessageQueues, \****
                                      \* The Maximum length of the global message 
          FNat, \****
                \* A finite Set of natural Numbers to restrain the Exploration.
          PossibleLengthOfSequences \****
                                    \* The Possible length of any Sequence used by the Actors.

VARIABLES actors, \****
                  \* A function to reference actors (ActorID -> Actor)
                  \*
                  \* [ActorIDs -> [type |-> string,
                  \*                       state |-> [string -> AnyType]]

          messages \****
                   \* A function (ActorID x ActorID -> Seq(Bag(message)))
                   \* containing messages to be send to the Actors
                   \*
                   \* ([name |-> string,
                   \*    body |-> [string -> AnyType],
                   \*    destination -> ActorIDs] -> Nat)

ASSUME /\ ActorIDs \subseteq Nat 
       /\ FNat \subseteq Nat
       /\ ActorIDs \cap FNat = {}
       /\ MaximumSizeOfMessageQueues \in Nat
       /\ MaximumSizeOfMessageQueues >= 0

----
\****
\* Available types for actor variables.

NatType == FNat
BoolType == {TRUE, FALSE}
RefType == ActorIDs
SeqType(x) == UNION {[1 .. n -> x] : n \in PossibleLengthOfSequences }

AnyNat == CHOOSE nat \in NatType : TRUE
AnyRef == CHOOSE id \in RefType : TRUE
AnyBool == CHOOSE bool \in BoolType : TRUE
AnySeq(x) == CHOOSE seq \in SeqType(x) : TRUE

----
\****
\* Shortcuts to the Type and State of an actor.

\* TypeOf(id:RefType) : string
TypeOf(id) == actors[id].type

\* StateOf(id:RefType) : [string -> {NatType, BoolType, RefType}]
StateOf(id) == actors[id].state


----
\****
\* Message related operations

\* HasMessagePending(id:RefType) : boolean
HasMessagePending(targetID) == \****
   \E sourceID \in ActorIDs :  \* TRUE if the Actor of the given ID has at least one Message to be evaluated, otherwise FALSE 
      /\ <<sourceID, targetID>> \in DOMAIN messages
      /\ BagCardinality(Head(messages[<<sourceID, targetID>>])) > 0

----
\****
\* Actor IDs

\* ActiveActorIDs : Set<<RefType>>
ActiveActorIDs == DOMAIN actors \****
                                \* A set of actor IDs that are currently in use.

\* ActiveActorIDsByType(type:string) : Set<<RefType>>
ActiveActorIDsByType(type) ==                  \****
   {id \in ActiveActorIDs : TypeOf(id) = type} \* a set of actor IDs of a certain kind of actor-types that are currently active

\* FreeIDs : Set<<RefType>>
LOCAL FreeIDs == \****
           \* a set of actor IDs that are currently not in use.
   ActorIDs \ (DOMAIN actors)

\* EnvironmentActorID : RefType
EnvironmentActorID == \****
                      \* The Id of the EnvironmentActor.
   CHOOSE id \in ActorIDs: TRUE

\* NFreeIDs(n:Nat) : Set<<RefType>>
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

\* NextFreeID(n:Nat) : RefType
NextFreeID(n) ==                                  \****
   CHOOSE id \in FreeIDs: id \notin NFreeIDs(n-1) \* the `^$n^{th}$^' next free ID if there is one (the @ predicate in ReActor)

----

ActorIDsInvariant == /\ EnvironmentActorID \in (DOMAIN actors)
                     /\ ActiveActorIDs \cap FreeIDs = {}
                     /\ ActiveActorIDs \cup FreeIDs = ActorIDs

====
