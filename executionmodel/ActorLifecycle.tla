---- MODULE ActorLifecycle ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorLifecycle describes the Creation of Actors and handling of
\* obsolete Actors wich are no longer references by other Actors and wich ids
\* can be reused for new actors

EXTENDS Actors

----

\* AcquaintanceTree(id:ActorID) : Set<<ActorID>>
LOCAL AcquaintanceTree(actorId) ==                      \****
   LET AT[id \in ActorIDs, used \in SUBSET ActorIDs] == \* Set of all direct and indirect Acquaintances of the Actor with the given ID
        Acquaintances(id) \cup
        UNION {AT[x, used \cup
        {id}] : x \in (Acquaintances(id) \ used)}
   IN {actorId} \cup AT[actorId,{}]

\* ReferencedActorIDs : Set<<ActorID>>
LOCAL ReferencedActorIDs ==             \****
   AcquaintanceTree(EnvironmentActorID) \* Set of all direct and indirect Acquantanves of the EnvironmentActor

\* ActorIDsWithPendingMessage : Set<<ActorID>>
LOCAL ActorIDsWithPendingMessage ==                \****
   {id \in ActiveActorIDs : HasMessagePending(id)} \* Set of all ActorIDs with an unevaluated Message

\* ActorIDsWithEnabledEvent : Set<<ActorID>>
LOCAL ActorIDsWithEnabledEvent == \****
   {id \in ActiveActorIDs :       \* Set of all ActorIDs with at least one enabled event
      \E event \in ActorEvents(TypeOf(id)) :
         Action_Condition("event", TypeOf(id), event, StateOf(id),
                          EmptyFunction)
   }

\* ObsoleteActorIDs : Set<<ActorID>>
LOCAL ObsoleteActorIDs ==                            \****
   ActiveActorIDs \ (ReferencedActorIDs \cup         \* IDs of all Active Actors that are not references, have no pending message and have no enabled event.
                     ActorIDsWithPendingMessage \cup \*
                     ActorIDsWithEnabledEvent)

----

\* CreateActor(actorInit : [type |-> string, state |-> [string -> AnyType]]]) :
\*    [type |-> string, state |-> [string -> AnyType], inbox |-> <<>>]
LOCAL CreateActor(actorInit) ==
   LET initState == CHOOSE initState \in PossibleState(actorInit.type) :
                       /\ InitPredicate(actorInit.type, initState)
                       /\ IsPartialFunctionOf(actorInit.state, initState)
   IN [type |-> actorInit.type, state |-> initState]

\* CreateActor(actorInitSet : {[type |-> string, state |-> [string -> AnyType]]]}) :
\*    [ActorIDs -> [type |-> string, state |-> [string -> AnyType], inbox |-> <<>>]
CreateActors(actorsInitSet) ==             \****
   LET CA[set \in SUBSET actorsInitSet] == \* Creates actors according to given set.
        IF Cardinality(set) = 0
        THEN EmptyFunction \* No Actor is created
        ELSE LET ai == CHOOSE ai \in set : TRUE
             IN ExtendFunction(
                   CA[set \ {ai}],
                   ai.id,
                   CreateActor(ai))
   IN CA[actorsInitSet]

----

\* CleanUpActors : boolean
CleanUpActors == /\ actors' = ReduceFunction(actors, ObsoleteActorIDs)
                 /\ UNCHANGED<<messages>>

----

\* ActorsInit : boolean
ActorsInit == actors = CreateActors({[id |-> EnvironmentActorID,
                                     type |-> "Environment",
                                     state |-> EmptyFunction]})

====
