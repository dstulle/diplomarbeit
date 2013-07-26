---- MODULE ActorLifecycle ----
\****
\* This file is part of the general actor behavior.
\* The MODULE ActorLifecycle describes the Creation of Actors and handling of
\* obsolete Actors wich are no longer references by other Actors and wich ids
\* can be reused for new actors

EXTENDS Actors

LOCAL NoActor == EmptyFunction \* represents a case in which no actor is created.
----

LOCAL ActorTemplate(type, state) ==
   [type |-> type,
    state |-> state,
    inbox |-> <<>>]

CreateActor(actorInit) ==
   LET initState == CHOOSE initState \in PossibleState(actorInit.type) :
                       /\ InitPredicate(actorInit.type, initState)
                       /\ IsPartialFunctionOf(actorInit.state, initState)
   IN ActorTemplate(actorInit.type, initState)

CreateActors(actorsInitSet) ==
   LET CA[set \in SUBSET actorsInitSet] ==
        IF Cardinality(set) = 0
        THEN NoActor
        ELSE LET ai == CHOOSE ai \in actorsInitSet : TRUE
             IN ExtendFunction(
                   CA[set \ {ai}],
                   ai.id,
                   CreateActor(ai))
   IN CA[actorsInitSet]

ActorsInit == actors = CreateActors({[id |-> EnvironmentActorID,
                                     type |-> "Environment",
                                     state |-> Stateless]})

CleanUnusedActors == TODO("declare when aqaintences are defined")

ActorIDsInvariant == /\ EnvironmentActorID \in (DOMAIN actors)
                     /\ ActiveActorIDs \cap FreeIDs = {}
                     /\ ActiveActorIDs \cup FreeIDs = ActorIDs

====
