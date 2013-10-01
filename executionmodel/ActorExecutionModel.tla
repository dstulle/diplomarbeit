---- MODULE ActorExecutionModel ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorExecutionModel describes combines all other Modules and
\* defines the actual Specification and Theorems

EXTENDS ActorActions

VARIABLE cycle

CycleInvariant == cycle \in {"action", "cleanup"}

Init == /\ ActorsInit
        /\ MessagesInit
        /\ cycle = "action"

Invariant == /\ ActorIDsInvariant
             /\ ActorsInvariant
             /\ MessageQueueLenInvariant
             /\ CycleInvariant

PrintSystemState == /\ PrintT("==== Next State ====")
                    /\ PrintT("---- messages:")
                    /\ PrintT(messages)
                    /\ PrintT("---- actors:")
                    /\ PrintT(actors)

Next == \/ /\ cycle = "action"
           /\ PrintSystemState
           /\ \E id \in ActiveActorIDs :
               ActorAction(id)
           /\ cycle' = "cleanup"
        \/ /\ cycle = "cleanup"
           /\ CleanUpActors
           /\ cycle' = "action"

Spec == Init /\[][Next]_<<actors, messages, cycle>> /\ <> (StateOf(EnvironmentActorID).result = "asdf")

THEOREM Spec => /\ []Invariant
                /\ []ActorsInvariant

====
