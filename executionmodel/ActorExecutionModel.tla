---- MODULE ActorExecutionModel ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorExecutionModel describes combines all other Modules and
\* defines the actual Specification and Theorems

EXTENDS ActorEvents

Init == /\ ActorsInit
        /\ MessagesInit

Invariant == /\ ActorIDsInvariant
             /\ MessageQueueLengthInvariant

Next == /\ PrintT("==== Next State ====")
        /\ PrintT("---- messages:")
        /\ PrintT(messages)
        /\ PrintT("---- actors:")
        /\ PrintT(actors)
        /\ \/ DeliverNextMessage
           \/ \E id \in ActiveActorIDs :
              ActorEvent(id)
        /\ CleanUnusedActors

Spec == Init /\[][Next]_<<actors, messages>>

THEOREM Spec => /\ []Invariant
                /\ []ActorsInvariant

====
