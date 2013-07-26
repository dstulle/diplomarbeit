---- MODULE ActorMessages ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorMessages describes the creation and deliveriy of messages.

EXTENDS ActorEnvironment

NoMessage == EmptyBag

MessagesInit == messages = NoMessage

----

RemainingMessageCapacity == MaximumLengthOfMessageQueue - BagCardinality(messages)

LOCAL MessageTemplate(messageInit) == ([name |-> messageInit.name,
                                  body |-> messageInit.body,
                                  destination |-> messageInit.destination]
                                 :> messageInit.amount )

LOCAL CreateMessage(messageInit) == IF BagCardinality(messages) < MaximumLengthOfMessageQueue
                                THEN MessageTemplate(messageInit)
                                ELSE NoMessage

CreateMessages(messageInitSet) ==
   LET CM[set \in SUBSET messageInitSet] ==
       IF Cardinality(set) = 0
       THEN NoMessage
       ELSE LET mi == CHOOSE mi \in messageInitSet : TRUE
            IN CreateMessage(mi) (+)
               CM[set \ {mi}]
   IN CM[messageInitSet]

DeliverNextMessage ==
   /\ BagCardinality(messages) > 0
   /\ LET message == CHOOSE mi \in BagToSet(messages) : TRUE
      IN /\ messages' = TakeFromBag(message,messages)
         /\ actors' = [actors
                       EXCEPT ![message.destination].inbox
                              = Append(actors[message.destination].inbox,
                                       ReduceFunction(message,"destination"))]

MessageQueueLengthInvariant == BagCardinality(messages) <= MaximumLengthOfMessageQueue

====
