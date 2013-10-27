---- MODULE ActorMessages ----
\****
\* This file is part of the general actor behavior.
\*
\* The MODULE ActorMessages describes the creation and deliveriy of messages.

EXTENDS ActorBase

----

\* CreateMessage(messageInit : [name |-> string,
\*                                              body |-> [string -> AnyType],
\*                                              destination -> ActorIDs,
\*                                              amount -> Nat]) :
\* ([name |-> string, body |-> [string -> AnyType], destination -> ActorIDs] :> Nat)
LOCAL CreateMessage(messageInit) ==
   (messageInit.destination :> ([name |-> messageInit.name,
                     body |-> messageInit.body]
                    :> messageInit.amount ))

\* CreateMessages(messagesInitSet : {[name |-> string,
\*                                                      body |-> [string -> AnyType],
\*                                                      destination -> ActorIDs,
\*                                                      amount -> Nat])} :
\* (destination :> ([name |-> string, body |-> [string -> AnyType]] :> Nat))
CreateMessages(messagesInitSet) ==
   LET CM[set \in SUBSET messagesInitSet] ==
       IF Cardinality(set) = 0
       THEN EmptyFunction \* No Message is send
       ELSE LET mi == CHOOSE mi \in set : TRUE
            IN  MergeFunctionsWithBags(CreateMessage(mi),
                                       CM[set \ {mi}])
   IN CM[messagesInitSet]

----

\* MaxLenOfMessageQueue(msgsBuffer : [ActorIDs \X ActorIDs -> Seq(Bag(Message))]) :
\* Nat
MaxLenOfMessageQueue(msgsBuffer) ==               \****
   LET MLOMS[traces \in SUBSET DOMAIN msgsBuffer, \*  The Length of the longest Sequence of any message Queue
             length \in Nat] == 
      IF traces = {}
      THEN length
      ELSE LET trace == CHOOSE trace \in traces : TRUE
           IN IF Len(msgsBuffer[trace]) > length
              THEN MLOMS[traces \ {trace}, Len(msgsBuffer[trace])]
              ELSE MLOMS[traces \ {trace}, length]
   IN MLOMS[DOMAIN msgsBuffer, 0]

----

\* MessagesInit : boolean
MessagesInit == messages = EmptyFunction

\* MessageQueueLenInvariant : boolean
MessageQueueLenInvariant ==
   MaxLenOfMessageQueue(messages) <= MaximumSizeOfMessageQueues

====
