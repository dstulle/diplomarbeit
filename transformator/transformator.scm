#!../gambc-v4_6_9/bin/gsi-script -:s

(include "utils.scm")
(include "display-helper.scm")

;--------------------------------------------------------

; defines the current ReActor Actor-Class to be red.
(define file-to-read (car (cdr (command-line))))

; defines the current ReActor Actor-Class to be red.
(define class-to-read (car (cdr (cdr (command-line)))))

; reads the input file
(define read-spec
  (call-with-input-file file-to-read
    (lambda (p)
      (read p))))

; The Classname of current Actor to read convertet into a format used by tla+
(define ClassName (string-capitalize class-to-read))

;--------------------------------------------------------
; Syntax definitions for all high level symbols found in ReActor
;--------------------------------------------------------

(define-syntax class
  (syntax-rules () 
    ((actor-class name body ...)
     (actor-class->tla 'name '(body ...)))))

(define-syntax constants
  (syntax-rules ()
    ((constants body ...)
     (constants->tla '(body ...)))))

(define-syntax constant
  (syntax-rules ()
    ((constant body ...)
     (constants->tla '(body ...)))))

(define-syntax assume
  (syntax-rules ()
    ((assume body ...)
     (assume->tla '(body ...)))))

(define-syntax acquaintances
  (syntax-rules ()
    ((acquaintances body ...)
     (acquaintances->tla '(body ...)))))

(define-syntax public-state
  (syntax-rules ()
    ((public-state body ...)
     (public-state->tla '(body ...)))))

(define-syntax variables
  (syntax-rules ()
    ((variables body ...)
     (variables->tla '(body ...)))))

(define-syntax variable
  (syntax-rules ()
    ((variable body ...)
     (variables->tla '(body ...)))))

(define-syntax invariant
  (syntax-rules ()
    ((invariant body ...)
     (invariant->tla '(body ...)))))

(define-syntax init-predicate
  (syntax-rules ()
    ((init-predicate body ...)
     (init-predicate->tla '(body ...)))))

(define-syntax events
  (syntax-rules ()
    ((events body ...)
     (events->tla '( body ...)))))

(define-syntax event
  (syntax-rules ()
    ((event body ...)
     (event->tla '(body ...)))))

(define-syntax operations
  (syntax-rules ()
    ((operations body ...)
     (operations->tla '(body ...)))))

(define-syntax operation
  (syntax-rules ()
    ((operation body ...)
     (operation->tla '( body ...)))))

;--------------------------------------------------------
; GENERAL
;--------------------------------------------------------

(define (type->tla type)
  (cond
    ((eq? type 'nat) "NatType")
    ((eq? type 'bool) "BoolType")
    ((eq? type 'ref) "RefType")
    ((eq? (car type) 'seq) (string-join "SeqType(" (string-join (type->tla (car (cdr type))) ")")))
    (else #f))) ;TODO some kind of exception would be nice here

(define (any type)
  (cond
    ((eq? type 'nat) "AnyNat")
    ((eq? type 'bool) "AnyBool")
    ((eq? type 'ref) "AnyRef")
    ((eq? (car type) 'seq) (string-join "AnySeq(" (string-join (type->tla (car (cdr type))) ")")))
    (else #f))) ;TODO some kind of exception would be nice here

;--------------------------------------------------------
; ELEMENT Expressions
;--------------------------------------------------------

; The element is anything that can be contained in a set.
; So it can be actually anything, a number, a reference, a bool or even a set.
; this display-element is can also be used if it is not entirely clear what
; exeacly can be expected at a certain position of the ReActor specification.
(define (display-element prefix expression)
  (cond
    ((string? expression)
     (display-string prefix expression))
    ((number? expression)
     (display (nat->tla prefix expression)))
    ((boolean? expression)
     (display-bool 0 prefix expression))
    ((eq? expression 'emptyset)
     (display "{ }"))
    ((eq? expression 'empty-sequence)
     (display "<< >>"))
    ((not (list? expression))
      (display prefix)
      (display expression))
    ((eq? (car expression) 'post)
      (display-post (car (cdr expression))))
    ((eq? (car expression) 'const)
      (display (constant-name (car (cdr expression)))))
    ((eq? (car expression) '@)
      (display-reference prefix expression))
    ((eq? (car expression) 'message)
      (display (message-value->tla (car (cdr expression)))))
    ((or (eq? (car expression) 'empty-seq)
         (eq? (car expression) 'enum->seq)
         (eq? (car expression) 'append)
         (eq? (car expression) 'concat)
         (eq? (car expression) 'tail))
      (display (seq->tla prefix expression)))
    ((or (eq? (car expression) '+)
         (eq? (car expression) '-)
         (eq? (car expression) '*)
         (eq? (car expression) '/)
         (eq? (car expression) 'length))
      (display (nat->tla prefix expression)))
    ((or (eq? (car expression) 'enum->set)
         (eq? (car expression) 'set-union)
         (eq? (car expression) 'set-intersection)
         (eq? (car expression) 'set-difference))
      (display-set prefix expression))
    ;TODO other possibilities
    (else
      (display "TODO-element:")
      (display expression))))
      
(define (element->tla prefix expression)
  (cond
    ((number? expression)
     (nat->tla prefix expression))
    ((not (list? expression))
      (string-join prefix expression))
    ((eq? (car expression) 'post)
      (post->tla (car (cdr expression))))
    ((eq? (car expression) 'message)
      (message-value->tla (car (cdr expression))))
    ((or (eq? (car expression) '+)
         (eq? (car expression) '-)
         (eq? (car expression) '*)
         (eq? (car expression) '/))
      (nat->tla prefix expression))
    (else
      (display "TODO-element->tla:")
      (display expression))))

;--------------------------------------------------------
; SET Expressions
;--------------------------------------------------------

(define (display-elements prefix expression)
  (cond ((eq? expression '())
          '())
        ((eq? (cdr expression) '())
         (display-element prefix (car expression)))
        (else
          (display-element prefix (car expression))
          (display ", ")
          (display-elements prefix (cdr expression)))))

(define (display-set prefix expression)
  (cond
    ((not (list? expression))
      (display expression))
    ((eq? (car expression) 'emptyset)
      (display "{}"))
    ((eq? (car expression) 'type)
      (display (type->tla (car (cdr expression)))))
    ((eq? (car expression) 'enum->set)
      (display "{")
      (display-elements prefix (cdr expression))
      (display "}"))
    ((eq? (car expression) 'if)
     (display "IF ")
     (display-bool 0 prefix (car (cdr expression)))
     (display " THEN ")
     (display-set prefix (car (cdr (cdr expression))))
     (display " ELSE ")
     (display-set prefix (car (cdr (cdr (cdr expression)))))
     )
    ;TODO: operations on sets
    (else
      (display "TODO-set:")
      (display expression))
      ))

;--------------------------------------------------------
; STRING Expressions
;--------------------------------------------------------

; This method is actually a placeholder for the case that operations with
; strings are supportet at some time in the future. Currently there are no
; such plans
(define (display-string prefix expression)
  (cond
    ((string? expression)
     (display "\"")
     (display expression)
     (display "\""))
    ((not (list? expression))
      (display "\"")
      (display expression)
      (display "\""))
    ;TODO: operations on strings
    (else
      (display "TODO-string:")
      (display expression))
      ))

;--------------------------------------------------------
; NUMERIC Expressions
;--------------------------------------------------------

(define (nat->tla prefix expression)
  (cond
    ((number? expression)
      (number->string expression))
    ((not (list? expression))
      (string-join prefix (symbol->string expression)))
    ((eq? (car expression) 'message)
      (message-value->tla (car (cdr expression))))
    ((eq? (car expression) 'const)
      (display (constant-name (car (cdr expression)))))
    ((eq? (car expression) '+)
      (string-join "(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
      (string-join " + "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
    ((eq? (car expression) '-)
      (string-join "(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
      (string-join " - "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
    ((eq? (car expression) '*)
      (string-join "(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
      (string-join " * "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
    ((eq? (car expression) 'length)
      (string-join "Len("
      (string-join (seq->tla prefix (car (cdr expression))) 
                   ")")))
    ((eq? (car expression) 'head)
      (string-join "Head("
      (string-join (seq->tla prefix (car (cdr expression))) 
                   ")")))
    ((eq? (car expression) '*)
      (string-join "(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
      (string-join " * "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
      ;TODO: other operations: div mod
    (else
      (string-join "TODO-nat:"
      (number->string expression)))))

;--------------------------------------------------------
; BOOLEAN Expressions
;--------------------------------------------------------

(define (boolean->string bool)
      (if bool
          "TRUE"
          "FALSE"))

(define (display-bool offset prefix expression . tail)
  (tab-feed expression tail offset)
  (cond
    ((boolean? expression)
      (display (boolean->string expression)))
    ((not (list? expression))
      (display prefix)
      (display expression))
    ((eq? (car expression) 'new)
      (display "new = {")
      (display-new-actors (cdr expression))
      (display "}"))
    ((eq? (car expression) 'send)
      (display "out = {")
      (display-new-messages (cdr expression))
      (display "}"))
    ((eq? (car expression) 'unchanged)
      (display-unchanged (car (cdr expression))))
    ((eq? (car expression) 'post)
      (display-post (car (cdr expression))))
    ((eq? (car expression) '=)
      (display-element prefix (car (cdr expression)))
      (display " = ")
      (display-element prefix (car (cdr (cdr expression)))))
    ((eq? (car expression) '!=)
      (display-element prefix (car (cdr expression)))
      (display " /= ")
      (display-element prefix (car (cdr (cdr expression)))))
    ((eq? (car expression) 'not)
      (display "\\neg ")
      (display-bool (+ offset 4) prefix (car (cdr expression))))
    ((eq? (car expression) 'and)
      (display "/\\ ")
      (display-bool (+ offset 3) prefix (car (cdr expression)))
      (if (not (eq? (cdr (cdr expression)) '()))
          (display-bool offset prefix (cons 'and (cdr (cdr expression))) 'tail)))
    ((eq? (car expression) 'or)
      (display "\\/ ")
      (display-bool (+ offset 3) prefix (car (cdr expression)))
      (if (not (eq? (cdr (cdr expression)) '()))
          (display-bool offset prefix (cons 'or (cdr (cdr expression))) 'tail)))
    ((eq? (car expression) 'element-of)
      (display-element prefix (car (cdr expression)))
      (display " \\in ")
      (display-set prefix (car (cdr (cdr expression)))))
    ((eq? (car expression) '>)
      (display (nat->tla prefix (car (cdr expression))))
      (display " > ")
      (display (nat->tla prefix (car (cdr (cdr expression))))))
    ((eq? (car expression) '<)
      (display (nat->tla prefix (car (cdr expression))))
      (display " < ")
      (display (nat->tla prefix (car (cdr (cdr expression))))))
    ((eq? (car expression) '>=)
      (display (nat->tla prefix (car (cdr expression))))
      (display " >= ")
      (display (nat->tla prefix (car (cdr (cdr expression))))))
    ((eq? (car expression) '<=)
      (display (nat->tla prefix (car (cdr expression))))
      (display " <= ")
      (display (nat->tla prefix (car (cdr (cdr expression))))))
    (else
      (display "TODO-bool:")
      (display expression))))

;--------------------------------------------------------
; SEQUENCE Expressions
;--------------------------------------------------------

(define (elements prefix expression)
  (cond ((eq? expression '())
          "")
        ((eq? (cdr expression) '())
         (element->tla prefix (car expression)))
        (else
          (string-join (element->tla prefix (car expression))
          (string-join ", "
                       (elements prefix (cdr expression)))))))

(define (seq->tla prefix expression)
  (cond
    ((eq? expression 'empty-sequence)
      "<< >>")
    ((not (list? expression))
      (string-join prefix (symbol->string expression)))
    ((eq? (car expression) 'enum->seq)
      (string-join "<< " 
      (string-join (elements prefix (cdr expression))
                   " >>")))
    ((eq? (car expression) 'append)
      (string-join "Append(" 
      (string-join (seq->tla prefix (car (cdr expression))) 
      (string-join ", "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
    ((eq? (car expression) 'concat)
      (string-join "(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
      (string-join " \\o "
      (string-join (nat->tla prefix (car (cdr (cdr expression))))
                   ")")))))
    ((eq? (car expression) 'tail)
      (string-join "Tail(" 
      (string-join (nat->tla prefix (car (cdr expression))) 
                   ")")))
    (else
      (string-join "TODO-seq:"
      (number->string expression)))))

;--------------------------------------------------------
; CONDITIONAL Expressions
;--------------------------------------------------------

(define (display-if offset prefix expression)
  '();TODO 
)

;--------------------------------------------------------
; CONSTANTS
;--------------------------------------------------------

(define (constant-name id)
 (string-join ClassName (string-join "ActorConstant_" (symbol->string id))))

; .example (extract-constant-ids (nat n) (bool b) (ref r)) -> '(n b r)
(define (extract-constant-ids typed-list)
  (if (null? typed-list)
    '()
    (cons (constant-name (car (cdr (car typed-list))))
          (extract-constant-ids (cdr typed-list)))))
; Takes an (type id) pair and generates a "id \in type"
(define (constant-assume-item pair)
  (string-join (constant-name (car (cdr pair))) (string-join " \\in " (type->tla (car pair)))))

; Generates a list of assumptions for constant names and their according type.
(define (extract-constant-assumptions typed-list)
  (if (null? typed-list)
    '()
    (cons (constant-assume-item (car typed-list))
          (extract-constant-assumptions (cdr typed-list)))))

;--------------------------------------------------------
; VARIABLES
;--------------------------------------------------------

; Takes an (type id) pair and generates a "id : type"
(define (variable-type-item pair)
  (string-join (symbol->string (car (cdr pair))) (string-join " : " (type->tla (car pair)))))

; Generats a list of variables with types
(define (extract-variable-with-types typed-list)
  (if (null? typed-list)
    '()
    (cons (variable-type-item (car typed-list))
          (extract-variable-with-types (cdr typed-list)))))

;--------------------------------------------------------
; INIT
;--------------------------------------------------------

(define (display-new-actor actor)
  (define type (string-capitalize (symbol->string (car actor))))
  (define id (car (cdr actor)))
  (define state (car (cdr (cdr actor))))

  (display "[type |-> \"") (display type) (display "\",") (newline)
  (tab-space 14) (display "id |-> ") (display-reference "pre." id) (display ",") (newline)
  (tab-space 14) (display "state |-> [") (display-comma-separated-block 25 (extract-variables-with-values "pre." state))  (display "] ]"))

(define (display-new-actors actors)
  (cond
    ((eq? actors '())
     '())
    ((eq? (cdr actors) '())
     (display-new-actor (car actors) ))
   (else
    (display-new-actor (car actors))
    (display ", ") (newline) (tab-space 13)
    (display-new-actors (cdr actors) ))))

;--------------------------------------------------------
; REFERENCE
;--------------------------------------------------------

(define (display-reference prefix expression)
  (cond
    ((eq? expression 'self)
      (display "id"))
    ((not (list? expression))
      (display prefix)
      (display expression))
    ((eq? (car expression) 'const)
      (display (constant-name (car (cdr expression)))))
    ((eq? (car expression) 'post)
      (display-post (car (cdr expression))))
    ((eq? (car expression) 'message)
      (display (message-value->tla (car (cdr expression)))))
    ((eq? (car expression) '@)
      (display "NextFreeID(") (display (nat->tla "pre." (car (cdr expression)))) (display ")"))
      ;TODO: other operations
    (else
      (display "TODO-reference:")
      (display expression))))

;--------------------------------------------------------
; SEND
;--------------------------------------------------------

(define (variable-value-item prefix pair)
  (define name (symbol->string (car pair)))
  (define value (car (cdr pair)))
  (cond
    ((eq? value 'self)
      (string-join name " |-> id"))
    ((eq? value 'empty-set)
      (string-join name " |-> { }"))
    ((eq? value 'empty-sequence)
      (string-join name " |-> << >>"))
    ((symbol? value)
      (string-join name (string-join " |-> " (string-join prefix (symbol->string value)))))
    ((string? value)
      (string-join name (string-join " |-> \"" (string-join value "\""))))
    ((number? value)
      (string-join name (string-join " |-> " (number->string value))))
    ((boolean? value)
      (string-join name (string-join " |-> " (boolean->string value))))
    ((and (list? value) (eq? (car value) 'const))
      (string-join name (string-join " |-> " (constant-name (car (cdr value))))))
    ((and (list? value) (eq? (car value) 'any))
      (string-join name (string-join " |-> " (any (car (cdr value))))))
    ((and (list? value) (eq? (car value) '*))
      (string-join name (string-join " |-> " (nat->tla prefix value))))
    ((and (list? value) (eq? (car value) '@))
      (string-join name (string-join " |-> NextFreeID(" (string-join (nat->tla "pre." (car (cdr value))) ")"))))
    ((and (list? value) (eq? (car value) 'head))
      (string-join name (string-join " |-> Head(" (string-join (seq->tla "pre." (car (cdr value))) ")"))))
    ((and (list? value) (eq? (car value) 'enum->seq))
      (string-join name (string-join " |-> " (seq->tla "pre." value))))
    ;TODO: other possibilities?
    (else
      (display "TODO-value-item-pair: {name: ") (display name) (display ", value: ") (display value)
)))

(define (extract-variables-with-values prefix typed-list)
  (if (null? typed-list)
    '()
    (cons (variable-value-item prefix (car typed-list))
          (extract-variables-with-values prefix (cdr typed-list)))))

(define (display-new-message message)
  (define amount (car message))
  (define name (car (cdr message)))
  (define destination (car (cdr (cdr (cdr message)))))
  (define body (car (cdr (cdr message))))

  (display "[name |-> ") (display-string "pre." name) (display ",") (newline)
  (tab-space 14) (display "destination |-> ") (display-reference "pre." destination) (display ",") (newline)
  (tab-space 14) (display "body |-> [") (display-comma-separated-block 24 (extract-variables-with-values "pre." body)) (display "],") (newline)
  (tab-space 14) (display "amount |-> ") (display (nat->tla "pre." amount)) (display "]"))

(define (display-new-messages messages)
  (cond
    ((eq? messages '())
     '())
    ((eq? (cdr messages) '())
     (display-new-message (car messages) ))
   (else
    (display-new-message (car messages))
    (display ",") (newline) (tab-space 13)
    (display-new-messages (cdr messages) ))))

;--------------------------------------------------------
; ACTIONS
;--------------------------------------------------------

(define (display-unchanged name)
  (display "post.") (display name) (display " = pre.") (display name))

(define (display-post name)
  (display "post.") (display name))

(define (post->tla name)
  (string-join "post." name))

(define (extract-names actions)
  (if (eq? actions '())
      '()
      (cons (symbol->string (car (cdr (car actions))))
            (extract-names (cdr actions)))))

(define (extract-names-with-quotes actions)
  (if (eq? actions '())
      '()
      (cons (string-join "\"" (string-join (symbol->string (car (cdr (car actions)))) "\""))
            (extract-names-with-quotes (cdr actions)))))

;--------------------------------------------------------
; MESSAGES
;--------------------------------------------------------

(define (message-value->tla name)
  (string-join "message.body." (symbol->string name)))

(define (display-possible-new-message message)
  (define name (car (cdr message)))
  (define body (car (cdr (cdr message))))
  
  (display "   [name : {\"") (display name) (display "\"},") (newline)
  (display "    destination : ActiveActorIDsByType(\"") (display ClassName) (display "\"), ") (newline)
  (display "    body : [")
  (display-comma-separated-block 12 (extract-variable-with-types body))
  (display "],") (newline)
  (display "    amount : {1,2}]"))

(define (display-possible-messages sub-expressions)
  (cond
    ((eq? sub-expressions '())
      '())
    ((eq? (cdr sub-expressions) '())
      (display-possible-new-message (car sub-expressions)))
    (else
      (display-possible-new-message (car sub-expressions))
      (display " \\cup ") (newline)
      (display-possible-messages (cdr sub-expressions)))))

;--------------------------------------------------------

(define (constants->tla const-names)
  (display-headder "Constants")
  (display "CONSTANTS ")
  
  (display-comma-separated-block (string-length "CONSTANTS ")
                                 (extract-constant-ids const-names)) (newline)
  (newline)
  (display "ASSUME ")
  (display-and-separated-block (string-length "ASSUME ")
                               (extract-constant-assumptions const-names)) (newline)
  (newline))

(define (assume->tla assume-expression)
  (display "ASSUME ")
  (display-bool (string-length "ASSUME ") (string-join ClassName "ActorConstant_") (car assume-expression)) (newline)
  (newline))

(define (acquaintances->tla acquaintances-expression)
  (display-headder "Acquaintances")
  (display ClassName) (display "_Acquaintances(state) == ") (newline)
  (display "   ")(display-set "state." (car acquaintances-expression)) (newline)
;  (display "{} \\* TODO") (newline)
  (newline))

(define (public-state->tla public-state-expression)
  (display-headder "Public State")
  (display ClassName) (display "_PublicState(state) == ")
;  (display (element->tla "state." (car acquaintances-expression))) (newline)
  (display "TRUE \\* TODO") (newline)
  (newline))

(define (variables->tla var-names)
  (display-headder "Variables")
  (display ClassName)
  (display "_PossibleState ==") (newline)
  (display "   [")
  (display-comma-separated-block (string-length "   [")
                                 (extract-variable-with-types var-names))
  (display "]") (newline)
  (newline)
  (display ClassName) (display "_PossibleNewActors ==") (newline)
  (display "   [type : {\"") (display ClassName) (display "\"},") (newline)
  (display "    state : ") (display ClassName) (display "_PossibleState,") (newline)
  (display "    id : ActorIDs]") (newline)
  (newline)
  )

(define (invariant->tla expression)
  (display ClassName ) (display "_Invariant(state) ==") (newline)
  (display "   /\\ state \\in ") (display ClassName) (display "_PossibleState") (newline)
  (display "   /\\ ")
  (display-bool 6 "state." (car expression)) (newline)
  (newline))

(define (init-predicate->tla expression)
  (display-headder "Init Predicate")
  (display ClassName ) (display "_InitPredicate(state) ==")
  (display-bool 3 "state." (car expression) 'newline) (newline)
  (newline))

(define (display-event-options type)
  (cond
    ((string=? type "Condition")
     (display "state"))
    ((string=? type "PossibleNewActors")
     (display "id, post, pre"))
    ((string=? type "PossibleNewMessages")
     (display "id, post, pre"))
    ((string=? type "Predicate")
     (display "id, post, new, out, pre"))
    (else
     (display type))
  )
)

(define (display-event-cases kind type names . tail)
  (if (not (eq? names '()))
    (and
      (cond
        ((eq? tail '())
          (display "   CASE"))
        (else
          (display "     []")))
    (display " name = \"") (display (car names)) (display "\" ->") (newline)
    (display "      ") (display ClassName) (display "_") (display (string-capitalize kind)) (display (string-capitalize (car names))) (display "_") (display type) (display "(") (display-event-options type) (display ")") (newline)
    (display-event-cases kind type (cdr names) 'tail))))

(define (events->tla sub-expressions)
  (display-headder "Events")
  (display ClassName) (display "_Events ==") (newline)
  (display "   {") (display-comma-separated-list (extract-names-with-quotes sub-expressions)) (display "}") (newline)
  (newline)
  (eval-sub-expressions sub-expressions)
  (newline)
  (display "----") (newline)
  (display "LOCAL ") (display ClassName) (display "_Event_Condition(name, state) ==") (newline)
  (display-event-cases "Event" "Condition" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event_PossibleNewActors(name, id, post, pre) ==") (newline)
  (display-event-cases "Event" "PossibleNewActors" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event_PossibleNewMessages(name, id, post, pre) ==") (newline)
  (display-event-cases "Event" "PossibleNewMessages" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event_Predicate(name, id, post, new, out, pre) ==") (newline)
  (display-event-cases "Event" "Predicate" (extract-names sub-expressions)) (newline)
  (newline))

(define (display-operation-options type)
  (cond
    ((string=? type "Condition")
     (display "state, message"))
    ((string=? type "PossibleNewActors")
     (display "id, post, pre, message"))
    ((string=? type "PossibleNewMessages")
     (display "id, post, pre, message"))
    ((string=? type "Predicate")
     (display "id, post, new, out, pre, message"))
    (else
     (display type))
  )
)

(define (display-operation-cases kind type names . tail)
  (if (not (eq? names '()))
    (and
      (cond
        ((eq? tail '())
          (display "   CASE"))
        (else
          (display "     []")))
    (display " name = \"") (display (car names)) (display "\" ->") (newline)
    (display "      ") (display ClassName) (display "_") (display (string-capitalize kind)) (display (string-capitalize (car names))) (display "_") (display type) (display "(") (display-operation-options type) (display ")") (newline)
    (display-operation-cases kind type (cdr names) 'tail))))
  
(define (operations->tla sub-expressions)
  (display-headder "Operations")
  (display ClassName) (display "_Operations ==") (newline)
  (display "   {") (display-comma-separated-list (extract-names-with-quotes sub-expressions)) (display "}") (newline)
  (newline)
  (eval-sub-expressions sub-expressions)
  (newline)
  (display "----") (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation_Condition(name, state, message) ==") (newline)
  (display-operation-cases "Operation" "Condition" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation_PossibleNewActors(name, id, post, pre, message) ==") (newline)
  (display-operation-cases "Operation" "PossibleNewActors" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation_PossibleNewMessages(name, id, post, pre, message) ==") (newline)
  (display-operation-cases "Operation" "PossibleNewMessages" (extract-names sub-expressions))
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation_Predicate(name, id, post, new, out, pre, message) ==") (newline)
  (display-operation-cases "Operation" "Predicate" (extract-names sub-expressions)) (newline)
  (newline))


;TODO comma seperate the list of new actors!
(define (display-possible-new-actors expression)
  (if (list? expression)
   (if (eq? expression '())
      '()
      (if (list? (car expression))
         (and (display-possible-new-actors (car expression))
              (display-possible-new-actors (cdr expression)))
         (if (eq? (car expression) 'new)
            (if (not (eq? (cdr expression) '()))
               (display-new-actors (cdr expression)))
            (display-possible-new-actors (cdr expression)))))))

;TODO comma seperate the list of new messages!
(define (display-possible-new-messages expression)
  (if (list? expression)
   (if (eq? expression '())
      '()
      (if (list? (car expression))
            (and (display-possible-new-messages (car expression))
                 (display-possible-new-messages (cdr expression)))
         (if (eq? (car expression) 'send)
            (if (not (eq? (cdr expression) '()))
              (display-new-messages (cdr expression)))
            (display-possible-new-messages (cdr expression)))))))


(define (event->tla sub-expression)
  (define EventName (string-capitalize (symbol->string (car sub-expression))))

  (display "LOCAL ") (display ClassName) (display "_Event") (display EventName) (display "_Condition(state) ==")
  (display-bool 3 "state." (car (cdr sub-expression)) 'newline) (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event") (display EventName) (display "_PossibleNewActors(id, post, pre) ==") (newline)
  (display "   {")
  (display-possible-new-actors (car (cdr (cdr sub-expression))))
  (display "}") (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event") (display EventName) (display "_PossibleNewMessages(id, post, pre) ==") (newline)
  (display "   {")
  (display-possible-new-messages (car (cdr (cdr sub-expression))))
  (display "}") (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Event") (display EventName) (display "_Predicate(id, post, new, out, pre) ==")
  (display-bool 3 "pre." (car (cdr (cdr sub-expression))) 'newline) (newline)
  (newline))

(define (operation->tla sub-expression)
  (define OperationName (string-capitalize (symbol->string (car sub-expression))))
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_Condition(state, message) ==")
  (display-bool 3 "state." (car (cdr (cdr sub-expression))) 'newline) (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_PossibleNewActors(id, post, pre, message) ==") (newline)
  (display "   {")
  (display-possible-new-actors (car (cdr (cdr (cdr sub-expression)))))
  (display "}") (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_PossibleNewMessages(id, post, pre, message) ==") (newline)
  (display "   {")
  (display-possible-new-messages (car (cdr (cdr (cdr sub-expression)))))
  (display "}") (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_Predicate(id, post, new, out, pre, message) ==")
  (display-bool 3 "pre." (car (cdr (cdr (cdr sub-expression)))) 'newline) (newline)
  (newline))

;--------------------------------------------------------

(define (actor-class->tla class-name sub-expressions)
  (display "---- MODULE ") (display ClassName) (display "Actor ----") (newline)
  (display "\\****") (newline)
  (display "\\* This file is automatically generated from the ") (display class-name) (display ".ral") (newline)
  (newline)
  (display "EXTENDS ActorBase") (newline)
  (newline)
  (eval-sub-expressions sub-expressions)
  
  (display "----") (newline)
 (newline)
(display ClassName) (display "_Action_Condition(kind, name, state, message) ==") (newline)
(display "   IF kind = \"event\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Event_Condition(name, state)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      ") (display ClassName) (display "_Operation_Condition(name, state, message)") (newline)
(newline)
(display ClassName) (display "_Action_PossibleNewActors(kind, name, id, post, pre, message) ==") (newline)
(display "   IF kind = \"event\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Event_PossibleNewActors(name, id, post, pre)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      ") (display ClassName) (display "_Operation_PossibleNewActors(name, id, post, pre, message)") (newline)
(newline)
(display ClassName) (display "_Action_PossibleNewMessages(kind, name, id, post, pre, message) ==") (newline)
(display "   IF kind = \"event\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Event_PossibleNewMessages(name, id, post, pre)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      ") (display ClassName) (display "_Operation_PossibleNewMessages(name, id, post, pre, message)") (newline)
(newline)
(display ClassName) (display "_Action_Predicate(kind, name, id, post, new, out, pre, message) ==") (newline)
(display "   IF kind = \"event\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Event_Predicate(name, id, post, new, out, pre)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      /\\ name = message.name") (newline)
(display "      /\\ ") (display ClassName) (display "_Operation_Predicate(name, id, post, new, out, pre, message)") (newline)
 (newline)
 
  (display "====")
  (newline))

(define (eval-sub-expressions expression-list)
  (if (not (null? expression-list))
      (and (eval (car expression-list))
           (eval-sub-expressions (cdr expression-list)))))

;--------------------------------------------------------

(eval read-spec)
