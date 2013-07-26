#!../gambc-v4_6_9/bin/gsi-script -:s
;----------------------------------------------------------
; UTILS
;----------------------------------------------------------

; combines two lists
; .parameter a The first list.
; .parameter b The second list.
; .returns Returns a list of "a" and "b".
(define (list-join a b)
  (cond
    ((null? b) a)
    ((null? a) b)
    (else (cons (car a) (list-join (cdr a) b)))))

; combines the two given strings
; .parameter a The first string
; .parameter b The second string
; .returns a string containing the content of a followed by the content of b
(define (string-join a b)
    (list->string (list-join (string->list a) (string->list b))))

; removes "-" and capitalize following letter
; for example: converts "asDf-fdsa" to "AsdfFdsa"
; .parameter str The string to be transformed
; .returns Returns a string with the first and all letters following a dash ("-") in upper case.
;          The rest of the letters will be in lower case. All dashes are removed.
(define (string-capitalize str . tail)
  (define lst (string->list str))
  (cond
   ((eq? lst '())
     (list->string '()))
   ((eq? (car lst) #\-)
     (list->string (string->list (string-capitalize (list->string (cdr lst))))))
   ((eq? tail '())
     (list->string (cons (char-upcase (car lst)) (string->list (string-capitalize (list->string (cdr lst)) 'tail)))))
   (else
     (list->string (cons (char-downcase (car lst)) (string->list (string-capitalize (list->string (cdr lst)) 'tail)))))
   ))

;----------------------------------------------------------

; defines the current ReActor Actor-Class to be red.
(define class-to-read (car (cdr (command-line))))

; reads the input file
(define read-spec
  (call-with-input-file (string-join "input/" (string-join class-to-read ".ral"))
    (lambda (p)
      (read p))))

; The Classname of current Actor to read convertet into a format used by tla+
(define ClassName (string-capitalize class-to-read))

;----------------------------------------------------------
; Syntax definitions for all high level symbols found in ReActor
;----------------------------------------------------------

(define-syntax class
  (syntax-rules () 
    ((actor-class name body ...)
     (actor-class->tla 'name '(body ...)))))

(define-syntax constants
  (syntax-rules ()
    ((constants body ...)
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

(define-syntax invariant
  (syntax-rules ()
    ((invariant body ...)
     (invariant->tla '(body ...)))))

(define-syntax init-predicate
  (syntax-rules ()
    ((init-predicate body ...)
     (init-predicate->tla '(body ...)))))

(define-syntax activities
  (syntax-rules ()
    ((activities body ...)
     (events->tla "Activity" "Activities" '( body ...)))))

(define-syntax activity
  (syntax-rules ()
    ((activity body ...)
     (activity->tla '(body ...)))))

(define-syntax operations
  (syntax-rules ()
    ((operations body ...)
     (operations->tla '(body ...)))))

(define-syntax operation
  (syntax-rules ()
    ((operation body ...)
     (operation->tla '( body ...)))))

;----------------------------------------------------------
; COSMETIC
;----------------------------------------------------------

; displays the given number of spaces
; .parameter offset The amount of spaces to be displayed
(define (tab-space offset)
  (display (make-string offset #\space)))

; displays the given number of spaces depending on the elements and tail parameters
; This function is used in all functions that are displaying elements in more than one line.
;
; .param elements The elements of the function that is actually displaying the elements.
; The last recursion step does not need an indention and if there are no more elements
; there will be no indention
;
; .param tail Indicates that the function call is not the first in recursion.
; Usually the first line is already at a position that dont need to be indented.
; Thus all but the first calles of the functions have any tail parameter given
; wich indicates that this particular call is not the first and therefore to be indended.
;
(define (tab-feed elements tail offset)
  (if (and (not (null? elements))
           (not (eq? '() tail)))
             (and (newline)
                  (tab-space offset))))

;----------------------------------------------------------
; GENERAL
;----------------------------------------------------------

(define (type->tla type)
  (cond
    ((eq? type 'nat) "NatType")
    ((eq? type 'bool) "BoolType")
    ((eq? type 'ref) "RefType")
    (else #f))) ;TODO some kind of exception would be nice here

(define (any type)
  (cond
    ((eq? type 'nat) "AnyNat")
    ((eq? type 'bool) "AnyBool")
    ((eq? type 'ref) "AnyRef")
    (else #f))) ;TODO some kind of exception would be nice here

;----------------------------------------------------------
; ELEMENT Expressions
;----------------------------------------------------------

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
    ((not (list? expression))
      (display prefix)
      (display expression))
    ((eq? (car expression) 'post)
      (display-post (car (cdr expression))))
    ((eq? (car expression) '@)
      (display-reference prefix expression))
    ((eq? (car expression) 'message)
      (display (message-value->tla (car (cdr expression)))))
    ((or (eq? (car expression) '+)
         (eq? (car expression) '-)
         (eq? (car expression) '*)
         (eq? (car expression) '/))
      (display-nat prefix expression))
    ((or (eq? (car expression) 'enum->set)
         (eq? (car expression) 'set-union)
         (eq? (car expression) 'set-intersection)
         (eq? (car expression) 'set-difference))
      (display-set prefix expression))
    ;TODO other possibilities
    (else
      (display "TODO-element:")
      (display expression))))

;----------------------------------------------------------
; SET Expressions
;----------------------------------------------------------

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
    ((eq? (car expression) 'type)
      (display (type->tla (car (cdr expression)))))
    ((eq? (car expression) 'enum->set)
      (display "{")
      (display-elements prefix (cdr expression))
      (display "}"))
    ;TODO: operations on sets
    (else
      (display "TODO-set:")
      (display expression))
      ))

;----------------------------------------------------------
; STRING Expressions
;----------------------------------------------------------

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

;----------------------------------------------------------
; NUMERIC Expressions
;----------------------------------------------------------

(define (nat->tla prefix expression)
  (cond
    ((number? expression)
      (number->string expression))
    ((not (list? expression))
      (string-join prefix (symbol->string expression)))
    ((eq? (car expression) 'message)
      (message-value->tla (car (cdr expression))))
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
      ;TODO: other operations: div mod
    (else
      (string-join "TODO-nat:"
      (number->string expression)))))

;----------------------------------------------------------
; BOOLEAN Expressions
;----------------------------------------------------------

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
    ((eq? (car expression) 'init)
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
    (else
      (display "TODO-bool:")
      (display expression))))

;----------------------------------------------------------
; CONDITIONAL Expressions
;----------------------------------------------------------

(define (display-if offset prefix expression)
  '();TODO 
)

;----------------------------------------------------------
; CONSTANTS
;----------------------------------------------------------

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

;----------------------------------------------------------
; VARIABLES
;----------------------------------------------------------

; Takes an (type id) pair and generates a "id : type"
(define (variable-type-item pair)
  (string-join (symbol->string (car (cdr pair))) (string-join " : " (type->tla (car pair)))))

; Generats a list of variables with types
(define (extract-variable-with-types typed-list)
  (if (null? typed-list)
    '()
    (cons (variable-type-item (car typed-list))
          (extract-variable-with-types (cdr typed-list)))))

;----------------------------------------------------------
; INIT
;----------------------------------------------------------

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
    (display-new-actor (cdr actors) ))))

;----------------------------------------------------------
; REFERENCE
;----------------------------------------------------------

(define (display-reference prefix expression)
  (cond
    ((not (list? expression))
      (display prefix)
      (display expression))
    ((eq? (car expression) 'const)
      (display (constant-name (car (cdr expression)))))
    ((eq? (car expression) 'post)
      (display-post (car (cdr expression))))
    ((eq? (car expression) '@)
      (display "NextFreeID(") (display (nat->tla "pre." (car (cdr expression)))) (display ")"))
      ;TODO: other operations
    (else
      (display "TODO-reference:")
      (display expression))))

;----------------------------------------------------------
; SEND
;----------------------------------------------------------

(define (variable-value-item prefix pair)
  (define name (symbol->string (car pair)))
  (define value (car (cdr pair)))
  (cond
    ((eq? value 'self)
      (string-join name " |-> id"))
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
  (define destination (car (cdr (cdr message))))
  (define body (car (cdr (cdr (cdr message)))))

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

;----------------------------------------------------------
; EVENTS
;----------------------------------------------------------

(define (display-unchanged name)
  (display "post.") (display name) (display " = pre.") (display name))

(define (display-post name)
  (display "post.") (display name))

(define (extract-names events)
  (if (eq? events '())
      '()
      (cons (symbol->string (car (cdr (car events))))
            (extract-names (cdr events)))))

(define (extract-names-with-quotes events)
  (if (eq? events '())
      '()
      (cons (string-join "\"" (string-join (symbol->string (car (cdr (car events)))) "\""))
            (extract-names-with-quotes (cdr events)))))

;----------------------------------------------------------
; MESSAGES
;----------------------------------------------------------

(define (message-value->tla name)
  (string-join "NextMessageBody(id)." (symbol->string name)))

(define (display-possible-new-message message)
  (define name (car (cdr message)))
  (define body (car (cdr (cdr message))))
  
  (display "   [name : {\"") (display name) (display "\"},") (newline)
  (display "    destination : ActiveActorIDsByType(\"") (display ClassName) (display "\"), ") (newline)
  (display "    body : [")
  (display-comma-separated-block 12 (extract-variable-with-types body))
  (display "],") (newline)
  (display "    amount : {1,2}]"))

(define (display-possible-new-messages sub-expressions)
  (cond
    ((eq? sub-expressions '())
      '())
    ((eq? (cdr sub-expressions) '())
      (display-possible-new-message (car sub-expressions)))
    (else
      (display-possible-new-message (car sub-expressions))
      (display " \\cup ") (newline)
      (display-possible-new-messages (cdr sub-expressions)))))

;----------------------------------------------------------

(define (constants->tla const-names)
  (display "----") (newline)
  (display "(* Constants *)") (newline)
  (newline)
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
  (display "----") (newline)
  (display "(* Acquaintances *)") (newline)
  (newline)
  (display ClassName) (display "_Acquaintances(state) == ")
;  (display-set "state." (car acquaintances-expression)) (newline)
  (display "{} \\* TODO") (newline)
  (newline))

(define (public-state->tla public-state-expression)
  (display "----") (newline)
  (display "(* Public State *)") (newline)
  (newline)
  (display ClassName) (display "_PublicState(state) == ")
;  (display-element "state." (car acquaintances-expression)) (newline)
  (display "TRUE \\* TODO") (newline)
  (newline))

(define (variables->tla var-names)
  (display "----") (newline)
  (display "(* Variables *)") (newline)
  (newline)
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
  (display "    id : FreeIDs]") (newline)
  (newline)
  )

(define (invariant->tla expression)
  (display ClassName ) (display "_Invariant(state) ==") (newline)
  (display "   /\\ state \\in ") (display ClassName) (display "_PossibleState") (newline)
  (display "   /\\ ")
  (display-bool 6 "state." (car expression)) (newline)
  (newline))

(define (init-predicate->tla expression)
  (display "----") (newline)
  (display "(* Init Predicate *)") (newline)
  (newline)
  (display ClassName ) (display "_InitPredicate(state) ==")
  (display-bool 3 "state." (car expression) 'newline) (newline)
  (newline))

(define (messages->tla sub-expressions)
  (display "----") (newline)
  (display "(* Messages *)") (newline)
  (newline)
  (display ClassName) (display "_PossibleNewMessages ==") (newline)
  (display-possible-new-messages sub-expressions) (newline)
  (newline)
  )

(define (display-event-options type)
  (cond
    ((string=? type "Condition")
     (display "state"))
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

(define (events->tla kind kindp sub-expressions)
  (display "----") (newline)
  (display "(* ") (display kindp) (display " *)") (newline)
  (newline)
  (display ClassName) (display "_") (display kindp) (display " ==") (newline)
  (display "   {") (display-comma-separated-list (extract-names-with-quotes sub-expressions)) (display "}") (newline)
  (newline)
  (eval-sub-expressions sub-expressions)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_") (display kind) (display "_Condition(name, state) ==") (newline)
  (display-event-cases kind "Condition" (extract-names sub-expressions)) (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_") (display kind) (display "_Predicate(name, id, post, new, out, pre) ==") (newline)
  (display-event-cases kind "Predicate" (extract-names sub-expressions)) (newline)
  (newline))
  
(define (operations->tla sub-expressions)
  (messages->tla sub-expressions)
  (events->tla "Operation" "Operations" sub-expressions)
)

(define (activity->tla sub-expression)
  (define ActivityName (string-capitalize (symbol->string (car sub-expression))))

  (display "LOCAL ") (display ClassName) (display "_Activity") (display ActivityName) (display "_Condition(state) ==")
  (display-bool 3 "state." (car (cdr sub-expression)) 'newline) (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Activity") (display ActivityName) (display "_Predicate(id, post, new, out, pre) ==")
  (display-bool 3 "pre." (car (cdr (cdr sub-expression))) 'newline) (newline)
  (newline))

(define (operation->tla sub-expression)
  (define OperationName (string-capitalize (symbol->string (car sub-expression))))
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_Condition(state) ==")
  (display-bool 3 "state." (car (cdr (cdr sub-expression))) 'newline) (newline)
  (newline)
  (display "LOCAL ") (display ClassName) (display "_Operation") (display OperationName) (display "_Predicate(id, post, new, out, pre) ==")
  (display-bool 3 "pre." (car (cdr (cdr (cdr sub-expression)))) 'newline) (newline)
  (newline))

;----------------------------------------------------------

(define (display-comma-separated-list elements)
  (cond
    ((null? elements) '())
    ((null? (cdr elements)) (display (car elements)))
    (else (display (car elements))
          (display ", ")
          (display-comma-separated-list (cdr elements)))))

(define (display-comma-separated-block offset elements . tail)
  (tab-feed elements tail offset)
  (cond
    ((null? elements) '())
    ((null? (cdr elements)) (display (car elements)))
    (else (display (car elements))
          (display ",")
          (display-comma-separated-block offset (cdr elements) 'tail))))

(define (display-and-separated-block offset elements . tail)
  (tab-feed elements tail offset)
  (cond
    ((null? elements) '())
    (else (if (not (eq? '() tail))
              (display (make-string offset #\space)))
          (display "/\\ ")
          (display (car elements)) (newline)
          (display-and-separated-block offset (cdr elements) 'tail))))

;----------------------------------------------------------

(define (actor-class->tla class-name sub-expressions)
  ;TODO the ClassName should start with a capital letter
  (display "---- MODULE ") (display ClassName) (display "Actor ----") (newline)
  (display "\\****") (newline)
  (display "\\* This file is automatically generated from the ") (display class-name) (display ".ral") (newline)
  (newline)
  (display "EXTENDS ActorEnvironment") (newline)
  (newline)
  (eval-sub-expressions sub-expressions)
  
  (display "----") (newline)
 (newline)
(display ClassName) (display "_Event_Condition(kind, name, state) ==") (newline)
(display "   IF kind = \"activity\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Activity_Condition(name, state)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      ") (display ClassName) (display "_Operation_Condition(name, state)") (newline)
(newline)
(display ClassName) (display "_Event_Predicate(kind, name, id, post, new, out, pre) ==") (newline)
(display "   IF kind = \"activity\"") (newline)
(display "   THEN") (newline)
(display "      ") (display ClassName) (display "_Activity_Predicate(name, id, post, new, out, pre)") (newline)
(display "   ELSE \\* kind = \"operation\"") (newline)
(display "      /\\ name = NextMessageName(id)") (newline)
(display "      /\\ ") (display ClassName) (display "_Operation_Predicate(name, id, post, new, out, pre)") (newline)
 (newline)
 
  (display "====")
  (newline))

(define (eval-sub-expressions expression-list)
  (if (not (null? expression-list))
      (and (eval (car expression-list))
           (eval-sub-expressions (cdr expression-list)))))

;----------------------------------------------------------

(eval read-spec)
