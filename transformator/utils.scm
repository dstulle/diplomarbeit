;--------------------------------------------------------
; UTILS
;--------------------------------------------------------

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

(define (add-to-strings string-list prefix suffix)
  (if (eq? string-list '())
      '()
      (cons (string-join prefix (string-join (car string-list) suffix))
            (add-to-strings (cdr string-list) prefix suffix))))

