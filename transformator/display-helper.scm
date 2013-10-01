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

;displays a horizontal bar and a comment containing the parameter string.
(define (display-headder title)
  (display "----") (newline)
  (display "\\****") (newline)
  (display "\\* ") (display title) (newline)
  (newline))

;----------------------------------------------------------

(define (display-comma-separated-list elements)
  (cond
    ((null? elements) '())
    ((null? (cdr elements)) (display (car elements)))
    (else (display (car elements))
          (display ", ")
          (display-comma-separated-list (cdr elements)))))

(define (display-x-separated-block x offset elements . tail)
  (tab-feed elements tail offset)
  (cond
    ((null? elements) '())
    ((null? (cdr elements)) (display (car elements)))
    (else (display (car elements))
          (display x)
          (display-x-separated-block x offset (cdr elements) 'tail))))

(define (display-comma-separated-block offset elements)
  (display-x-separated-block "," offset elements))

(define (display-cup-separated-block offset elements)
  (display-x-separated-block " \\cup" offset elements))

(define (display-cap-separated-block offset elements)
  (display-x-separated-block " \\cap" offset elements))


(define (display-and-separated-block offset elements . tail)
  (tab-feed elements tail offset)
  (cond
    ((null? elements) '())
    (else (if (not (eq? '() tail))
              (display (make-string offset #\space)))
          (display "/\\ ")
          (display (car elements)) (newline)
          (display-and-separated-block offset (cdr elements) 'tail))))

;TODO display-or seperated block

