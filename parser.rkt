#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))
(require syntax/readerr)
(require "lexer.rkt")

(define (parse input-file)
  (define check (open-input-file input-file))
  (define list-toks (more-lex check))
  (displayln list-toks)
  (if (empty? (program list-toks))
      (display "Accept")
      (display "Warning: FOUND UNEXPECTED EXTRA TOKENS")
      ))
    

(define (match expected list-toks)
  (if (eq? (first (first list-toks)) expected)
      (rest list-toks)
      (error (format "Syntax error will be on line ~a" (second (first list-toks))))))


(define (program list-toks)
  (case(first (first list-toks))
    [(ID read write eof)
     (match 'eof (stmt_list list-toks))]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))


(define (stmt_list list-toks)
  (case (first (first list-toks))
    [(ID read write)
     (stmt_list (stmt list-toks))]
    [(eof)
     list-toks]
   [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (stmt list-toks)
  (case (first (first list-toks))
    [(ID)
     (expr (match 'assign (match 'ID list-toks)))]
    [(read)
     (match 'ID (match 'read list-toks))]
    [(write)
     (expr (match 'write list-toks))]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (expr list-toks)
  (case (first (first list-toks))
    [(ID num L_PARAN)
     (term_tail(term list-toks))]
  [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (term_tail list-toks)
  (case (first (first list-toks))
    [(plus minus)
     (term_tail(term (add_op list-toks)))]
    [(R_PARAN ID read write eof)
     list-toks]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (term list-toks)
  (case (first (first list-toks))
    [(ID num L_PARAN)
     (factor_tail (factor list-toks))]
   [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (factor_tail list-toks)
  (case (first (first list-toks))
    [(time div)
     (factor_tail (factor (mult_op list-toks)))]
    [(plus minus R_PARAN ID read write eof)
     list-toks]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (factor list-toks)
  (case (first (first list-toks))
    [(ID)
     (match 'ID list-toks)]
    [(num)
     (match 'num list-toks)]
    [(L_PARAN)
     (match 'R_PARAN (expr (match 'L_PARAN list-toks)))]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))
(define (add_op list-toks)
  (case (first (first list-toks))
    [(plus)
     (match 'plus list-toks)]
    [(minus)
     (match 'minus list-toks)]
   [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))

(define (mult_op list-toks)
  (case (first (first list-toks))
    [(time)
     (match 'time list-toks)]
    [(div)
     (match 'div list-toks)]
    [else (error (format "Syntax error will be on line ~a" (second (first list-toks))))]))
(provide parse)

;(parse "input02.txt")

;(program tokens)

;(define (parse input-file)
 ;(define tokens (tokens-input (open-input-file input-file)))

  ;(displayln tokens)
  ;(displayln (first tokens))         ; First token from token list
  ;(displayln (first (first tokens))) ; Token type (used for all of your comparisons)
  ;(program tokens)

;(for/list ([file-path (directory-list "tests" #:build? #t)])
  ;(displayln (path->string file-path)) ; just so you can see which tests might have failed
  ;(parse file-path))