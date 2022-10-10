#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))
(require syntax/readerr)

(define tokens-input
  (lexer

   [(:or "read")(cons ` (read, (string->symbol lexeme))(tokens-input input-port))]
   [(:or "write")(cons ` (write, (string->symbol lexeme))(tokens-input input-port))]
    
   [(:: alphabetic (:* (:or alphabetic numeric))) (cons ` (ID, (string->symbol lexeme))(tokens-input input-port))]
   [(:or (:+ numeric) (:: (:* numeric) (:or (:: #\. numeric) (:: numeric #\.)) (:* numeric))) (cons ` (num, (string->symbol lexeme))(tokens-input input-port))]
   [(:or #\tab #\space #\newline "\r") (tokens-input input-port)]

   [(:or "(") (cons `( L_PARAN ,(string->symbol lexeme))(tokens-input input-port))]
   [(:or ")") (cons `( R_PARAN ,(string->symbol lexeme))(tokens-input input-port))]

   [#\+ (cons `(plus ,(string->symbol lexeme))(tokens-input input-port))]
   [#\- (cons `(minus ,(string->symbol lexeme))(tokens-input input-port))]
   [#\* (cons `(time ,(string->symbol lexeme))(tokens-input input-port))]
   [#\/ (cons `(div ,(string->symbol lexeme))(tokens-input input-port))]

  


   [(:or ":=" ) (cons `(assign ,(string->symbol lexeme)) (tokens-input input-port))]

   [(:or "$$") `((eof ,"EOF"))]))


(define (match expected list-toks)
  (if (eq? (first (first list-toks)) expected)
      (rest list-toks)
      (error "parse_error")))



(define (program list-toks)
  (case(first (first list-toks))
    [(ID read write eof)
     (match 'eof (stmt_list list-toks))]
    [else (error "parse_error")]
    ))


(define (stmt_list list-toks)
  (case (first (first list-toks))
    [(ID read write)
     (stmt_list (stmt list-toks))]
    [(eof)
     list-toks]
    [else (error "parse_error")]))

(define (stmt list-toks)
  (case (first (first list-toks))
    [(ID)
     (expr (match 'assign (match 'ID list-toks)))]
    [(read)
     (match 'ID (match 'read list-toks))]
    [(write)
     (expr (match 'write list-toks))]
    [else (error "parse_error")]))

(define (expr list-toks)
  (case (first (first list-toks))
    [(ID num L_PARAN)
     (term_tail(term list-toks))]
    [else (error "parse_error")]))

(define (term_tail list-toks)
  (case (first (first list-toks))
    [(plus minus)
     (term_tail(term (add_op list-toks)))]
    [(R_PARAN ID read write eof)
     list-toks]
    [else (error "parse_error")]))

(define (term list-toks)
  (case (first (first list-toks))
    [(ID num L_PARAN)
     (factor_tail (factor list-toks))]
    [else (error "parse_error")]))

(define (factor_tail list-toks)
  (case (first (first list-toks))
    [(time div)
     (factor_tail (factor (mult_op list-toks)))]
    [(plus minus R_PARAN ID read write eof)
     list-toks]
    [else (error "parse_error")]))

(define (factor list-toks)
  (case (first (first list-toks))
    [(ID)
     (match 'ID list-toks)]
    [(num)
     (match 'num list-toks)]
    [(L_PARAN)
     (match 'R_PARAN (expr (match 'L_PARAN list-toks)))]
    [else (error "parse_error")]))

(define (add_op list-toks)
  (case (first (first list-toks))
    [(plus)
     (match 'plus list-toks)]
    [(minus)
     (match 'minus list-toks)]
    [else (error "parse_error")]))

(define (mult_op list-toks)
  (case (first (first list-toks))
    [(time)
     (match 'time list-toks)]
    [(div)
     (match 'div list-toks)]
    [else (error "parse_error")]))

;(program tokens)

(define (parse input-file)
  (define tokens (tokens-input (open-input-file input-file)))

  (displayln tokens)
  (displayln (first tokens))         ; First token from token list
  (displayln (first (first tokens))) ; Token type (used for all of your comparisons)
  (program tokens))

(for/list ([file-path (directory-list "tests" #:build? #t)])
  (displayln (path->string file-path)) ; just so you can see which tests might have failed
  (parse file-path))
