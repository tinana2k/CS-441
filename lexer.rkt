#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require (prefix-in : parser-tools/lex-sre))
(require syntax/readerr)


(define (more-lex input-port [line-num 1])

  (define tokens-input
  
    (lexer

     [(:or "read") (cons `(read ,line-num ,(string->symbol lexeme)) (more-lex input-port line-num))]
     [(:or "write") (cons `(write ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
    
     [(:: alphabetic (:* (:or alphabetic numeric))) (cons `(ID ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [(:or (:+ numeric) (:: (:* numeric) (:or (:: #\. numeric) (:: numeric #\.)) (:* numeric))) (cons ` (num ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [(:or #\tab #\space ) (more-lex input-port line-num)]
     [(:or #\newline "\r\n") (more-lex input-port (+ line-num 1))]

   

     [(:or "(") (cons `( L_PARAN ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [(:or ")") (cons `( R_PARAN ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]

     [#\+ (cons `(plus ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [#\- (cons `(minus ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [#\* (cons `(time ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]
     [#\/ (cons `(div ,line-num ,(string->symbol lexeme))(more-lex input-port line-num))]

  


     [(:or ":=" ) (cons `(assign ,line-num ,(string->symbol lexeme)) (more-lex input-port line-num))]

     [(:or "$$") `((eof ,line-num ,"EOF"))]))
  
  (tokens-input input-port))

(provide more-lex)

 