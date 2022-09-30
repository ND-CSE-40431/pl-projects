(load "lists.scm")

;;; Constants

(define (space? c) (member c '(#\space #\tab #\newline)))
(define special-toks '("(" ")" "λ" "\\" "." "=" ":" "->"))
(define reserved-words '("0" "succ" "pred" "iszero"
                         "true" "false" "if" "then" "else"
                         "let" "in"
                         "fix"))

;;; Lexer

(define (occurs-at ss s i)
  (let ((j (+ i (string-length ss))))
    (and (<= j (string-length s))
         (string=? ss (substring s i j)))))

(define (lexer s)
  (let loop ((i 0) (j 0) (w '()))
    (letrec ((flush (lambda () (if (< i j) (cons (substring s i j) w) w))))
      (cond ((= j (string-length s))
             (reverse (flush)))
            ((space? (string-ref s j))
             (loop (+ j 1) (+ j 1) (flush)))
            ((find (lambda (tok) (occurs-at tok s j)) special-toks) =>
             (lambda (tok)
               (let ((k (+ j (string-length tok))))
                 (loop k k (cons tok (flush))))))
            (else (loop i (+ j 1) w))))))

;;; Simple imperative stack for tokens

(define list->buf box)
(define (buf-empty? buf) (null? (unbox buf)))
(define (buf-next buf default) (if (buf-empty? buf) default (car (unbox buf))))
(define (buf-pop! buf) (when (not (buf-empty? buf))
                         (set-box! buf (cdr (unbox buf)))))

;;; Parser

(define (expect what w)
  (if (equal? (buf-next w #f) what)
      (buf-pop! w)
      (raise (string-append "expected '" what "'"))))

(define (parse-term s)
  (let* ((w (list->buf (lexer s)))
         (t (parse-abs w)))
    (if (buf-empty? w)
        t
        (raise (string-append "unexpected '" (buf-next w #f) "' after term")))))

(define (parse-abs w)
  (let ((next (buf-next w #f)))
    (cond ((equal? next #f)
           (raise "unexpected end of string"))
          
          ((member next '("\\" "λ"))
           (let* ((_ (buf-pop! w))
                  (x (parse-var w)))
             (let ((next (buf-next w #f)))
               (cond ((equal? next #f)
                      (raise "unexpected end of string"))
                     ((equal? next ":")
                      (let* ((_   (expect ":" w))
                             (tau (parse-arrow w))
                             (_   (expect "." w))
                             (t   (parse-abs w)))
                        (list 'typed-lambda x tau t)))
                     ((equal? next ".")
                      (let* ((_ (expect "." w))
                             (t (parse-abs w)))
                        (list 'lambda x t)))
                     (else (raise "expected : or ."))))))
     
          ((equal? next "if")
           (let* ((_  (expect "if" w))
                  (t1 (parse-abs w))
                  (_  (expect "then" w))
                  (t2 (parse-abs w))
                  (_  (expect "else" w))
                  (t3 (parse-abs w)))
             (list 'if t1 t2 t3)))
    
          ((equal? next "let")
           (let* ((_  (expect "let" w))
                  (x  (parse-var w))
                  (_  (expect "=" w))
                  (t1 (parse-abs w))
                  (_  (expect "in" w))
                  (t2 (parse-abs w)))
             (list 'let x t1 t2)))
    
          (else (parse-app w)))))

(define (parse-app w)
  (let loop ((t (let ((next (buf-next w #f)))
                  (cond ((member next '("succ" "pred" "iszero" "fix"))
                         (buf-pop! w)
                         (list (string->symbol next) (parse-atom w)))
                        (else (parse-atom w))))))
    (if (member (buf-next w #f) '(#f ")" "then" "else" "in"))
        t
        (loop (list 'app t (parse-atom w))))))

(define (parse-atom w)
  (let ((next (buf-next w #f)))
    (cond ((equal? next "0")
           (buf-pop! w) 'zero)
          ((member next '("true" "false"))
           (buf-pop! w) (string->symbol next))
          ((equal? next "(") (let* ((_ (expect "(" w))
                                    (t (parse-abs w))
                                    (_ (expect ")" w)))
                               t))
          (else (list 'var (parse-var w))))))

(define (parse-var w)
  (let ((tok (buf-next w #f)))
    (cond ((equal? tok #f) (raise "unexpected end of string"))
          ((or (member tok special-toks)
               (member tok reserved-words))
           (raise (string-append "unexpected '" tok "'")))
          (else (buf-pop! w) tok))))

(define (parse-arrow w)
  (let loop ((tau (parse-base w)))
    (cond ((equal? (buf-next w #f) "->")
           (let* ((_ (expect "->" w))
                  (tau2 (parse-arrow w)))
             (loop (list 'arrow tau tau2))))
          (else tau))))

(define (parse-base w)
  (let ((next (buf-next w #f)))
    (cond ((equal? next #f)
           (raise "unexpected end of string"))
          ((member next '("Nat" "Bool"))
           (buf-pop! w) (string->symbol next))
          ((equal? next "(")
           (let* ((_ (expect "(" w))
                  (tau (parse-arrow w))
                  (_  (expect ")" w)))
             tau))
          (else (raise (string-append "unexpected '" (buf-next w #f) "'"))))))

;;; Formatter

(define (format-abs t)
  (if (pair? t)
      (case (car t)
        ((if) (string-append "if " (format-abs (second t))
                             " then " (format-abs (third t))
                             " else " (format-abs (fourth t))))
        ((lambda) (string-append "λ" (second t)
                                 ". " (format-abs (third t))))
        ((typed-lambda) (string-append "λ" (second t)
                                       ":" (format-arrow (third t))
                                       ". " (format-abs (fourth t))))
        ((let) (string-append "let " (second t)
                              " = " (format-abs (third t))
                              " in " (format-abs (fourth t))))
        (else (format-app t)))
      (format-app t)))

(define (format-app t)
  (if (pair? t)
      (case (car t)
        ((succ pred iszero fix)
         (string-append (symbol->string (car t)) " " (format-atom (second t))))
        ((app)
         (string-append (format-app (second t)) " " (format-atom (third t))))
        (else
         (format-atom t)))
      (format-atom t)))

(define (format-atom t)
  (if (pair? t)
      (case (car t)
        ((var) (second t))
        ((if let lambda typed-lambda succ pred iszero fix app)
         (string-append "(" (format-abs t) ")"))
        (else (raise "invalid term")))
      (case t
        ((zero) "0")
        ((true false) (symbol->string t))
        (else (raise "invalid term")))))

(define format-term format-abs)

(define (format-arrow tau)
  (if (and (pair? tau) (equal? (first tau) 'arrow))
      (string-append (format-base (second tau)) "->" (format-arrow (third tau)))
      (format-base tau)))

(define (format-base tau)
  (if (pair? tau)
      (case (first tau)
        ((typevar) (second tau))
        ((arrow) (string-append "(" (format-arrow tau) ")"))
        (else (raise "invalid type")))
      (case tau
        ((Nat) "Nat")
        ((Bool) "Bool")
        (else (raise "invalid base type")))))

(define format-type format-arrow)

;;; Simple read-eval-print loop

(define (read-lines prompt f)
  (let ((line (get-line (current-input-port))))
        (if (not (eof-object? line))
            (begin
              (f line)
              (read-lines prompt f))
            (newline))))
