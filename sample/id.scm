(use-modules (srfi srfi-34))

(load "syntax.scm")

(read-lines "id> "
            (lambda (input)
              (let ((output
                     (guard (exn (else exn))
                            (format-term (parse-term input)))))
                (display output)
                (newline))))
