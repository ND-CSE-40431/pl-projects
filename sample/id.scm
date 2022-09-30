#!/usr/bin/env scheme --script

(load "syntax.scm")

(read-lines "id> "
            (lambda (input)
              (let ((output
                     (guard (exn (else (string-append "error: " exn)))
                            (format-term (parse-term input)))))
                (display output)
                (newline))))
