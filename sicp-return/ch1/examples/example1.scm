;; (display) fn is using to display result in REPL
;; (newline) fn will make readable result
;; Bash command to start scheme REPL==> $ rlwrap scheme
;; load script in REPL==> 1 ]=> (load "example1.scm")

(newline)

(display (+ 2 6))

(newline)

(display (+ (* 3
               (+ (* 2 4)
                  (+ 3 5)))
            (+ (- 10 7)
               6)))
