1 ]=> (list 'a 'b 'c)

;Value 11: (a b c)

1 ]=> (list (list 'george))

;Value 12: ((george))

1 ]=> (cdr '((x1 x2) (y1 y2)))

;Value 13: ((y1 y2))

1 ]=> (cadr '((x1 x2) (y1 y2)))

;Value 14: (y1 y2)

1 ]=> (pair? (car '(a short list)))

;Value: #f

1 ]=> (memq 'red '((red shoes) (blue socks)))

;Value: #f

1 ]=> (memq 'red '(red shoes blue socks))

;Value 15: (red shoes blue socks
