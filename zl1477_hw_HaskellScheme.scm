(define ans '())
(define (hw l1 l2)
  (define b1 (list-copy l1))
  (define b2 (list-copy l2))
  (set! ans '())
  (if (or (null? l1) (null? l2))
      ans
      (o l1 l2 b1 b2))
  ans)

(define (o l1 l2 b1 b2)
  (define i1 '())
  (if (null? l1)
      '()
      (begin
	(set! i1 (car l1))
        (i i1 l2 (cdr l1) b2))))

(define (i i1 l2 b1 b2)
  (if (null? l2)
      (o b1 b2 b1 b2)
      (begin
	(set! ans (append ans (list (list i1 (car l2)))))
        (i i1 (cdr l2) b1 b2))))
