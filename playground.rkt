#lang typed/racket

;;; Example of union types
(define-type (Maybe a) (U None (Some a)))
(struct None ())
(struct (a) Some ([v : a]))

;;; Polymorphism
(: my-map (All (A B) (-> (-> A B) (Listof A) (Listof B))))
(define (my-map fn lst)
  (if (null? lst)
      lst
      (cons (fn (car lst)) (my-map fn (cdr lst)))))

;;; Non-uniform variable-arity function
(: my-foldl (All (Init/Res A B ...)
                 (->
                  (-> Init/Res A B ... B Init/Res)
                  Init/Res
                  (Listof A)
                  (Listof B) ... B
                  Init/Res)))
(define (my-foldl fn start as . bss)
  (if (or (null? as)
          (ormap null? bss))
      start
      (apply my-foldl
             fn
             (apply fn start (car as) (map car bss))
             (cdr as)
             (map cdr bss))))

;; Try running:
;; (inst my-foldl Integer Boolean String Number String String Number)
