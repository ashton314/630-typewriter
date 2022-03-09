#lang typed/racket

(define-type Story-Node (U root node))
(struct root ([prompt : String]) #:transparent)
(struct node ([text : String] [parent : Story-Node]) #:transparent)

(: read-story-match (-> Story-Node (Listof String)))
(define (read-story-match nd)
  (match nd
    [(root a) (list a)]
    [(node txt p)
     (append (read-story-match p) (list txt))]))

(: read-story-match-partial (-> Story-Node (Listof String)))
(define (read-story-match-partial nd)
  (match nd
    [(node txt p)
     (append (read-story-match-partial p) (list txt))]))

(: read-story (-> Story-Node (Listof String)))
(define (read-story nd)
  (cond
    [(root? nd)
     (list (root-prompt nd))]
    [(node? nd)
     (append (read-story (node-parent nd)) (list (node-text nd)))]))

(define-type (Maybe a) (U None (Some a)))
(struct None ())
(struct (a) Some ([v : a]))

