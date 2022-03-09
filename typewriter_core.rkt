#lang typed/racket

(provide (all-defined-out))

(define-type StoryNode (U root node))
(struct root ([prompt : String]) #:transparent)
(struct node ([text : String] [parent : StoryNode] [author : String]) #:transparent)
(struct game ([nodes : (Listof StoryNode)]) #:transparent)

(: read-story (-> StoryNode (Listof String)))
(define (read-story nd)
  (cond
    [(root? nd)
     (list (root-prompt nd))]
    [(node? nd)
     (append (read-story (node-parent nd)) (list (node-text nd)))]))

(: gather-game-stories (-> game (Listof (Listof String))))
(define (gather-game-stories game)
  (map read-story (game-nodes game)))

(: game-add-node (-> game node game))
(define (game-add-node gm nd)
  (game (cons nd (game-nodes gm))))

(: parents (-> game (Setof StoryNode)))
(define (parents gm)
  (: loop (-> (Listof StoryNode) (Setof StoryNode) (Setof StoryNode)))
  (define (loop nodes known-parents)
    (let ([nd (car nodes)])             ; Gotta have this let for occurence typing
      (if (root? nd)
          (loop (cdr nodes) (set-add known-parents nd))
          (set-add known-parents (node-parent nd)))))
  (loop (game-nodes gm) (set)))

(: leaves (-> game (Setof StoryNode)))
(define (leaves gm)
  (let ([nodes-with-kids (parents gm)])
    (set-symmetric-difference nodes-with-kids (list->set (game-nodes gm)))))

(: children-of (-> StoryNode game (Setof StoryNode)))
(define (children-of p gm)
  (list->set (filter (λ ([n : StoryNode]) (has-direct-parent? n p)) (game-nodes gm))))

(: has-direct-parent? (-> StoryNode StoryNode Boolean))
(define (has-direct-parent? nd maybe-parent)
  (and (not (root? nd))
       (eq? (node-parent nd) maybe-parent)))

(: has-ancestor? (-> StoryNode StoryNode Boolean))
(define (has-ancestor? nd maybe-grandpa)
  (cond
    [(root? nd) #f]
    [(eq? nd maybe-grandpa) #t]
    [(eq? (node-parent nd) maybe-grandpa) #t]
    [else (has-ancestor? (node-parent nd) maybe-grandpa)]))
