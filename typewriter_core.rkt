#lang typed/racket

(provide (all-defined-out))

(require/typed "typewriter_storage.rkt"
  [nodes-from-game (-> Integer (Listof (Vector Integer String Integer String (U One Zero))))]
  [new-node! (-> NodeId String String Boolean NodeId)])

(define-type StoryNode (U root node))
(define-type NodeId Integer)
(define-type GameId Integer)
(struct root ([id : NodeId] [prompt : String]) #:transparent)
(struct node ([id : NodeId] [text : String] [parent : StoryNode] [author : String]) #:transparent)
(struct game ([id : GameId] [nodes : (Listof StoryNode)]) #:transparent)

;;; Read the story starting at the root and going down to this leaf node.
(: read-story (-> StoryNode (Listof String)))
(define (read-story nd)
  (cond
    [(root? nd)
     (list (root-prompt nd))]
    [(node? nd)
     (append (read-story (node-parent nd)) (list (node-text nd)))]))

;;; Read *all* the stories!
(: gather-game-stories (-> game (Listof (Listof String))))
(define (gather-game-stories game)
  (map read-story (game-nodes game)))

;;; Add a node to the game (with persistance)
(: game-add-node (-> game node game))
;; (define (game-add-node gm nd)
;;   (let ([new-node-id (new-node! (parent-id nd) (node-text nd) (node-author nd) #f)])
;;     (game (game-id gm)
;;           (cons (node new-node-id (node-text nd) (node-parent nd) (node-author nd))
;;                 (game-nodes gm)))))
(define (game-add-node gm nd)
  (new-node! (parent-id nd) (node-text nd) (node-author nd) #f)
  ;; Now re-fetch the game from the db
  (hydrate-game (game-id gm)))

(: parent-id (-> node NodeId))
(define (parent-id nd)
  (let ([prt (node-parent nd)])
    (if (node? prt)
        (node-id prt)
        (root-id prt))))

(: parents (-> game (Setof StoryNode)))
(define (parents gm)
  (: loop (-> (Listof StoryNode) (Setof StoryNode) (Setof StoryNode)))
  (define (loop nodes known-parents)
    (let ([nd (car nodes)])             ; Gotta have this let for occurence typing
      (if (root? nd)
          (loop (cdr nodes) (set-add known-parents nd))
          (set-add known-parents (node-parent nd)))))
  (loop (game-nodes gm) (set)))

(: fold-parents (-> game (Setof StoryNode)))
(define (fold-parents gm)
  (foldl
   (λ ([nd : StoryNode] [known-parents : (Setof StoryNode)])
     (if (root? nd)
         (set-add known-parents nd)
         (set-add known-parents (node-parent nd))))
   (let ([n1 (car (game-nodes gm))])
     ;; If this set is empty, it won't typecheck
     (set (if (root? n1) n1 (node-parent n1))))
   (game-nodes gm)))

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

;;; Database interaction
(: hydrate-game (-> Integer game))
(define (hydrate-game game-id)
  (let ([all-nodes : (HashTable NodeId StoryNode) (make-hash)])
  (game
   game-id
   (for/list ([n (nodes-from-game game-id)])
     (match n
       [(vector id txt prnt auth rootp)
        (let ([me (if (= 1 rootp)
                      (root id txt)
                      (node id txt (hash-ref all-nodes prnt) auth))])
          (hash-set! all-nodes id me)
          me)])))))
