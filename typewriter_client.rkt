#lang racket

(require "typewriter_core.rkt"
         "typewriter_storage.rkt")

(define (prompt txt)
  (printf "~a: " txt)
  (read-line))


(define (story-repl leaf gm)
  (displayln (if (root? leaf)
                 (format "Prompt: ~a" (root-prompt leaf))
                 (format "[~a]: ~a\n" (node-author leaf) (node-text leaf))))
  (let ([children (set->list (children-of leaf gm))])
    (for ([chld children]
          [i (in-naturals)])
      (printf "(~a) [~a]: ~a\n" i (node-author chld) (node-text chld)))
    (let ([action (prompt "\n(0-9: move; r: read; u: up; i: write; q: quit)")])
      (printf "\n-----\n")
      (match action
        ["q" (displayln "Auf Wiedersehen!")]
        ["i" (let-values ([(new-leaf new-gm) (add-story leaf gm)])
               (story-repl new-leaf new-gm))]
        ["u" (story-repl (node-parent leaf) gm)] ; bug here: what if at root?
        ["r" (map displayln (read-story leaf))
             (story-repl leaf gm)]
        [(pregexp #px"^[0-9]+$" (list x))
         (story-repl (car (drop children (string->number x))) gm)]
        [_ (story-repl leaf (refresh-game gm))]))))

(define (add-story parent gm)
  (let ([auth (prompt "Author")]
        [txt (prompt "Story")])
    (match (game-add-node gm txt auth parent)
      [(cons new-leaf new-game)
       (values new-leaf new-game)])))

(define (begin-new-game starting-prompt)
  (init-db!)
  (let* ([gm-id (new-game!)]
         [game-info (game-add-node (hydrate-game gm-id) starting-prompt "nobody" #f)])
    (story-repl (car game-info) (cdr game-info))))

(define (resume-game gid)
  (let ([game (hydrate-game gid)])
    (story-repl (car (set->list (leaves game))) game)))

(define (refresh-game gm)
  (hydrate-game (game-id gm)))

;; (story-repl (root "Beginning something epic..."))
