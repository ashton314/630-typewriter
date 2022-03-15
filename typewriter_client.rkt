#lang racket

(require "typewriter_core.rkt"
         "typewriter_storage.rkt")

; story-node string string -> story-node
(define (new-installment parent-node author text)
  (node 0 text parent-node author))

(define (prompt txt)
  (printf "~a: " txt)
  (read-line))

(define (write-installment gm parent-node)
  (let* ([author (prompt "Author")]
         [story (prompt "Story")]
         [new-node (new-installment parent-node author story)])
    (pretty-print new-node)
    (fetch-node-by-id (game-add-node gm new-node) (node-id new-node))))

(define (story-repl leaf gm)
  (displayln (if (root? leaf)
                 (format "Prompt: ~a" (root-prompt leaf))
                 (format "[~a]: ~a\n" (node-author leaf) (node-text leaf))))
  (let ([children (set->list (children-of leaf gm))])
    (for ([chld children]
          [i (in-naturals)])
      (printf "(~a) [~a]: ~a\n" i (node-author chld) (node-text chld)))
    (let ([action (prompt "\n(0-9: move; r: read; u: up; i: write; q: quit)")])
      (match action
        ["q" (displayln "Auf Wiedersehen!")]
        ["i" (let ([new-leaf (write-installment gm leaf)])
               (story-repl new-leaf (hydrate-game (game-id gm))))]
        ["u" (story-repl (node-parent leaf) gm)] ; bug here: what if at root?
        ["r" (map displayln (read-story leaf))
             (story-repl leaf gm)]
        [(pregexp #px"^[0-9]+$" (list x))
         (story-repl (car (drop children (string->number x))) gm)]
        [_ (story-repl leaf)]))))

(define (begin-game prompt [gid #f])
  (init-db!)
  ;; We have to get the game after calling init-db!
  (let* ([the-game-id (or gid (new-game!))]
         [the-root (new-node! the-game-id prompt "root" #t)])
    (printf "New game id: ~a\n" the-game-id)
    (printf "Root ID: ~a\n" the-root)
    (let* ([the-game (hydrate-game the-game-id)]
           [root-node (fetch-node-by-id the-game the-root)])
      (story-repl root-node (hydrate-game the-game-id)))))

;; (story-repl (root "Beginning something epic..."))
