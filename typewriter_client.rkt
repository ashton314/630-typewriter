#lang racket

(require "typewriter_core.rkt")

; story-node string string -> story-node
(define (new-installment leaf-node author text)
  (node text leaf-node author))

(define (prompt txt)
  (printf "~a: " txt)
  (read-line))

(define (write-installment node)
  (let ([author (prompt "Author")]
        [story (prompt "Story")])
    (new-installment node author story)))

(define (story-repl [leaf (root)] [gm (game (list leaf))])
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
        ["i" (let ([new-leaf (write-installment leaf)])
               (story-repl new-leaf (game-add-node gm new-leaf)))]
        ["u" (story-repl (node-parent leaf) gm)] ; bug here: what if at root?
        ["r" (map displayln (read-story leaf))
             (story-repl leaf gm)]
        [(pregexp #px"^[0-9]+$" (list x))
         (story-repl (car (drop children (string->number x))) gm)]
        [_ (story-repl leaf)]))))

;; (story-repl (root "Beginning something epic..."))
