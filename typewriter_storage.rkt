#lang racket

(require db)

(provide (all-defined-out))

(define *game-store* (make-parameter "./game_store.sqlite"))

(define *dbh* null)
(define (get-db-instance!)
  (when (null? *dbh*)
    (set! *dbh* (sqlite3-connect #:database (*game-store*) #:mode 'create)))
  *dbh*)

(define (init-db!)
  (let ([dbh (get-db-instance!)])
    (query-exec dbh
                "create table if not exists games (id integer primary key autoincrement, name text)")
    (query-exec dbh
                "create table if not exists nodes (id integer primary key autoincrement, story_text text, parent integer, author text, is_root boolean default false)")
    ))

;; returns game id
(define (new-game! [name "game"])
  (query-value (get-db-instance!)
               "insert into games (name) values ($1) returning id" name))

;; returns node id
(define (new-node! parent-id text author root?)
  (query-value (get-db-instance!)
               "insert into nodes ('parent', 'story_text', 'author', 'is_root') values ($1, $2, $3, $4) returning id" parent-id text author (if root? 1 0)))

(define (list-nodes)
  (query-rows (get-db-instance!)
              "select * from nodes"))

(define (nodes-from-game story-id)
  (query-rows (get-db-instance!)
              "WITH RECURSIVE node_of(n) AS (VALUES($1) UNION SELECT id FROM nodes, node_of WHERE nodes.parent = node_of.n) SELECT * FROM nodes WHERE id IN node_of" story-id))
