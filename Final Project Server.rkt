#lang htdp/isl+
(require 2htdp/universe)
(require 2htdp/abstraction)

;;Game Constants


(define ROPE-LENGTH 20)
(define INIT-POSITION (quotient ROPE-LENGTH 2))

;; A SState is a (make-state (ListOf World) Integer)
;; Representing the worlds in te game and the current knot position, and
(define-struct sstate (worlds knot))

(define INIT-USTATE (make-sstate (list) INIT-POSITION))

;; Signup
;; sign up the given world by adding it to the state
;; alerting that player to their team
(define (handle-signup a-ss an-iw)
  (make-bundle
     (make-sstate (cons an-iw (sstate-worlds a-ss)) (sstate-knot a-ss))
     (list (make-mail an-iw (list "team-and-position" (= (random 2) 1) (sstate-knot a-ss))))
     '()))

;; iworld1: Used for testing
(check-expect
 (bundle? (handle-signup INIT-USTATE iworld1)) #t)

;; game-is-over
;; Triggers the game over screen
(define (game-is-over a-ss n-knot)
  (make-bundle
          (make-sstate (sstate-worlds a-ss) n-knot)
          (map (lambda (aw)
                 (make-mail aw (list "GAME OVER" n-knot (= 0 n-knot))))
               (sstate-worlds a-ss))
         '()))

;; new-sstate
(define (new-sstate a-ss new-knot)
  (make-bundle
          (make-sstate (sstate-worlds a-ss) new-knot)
          (map (lambda (aw)
                 (make-mail aw (list "knot-position" new-knot)))
               (sstate-worlds a-ss))
         '()))

;; edge?: Number -> Boolean
;; To determine if the knot is at the edge
(define (edge? n-knot)
  (or
   (= 0 n-knot)
   (= 19 n-knot)))

;; Testing
(check-expect (edge? 1) #f)
(check-expect (edge? 10) #f)
(check-expect (edge? 19) #t)

;; handle-message
(define (handle-message a-ss an-iw msg)
  (match msg
    [(list "move" left?) ;; Should we move left?
     (local ([define new-knot
               (if left?
                   (sub1 (sstate-knot a-ss))
                   (add1 (sstate-knot a-ss)))])
       (if (eq? (edge? new-knot) #t)
           (game-is-over a-ss new-knot)
           (new-sstate a-ss new-knot)))]))

;swap-teams
;; To swap a play's team
(define (swap-teams a-ss)
  (make-bundle
           a-ss
          (map (lambda (w)
                 (make-mail w (list "swap-teams")))
               (sstate-worlds a-ss))
         '()))

;; process-tick
;; To determine whether the player swaps teams
(define (process-tick a-ss)
  (if
      (= 0 (random 3))
      (swap-teams a-ss)
      a-ss))

;; String -> USTATE
;; Given a name, returns a final game state
;; EFFECT: runs a server for a tug-of-war game
(define (run-server a-name)
  (universe INIT-USTATE
    [on-new handle-signup]
    [on-msg handle-message]
    [on-tick process-tick 4]))

(run-server "tug-of-war")