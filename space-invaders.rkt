;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Game -> Game
(define (main g)
  (big-bang g                    ; Game
    (on-tick next-game-state)    ; Game -> Game
    (to-draw render-game)        ; Game -> Image
    (on-key control-tank)        ; Game kevt -> Game
    (stop-when invader-landed?))); Game -> Boolean

;; Game -> Game
;; produces next game state after a clock tick
(check-expect (next-game-state G0) G0) ;only tank
(check-expect (next-game-state G1) G1) ;only tank
(check-expect (next-game-state G2) (make-game
                                    (list (make-invader (+ (invader-x I1)
                                                           (invader-dx I1))
                                                        (+ (invader-y I1)
                                                           INVADER-Y-SPEED)
                                                        (invader-dx I1)))
                                    (list (make-missile (missile-x M1)
                                                        (- (missile-y M1)
                                                           MISSILE-SPEED)))
                                    T1))

;(define (next-game-state s) G0) ;stub

(define (next-game-state s)
  (make-game (advance-invaders (game-invaders s))
             (advance-missiles (game-missiles s) (game-invaders s))
             (game-tank s)))

;; (listof Invader) -> (listof Invader)
;; advance each invader in the list by (invader-dx) and INVADER-Y-SPEED
(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list I1)) (list (make-invader (+ (invader-x I1)
                                                                  (invader-dx I1))
                                                               (+ (invader-y I1)
                                                                  INVADER-Y-SPEED)
                                                               (invader-dx I1))))
(check-expect (advance-invaders (list I1 I2)) (list (make-invader (+ (invader-x I1)
                                                                     (invader-dx I1))
                                                                  (+ (invader-y I1)
                                                                     INVADER-Y-SPEED)
                                                                  (invader-dx I1))
                                                    (make-invader (+ (invader-x I2)
                                                                     (invader-dx I2))
                                                                  (+ (invader-y I2)
                                                                     INVADER-Y-SPEED)
                                                                  (invader-dx I2))))

;(define (advance-invaders loi) (list I1)) stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (advance-invader (first loi))
                    (advance-invaders (rest loi)))]))

;; Invader -> Invader
;; advances a single invader by (invader-dx) and INVADER-Y-SPEED
(check-expect (advance-invader I1)  (make-invader (+ (invader-x I1)
                                                     (invader-dx I1))
                                                  (+ (invader-y I1)
                                                     INVADER-Y-SPEED)
                                                  (invader-dx I1)))

;(define (advance-invader i) I1)

(define (advance-invader invader)
  (make-invader (+ (invader-x invader)
                   (invader-dx invader))
                (+ (invader-y invader)
                   INVADER-Y-SPEED)
                (invader-dx invader)))

;; (listof Missile) (listof Invader) -> (listof Missile)
;; advance each missile in the list upward by MISSILE-SPEED,
;;  remove the missile from list if it goes past the top or hits invader
(check-expect (advance-missiles empty empty) empty) ;base case
(check-expect (advance-missiles empty (list I1 I2)) empty) ; base case
(check-expect (advance-missiles (list M1 M2) (list I2 I3)) ; not hitting ceiling or invader
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))
                    (make-missile (missile-x M2)
                                  (- (missile-y M2) MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 M2) (list I1 I2)) ; M2 hitting I1
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 M3) (list I1 I2)) ; M3 past I1
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 (make-missile 150 0)) (list I2 I3)) ; missile hitting top
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 (make-missile 150 (-(image-height MISSILE)))) ; missile past top
                                (list I2 I3))
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))))

;(define (advance-missiles lom loi) (list M1))

(define (advance-missiles lom loi)
  (remove-missiles (move-missiles lom) loi))

;; (listOf Missile) -> (listof Missile)
;; produces a (listof Missile) by advancing each missile by MISSILE-SPEED in input list
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles (list M1))
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))))
(check-expect (move-missiles (list M1 M2))
              (list (make-missile (missile-x M1)
                                  (- (missile-y M1) MISSILE-SPEED))
                    (make-missile (missile-x M2)
                                  (- (missile-y M2) MISSILE-SPEED))))

;(define (move-missiles lom) (list M1)) ;stub

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (move-missile (first lom))
                    (move-missiles (rest lom)))]))

;; Missile -> Missile
;; move missile by MISSILE-SPEED

;(define (move-missile m) M1) ;stub

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (or (missile-exit? (first lom))
                      (missile-hit? (first lom) loi))
                  (remove-missiles (rest lom) loi)
                  (cons (first lom)
                        (remove-missiles (rest lom) loi)))]))

;; Missile -> Boolean
;; produces true if the given missile has exited from top
(check-expect (missile-exit? M1) false)
(check-expect (missile-exit? (make-missile 150 (-(image-height MISSILE)))) true)

;(define (missile-exit? m) false)

(define (missile-exit? m)
  (< (missile-y m) 0))

;; Missle (listof Invader) -> Boolean
;; produces true if the given missile is within the hit range of given list of invaders
(check-expect (missile-hit? M1 empty) false)
(check-expect (missile-hit? M2 (list I1 I2 I3)) true)
(check-expect (missile-hit? M3 (list I2 I3)) false)
(check-expect (missile-hit? M3 (list I1 I3)) true)

;(define (missile-hit? m loi) false) ;stub

(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (or
               (in-range? m (first loi))
               (missile-hit? m (rest loi)))]))

;; Missile Invader -> Boolean
;; produces true if the given missile is within hit range of given invader
(check-expect (in-range? M1 I1) false)
(check-expect (in-range? M2 I1) true)
(check-expect (in-range? M3 I1) true)

;(define (in-range? m i) false)

(define (in-range? m i)
  (<= (sqrt (+ (sqr (- (missile-x m) (invader-x i)))
               (sqr (- (missile-y m) (invader-y i)))))
      HIT-RANGE))



;; Game -> Image
;; renders given game state as image
;; !!!

(define (render-game g) BACKGROUND) ;stub

;; Game kevt -> Game
;; produces next game state by moving tank left/right on left/right arrow keys
;;   shoots missiles whne spacebar is pressed
;; !!!

(define (control-tank g kevt) G0) ;stub

;; Game -> Boolean
;; produces true if invader reaches land
;; !!!

(define (invader-landed? g) false) ;stub


