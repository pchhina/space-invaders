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
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

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
             (advance-missiles (game-missiles s))
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

;; (listof Missile) -> (listof Missile)
;; advance each missile in the list upward by MISSILE-SPEED
;; !!!

(define (advance-missiles lom) (list M1))


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


