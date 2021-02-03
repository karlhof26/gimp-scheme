;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; GIMP - The GNU Image Manipulation Program                               ;;; 
;;; Copyright (C) 1995 Spencer Kimball and Peter Mattis                     ;;;
;;;                                                                         ;;;
;;; This program is free software: you can redistribute it and/or modify    ;;;
;;; it under the terms of the GNU General Public License as published by    ;;;
;;; the Free Software Foundation, either version 3 of the License, or       ;;;
;;; (at your option) any later version.                                     ;;;
;;;                                                                         ;;;
;;; This program is distributed in the hope that it will be useful,         ;;;
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of          ;;;
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ;;;
;;; GNU General Public License for more details.                            ;;;
;;;                                                                         ;;;
;;; You should have received a copy of the GNU General Public License       ;;;
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.   ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; Crossword Grid                                                          ;;;
;;;                                                                         ;;;
;;; crossword-grid.scm - version 1.02                                       ;;;
;;; Copyright (C) 2011-2014 Gino D                                          ;;;
;;; https://sites.google.com/site/ginodonig/gimp-scripts                    ;;;
;;;                                                                         ;;;
;;; This script creates an image filled with a texture representing a       ;;;
;;; random crossword grid.                                                  ;;;
;;;                                                                         ;;;
;;; ....................................................................... ;;;
;;;                                                                         ;;;
;;; VERSION HISTORY                                                         ;;;
;;;                                                                         ;;;
;;; 1.00 - August 2011                                                      ;;;
;;;  * First release.                                                       ;;;
;;;                                                                         ;;;
;;; 1.01 - June 2013                                                        ;;;
;;;  * Made the script fully compatible with GIMP 2.8, while maintaining    ;;;
;;;    the backward compatibility with the previous versions beginning from ;;;
;;;    GIMP 2.6.10.                                                         ;;;
;;;  * Added the ability to explicitly set the random seed controlling the  ;;;
;;;    randomness of the arrangement of the shaded cells, while keeping the ;;;
;;;    possibility to let the script generate the seed automatically.       ;;;
;;;  * The previous method of randomization based on the "RGB Noise" filter ;;;
;;;    has been discarded in favor of a new block of code consisting only   ;;;
;;;    of Scheme commands and internal GIMP procedures.                     ;;;
;;;  * Added the ability to put a border around the whole crossword grid.   ;;;
;;;  * Now the script records a summary of the crossword grid's features    ;;;
;;;    into the image comment of the generated pattern.                     ;;;
;;;  * Lots of enhancements, cleanups and minor bug fixes regarding the     ;;;
;;;    code, the dialog window and the appearance of the pattern.           ;;;
;;;                                                                         ;;;
;;; 1.02 - May 2014                                                         ;;;
;;;  * Added the ability to automatically recover and reuse the random seed ;;;
;;;    associated with the last crossword grid created.                     ;;;
;;;  * Rearranged the sequence of the options in the dialog window.         ;;;
;;;  * Fixed some minor bugs; enhanced and cleaned up the code.             ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *crossword-grid-2.8.0* 1)

(let* ((gimp-version-comparison
        (lambda (predicate version-a version-b)
          (letrec ((func
                    (lambda (string1 p q)
                      (cond ((= (string-length string1) p)
                             (* (expt 100 q) (string->atom string1)))
                            ((eqv? (string-ref string1 p)
                                   (integer->char 46))
                             (+ (* (expt 100 q)
                                   (string->atom (substring string1 0 p)))
                                (func (substring string1
                                                 (+ p 1)
                                                 (string-length string1))
                                      0
                                      (- q 1))))
                            (else (func string1 (+ p 1) q))))))
            (predicate (func version-a 0 2)
                       (func version-b 0 2))))))
  (cond ((gimp-version-comparison < (car (gimp-version)) "2.6.10")
         (quit))
        ((gimp-version-comparison < (car (gimp-version)) "2.8.0")
         (set! *crossword-grid-2.8.0* 0))))

(define *cw-seed* 1)

(if (not (defined? (quote *crossword-seed*)))
    (define *crossword-seed* 1431655764))

(define (script-fu-crossword-grid num-columns
                                  num-rows
                                  cell-width
                                  cell-height
                                  light-color
                                  block-color
                                  border-color
                                  cell-border-tn
                                  grid-border-tn
                                  block-margins
                                  max-ans-length
                                  ensure-symmetry
                                  add-clue-numbers
                                  digit-size
                                  text-color
                                  text-font
                                  random-behavior
                                  specific-seed
                                  separate-layers)
  (cond ((= *crossword-grid-2.8.0* 0)
         (define (gimp-context-get-sample-threshold-int))
         (define (gimp-context-set-sample-threshold-int sample-threshold)
           (set! gimp-context-get-sample-threshold-int
                 (lambda () (cons sample-threshold ()))))
         (define (gimp-context-get-antialias))
         (define (gimp-context-set-antialias antialias)
           (set! gimp-context-get-antialias
                 (lambda () (cons antialias ()))))
         (define (gimp-context-get-feather))
         (define (gimp-context-set-feather feather)
           (set! gimp-context-get-feather
                 (lambda () (cons feather ()))))
         (define (gimp-context-get-feather-radius))
         (define (gimp-context-set-feather-radius feather-radius-x
                                                  feather-radius-y)
           (set! gimp-context-get-feather-radius
                 (lambda ()
                   (cons feather-radius-x
                         (cons feather-radius-y
                               ())))))
         (define (gimp-context-get-sample-merged))
         (define (gimp-context-set-sample-merged sample-merged)
           (set! gimp-context-get-sample-merged
                 (lambda () (cons sample-merged ()))))
         (define (gimp-context-get-sample-transparent))
         (define (gimp-context-set-sample-transparent sample-transparent)
           (set! gimp-context-get-sample-transparent
                 (lambda () (cons sample-transparent ()))))
         
         (define (gimp-context-get-sample-criterion))
         
         (define (gimp-context-set-sample-criterion sample-criterion)
           (set! gimp-context-get-sample-criterion
                 (lambda () (cons sample-criterion ())))
         )
         
         (define (gimp-context-get-interpolation))
         
         (define (gimp-context-set-interpolation interpolation)
           (set! gimp-context-get-interpolation
                 (lambda () (cons interpolation ())))
         )
         
         (define (gimp-context-get-transform-resize))
         
         (define (gimp-context-set-transform-resize transform-resize)
           (set! gimp-context-get-transform-resize
                 (lambda () (cons transform-resize ())))
         )
         
         (define (gimp-image-attach-parasite image parasite)
           (gimp-image-parasite-attach image parasite)
         )
         
         (define (gimp-image-insert-layer image layer parent position)
           (gimp-image-add-layer image layer position)
         )
         
         (define (gimp-item-transform-rotate-simple item
                                                    rotate-type
                                                    auto-center
                                                    center-x
                                                    center-y)
           (gimp-drawable-transform-rotate-simple
            item
            rotate-type
            auto-center
            center-x
            center-y
            (car (gimp-context-get-transform-resize)))
         )
         (define (gimp-image-select-rectangle image
                                              operation
                                              x
                                              y
                                              width
                                              height)
           
           (gimp-select-rectangle
            image
            operation
            x
            y
            width
            height
            ;operation
            ;(car (gimp-context-get-feather))
            ;(/ (apply + (gimp-context-get-feather-radius)) 2)
           )
           
           
         )
         
         (define (gimp-image-select-polygon image operation num-segs segs)
           (gimp-free-select
            image
            num-segs
            segs
            operation
            (car (gimp-context-get-antialias))
            (car (gimp-context-get-feather))
            (/ (apply + (gimp-context-get-feather-radius)) 2)
           )
         )
         
         (define (gimp-item-transform-flip-simple item
                                                  flip-type
                                                  auto-center
                                                  axis)
           (gimp-drawable-transform-flip-simple
            item
            flip-type
            auto-center
            axis
            (car (gimp-context-get-transform-resize))
           )
         )
         (define (gimp-image-select-contiguous-color image
                                                     operation
                                                     drawable
                                                     x
                                                     y)
           (gimp-fuzzy-select-full
            drawable
            x
            y
            (car (gimp-context-get-sample-threshold-int))
            operation
            (car (gimp-context-get-antialias))
            (car (gimp-context-get-feather))
            (car (gimp-context-get-feather-radius))
            (car (cdr (gimp-context-get-feather-radius)))
            (car (gimp-context-get-sample-merged))
            (car (gimp-context-get-sample-transparent))
            (car (gimp-context-get-sample-criterion))))
         (define (gimp-image-select-color image
                                          operation
                                          drawable
                                          color)
           (gimp-by-color-select-full
            drawable
            color
            (car (gimp-context-get-sample-threshold-int))
            operation
            (car (gimp-context-get-antialias))
            (car (gimp-context-get-feather))
            (car (gimp-context-get-feather-radius))
            (car (cdr (gimp-context-get-feather-radius)))
            (car (gimp-context-get-sample-merged))
            (car (gimp-context-get-sample-transparent))
            (car (gimp-context-get-sample-criterion)))
         )
         (define (gimp-layer-scale layer new-width new-height local-origin)
           (gimp-layer-scale-full
            layer
            new-width
            new-height
            local-origin
            (car (gimp-context-get-interpolation)))
         )
         (define (gimp-item-set-name item name)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-set-name item name)
               (gimp-drawable-set-name item name)
           )
         )
         (define (gimp-image-select-item image operation item)
           (cond ((= (car (gimp-drawable-is-layer item)) TRUE)
                  (gimp-selection-layer-alpha item))
                 ((= (car (gimp-drawable-is-channel item)) TRUE)
                  (gimp-selection-combine item operation))
                 ((= (car (gimp-vectors-is-valid item)) TRUE)
                  (gimp-vectors-to-selection
                   item
                   operation
                   (car (gimp-context-get-antialias))
                   (car (gimp-context-get-feather))
                   (car (gimp-context-get-feather-radius))
                   (car (cdr (gimp-context-get-feather-radius)))))
           )
         )
        )
       )
  (define (round2 num int)
    (let* ((rnd (/ (truncate (+ (* (expt 10 int) num)
                                (if (< num 0) -0.5 0.5)))
                   (expt 10 int))))
      (if (= int 0)
          (inexact->exact rnd)
          rnd)))
  (define (color-notations color)
    (let* ((func (lambda (x)
                   (string-append
                    (make-string 1
                                 (string-ref "0123456789ABCDEF"
                                             (quotient x 16)))
                    (make-string 1
                                 (string-ref "0123456789ABCDEF"
                                             (remainder x 16)))))))
      (string-append "#"
                     (func (car color))
                     (func (car (cdr color)))
                     (func (car (cdr (cdr color))))
                     " - "
                     "RGB("
                     (atom->string (car color))
                     ","
                     (atom->string (car (cdr color)))
                     ","
                     (atom->string (car (cdr (cdr color))))
                     ")")))
  (define (block-amount x y z)
    (+ (/ 3 16)
       (* (/ 2 3)
          (+ (* (- (/ x z) 1)
                (ceiling (/ y 4))
                (if (< z x) 1 0))
             (* (- (/ y z) 1)
                (ceiling (/ x 4))
                (if (< z y) 1 0)))
          (/ 1 (* x y)))))
  ;;
  ;; random number routines
  (define (srand seed)
    (set! *cw-seed* seed)
    *cw-seed*)
  (define (msrg-rand)
    (let ((A 48271)
          (M 2147483647)
          (Q 44488)
          (R 3399))
      (let* ((hi (quotient *cw-seed* Q))
             (lo (modulo *cw-seed* Q))
             (test (- (* A lo) (* R hi))))
        (if (> test 0)
            (set! *cw-seed* test)
            (set! *cw-seed* (+ test M)))))
    *cw-seed*)
  (define (random n)
    (let* ((n (inexact->exact (truncate n)))
           (M 2147483647)
           (slop (modulo M n))
          )
      (let loop ((r (msrg-rand)))
        (if (> r slop)
            (modulo r n)
            (loop (msrg-rand))
        )
      )
    )
  )
  ;;
  (let* ((x1 (round2 num-columns 0))
         (y1 (round2 num-rows 0))
         (cell-width (round2 cell-width 0))
         (cell-height (round2 cell-height 0))
         (cell-border-tn (round2 cell-border-tn 0))
         (grid-border-tn (round2 grid-border-tn 0))
         (block-margins (min (round2 block-margins 0)
                             (- (ceiling (/ (min cell-width cell-height) 2))
                                (+ cell-border-tn 1))))
         (max-ans-length (round2 max-ans-length 0))
         (digit-size (round2 digit-size 0))
         (x2 (if (and (= ensure-symmetry TRUE)
                      (>= x1 y1))
                 (ceiling (/ x1 2))
                 x1))
         (y2 (if (and (= ensure-symmetry TRUE)
                      (not (>= x1 y1)))
                 (ceiling (/ y1 2))
                 y1))
         (x3 (inexact->exact (ceiling (/ x2 4))))
         (y3 (inexact->exact (ceiling (/ y2 4))))
         (x4 (if (= ensure-symmetry TRUE)
                 (ceiling (/ x1 2))
                 x1))
         (y4 (if (= ensure-symmetry TRUE)
                 (ceiling (/ y1 2))
                 y1))
         (image1 (car (gimp-image-new x1 y1 RGB)))
         (layer1
          (car
           (gimp-layer-new image1 x1 y1 RGBA-IMAGE "layer1" 100 LAYER-MODE-NORMAL)))
         (layer2)
         (layer3)
         (layer4)
         (layer5)
         (layer6)
         (layer7)
         (lights)
         (blocks)
         (cell-borders)
         (grid-border)
         (clue-numbers)
         (crossword)
         (text-ofs-x)
         (text-ofs-y)
         
         (test1 7)
         (test1a 8)
         (test1b 9)
         (test1c 10)
         (test1d 11)
        )
    (case random-behavior
      ((0) (srand (- (car (gettimeofday)) (round2 (expt 10 9) 0)))
           (msrg-rand)
           (msrg-rand)
           (set! *crossword-seed* (msrg-rand)))
      ((1) (if (not (integer? *crossword-seed*))
               (set! *crossword-seed* 1431655764)))
      ((2) (set! *crossword-seed* (round2 specific-seed 0))))
    (gimp-context-push)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    (gimp-context-set-sample-threshold-int 0)
    (gimp-context-set-antialias FALSE)
    (gimp-context-set-feather FALSE)
    (gimp-context-set-feather-radius 0 0)
    (gimp-context-set-sample-merged FALSE)
    (gimp-context-set-sample-transparent FALSE)
    (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
    (gimp-context-set-interpolation INTERPOLATION-NONE)
    (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
    (gimp-context-set-feather-radius 4 4) ; line added by karlhof26
    (gimp-image-set-filename image1 "Crossword Grid")
    ;;
    ;; write a summary of the crossword grid's features in the image comment
    (gimp-image-attach-parasite
     image1
     (list* "gimp-comment"
            1
            (string-append "Number of columns:  "
                           (atom->string x1)
                           "\r\nNumber of rows:  "
                           (atom->string y1)
                           "\r\nCell width (in pixels):  "
                           (atom->string cell-width)
                           "\r\nCell height (in pixels):  "
                           (atom->string cell-height)
                           "\r\nColor of lights:  "
                           (color-notations light-color)
                           "\r\nColor of blocks:  "
                           (color-notations block-color)
                           "\r\nColor of borders:  "
                           (color-notations border-color)
                           "\r\nCell border thickness (in pixels):  "
                           (atom->string cell-border-tn)
                           "\r\nGrid border thickness (in pixels):  "
                           (atom->string grid-border-tn)
                           "\r\nBlock margins (in pixels):  "
                           (atom->string block-margins)
                           "\r\nMaximum answer length:  "
                           (atom->string max-ans-length)
                           "\r\nEnsure 180 degree rotational symmetry:  "
                           (if (= ensure-symmetry FALSE) "FALSE" "TRUE")
                           "\r\nAdd clue numbers:  "
                           (if (= add-clue-numbers FALSE)
                               "FALSE"
                               (string-append
                                "TRUE"
                                "\r\n    Size of digit "
                                "(in % of cell height):  "
                                (atom->string digit-size)
                                "\r\n    Text color:  "
                                (color-notations text-color)
                                "\r\n    Font:  "
                                (make-string 1 (integer->char 34))
                                text-font
                                (make-string 1 (integer->char 34))))
                           "\r\nRandom seed:  "
                           (atom->string *crossword-seed*)
                           "\r\nKeep layers separate:  "
                           (if (= separate-layers FALSE) "FALSE" "TRUE")
                           "\r\n")
            ()))
    ;;
    (gimp-image-undo-disable image1)
    (gimp-image-insert-layer image1 layer1 0 0)
    (let d-loop ((d-x 0)
                 (d-y 0)
                 (d-bk-p1 (block-amount x1 y1 max-ans-length))
                 (d-bk-p2 ()))
      ;;
      ;; prepare the crossword grid
      (gimp-image-resize image1 (* 4 x3) (* 4 y3) 0 0)
      (gimp-layer-resize layer1 (* 4 x3) (* 4 y3) 0 0)
      (gimp-edit-fill layer1 FILL-BACKGROUND)
      (gimp-progress-init " " 0)
      (gimp-progress-set-text "Preparing grid")
      (srand *crossword-seed*)
      (let e-loop ((e (* x3 y3))
                   (e-vcr (make-vector (* x3 y3) 0))
                   (e-rdm1 (random (* x3 y3)))
                   (e-rdm2 ()))
        (set! e-rdm2 (+ e-rdm1 (vector-ref e-vcr e-rdm1)))
        (vector-set! e-vcr
                     e-rdm1
                     (+ (- (- e 1) e-rdm1)
                        (if (< e-rdm1 (- e 1))
                            (vector-ref e-vcr (- e 1))
                            0)))
        (let f-loop ((f 4)
                     (f-vcr-x (make-vector 4 0))
                     (f-x1 (random 4))
                     (f-x2 ())
                     (f-vcr-y (make-vector 4 0))
                     (f-y1 (random 4))
                     (f-y2 ()))
          (set! f-x2 (+ f-x1 (vector-ref f-vcr-x f-x1)))
          (vector-set! f-vcr-x
                       f-x1
                       (+ (- (- f 1) f-x1)
                          (if (< f-x1 (- f 1))
                              (vector-ref f-vcr-x (- f 1))
                              0)))
          (set! f-y2 (+ f-y1 (vector-ref f-vcr-y f-y1)))
          (vector-set! f-vcr-y
                       f-y1
                       (+ (- (- f 1) f-y1)
                          (if (< f-y1 (- f 1))
                              (vector-ref f-vcr-y (- f 1))
                              0)))
          (gimp-drawable-set-pixel layer1
                                   (+ (remainder (+ f-x2 d-x) 4)
                                      (* 4 (remainder e-rdm2 x3)))
                                   (+ (remainder (+ f-y2 d-y) 4)
                                      (* 4 (quotient e-rdm2 x3)))
                                   4
                                   #(0 0 0 255))
          (if (> f 2)
              (f-loop (- f 1)
                      f-vcr-x
                      (random (- f 1))
                      f-x2
                      f-vcr-y
                      (random (- f 1))
                      f-y2)))
        (gimp-progress-update (- (+ 1 (/ 1 (* x3 y3))) (/ e (* x3 y3))))
        (if (> e 1)
            (e-loop (- e 1)
                    e-vcr
                    (random (- e 1))
                    e-rdm2)))
      (gimp-image-resize image1 x2 y2 0 0)
      (gimp-layer-resize layer1 x2 y2 0 0)
      (gimp-drawable-offset layer1
                            TRUE
                            OFFSET-BACKGROUND
                            (* -4 (truncate (/ x3 2)))
                            (* -4 (truncate (/ y3 2))))
      ;;
      ;; ensure 180 degree rotational symmetry
      (cond ((= ensure-symmetry TRUE)
             (gimp-image-resize image1 x1 y1 0 0)
             (gimp-layer-resize layer1 x1 y1 0 0)
             (set! layer2 (car (gimp-layer-copy layer1 TRUE)))
             (gimp-image-insert-layer image1 layer2 0 1)
             (gimp-item-transform-rotate-simple layer2 ROTATE-180 TRUE 0 0)
             (gimp-image-select-rectangle image1
                                          CHANNEL-OP-REPLACE
                                          (truncate (/ x1 2))
                                          (truncate (/ y1 2))
                                          x2
                                          y2)
             (gimp-edit-clear layer1)
             (gimp-selection-none image1)
             (set! layer1
                   (car (gimp-image-merge-down image1
                                               layer1
                                               EXPAND-AS-NECESSARY)))))
      ;;
      ;; prevent the answers from exceeding the maximum length allowed
      (cond ((or (< max-ans-length x1)
                 (< max-ans-length y1))
             (gimp-progress-init " " 0)
             (gimp-progress-set-text "Limiting answers lengths")
             (if (< max-ans-length x1)
                 (let g-loop ((g-x 0)
                              (g-y 0)
                              (g-count ())
                              (g-num (if (< max-ans-length y1) 0.5 1)))
                   (gimp-image-select-polygon
                    image1
                    CHANNEL-OP-REPLACE
                    8
                    (if (even? g-y)
                        (vector g-x                      g-y
                                (+ g-x max-ans-length 1) g-y
                                (+ g-x max-ans-length 1) (+ g-y 1)
                                g-x                      (+ g-y 1))
                        (vector (- x1 g-x)                      g-y
                                (- x1 (+ g-x max-ans-length 1)) g-y
                                (- x1 (+ g-x max-ans-length 1)) (+ g-y 1)
                                (- x1 g-x)                      (+ g-y 1))))
                   (set! g-count
                         (vector-ref (apply vector
                                            (gimp-histogram layer1
                                                            HISTOGRAM-VALUE
                                                            0
                                                            0))
                                     4))
                   (cond ((= g-count 0)
                          (set! g-count
                                (+ (truncate (* (/ 2 3) max-ans-length))
                                   (random 3)))
                          (gimp-drawable-set-pixel layer1
                                                   (if (even? g-y)
                                                       (- (+ g-x g-count) 1)
                                                       (- x1 (+ g-x g-count)))
                                                   g-y
                                                   4
                                                   #(0 0 0 255))))
                   (gimp-progress-update (* g-num (/ (+ g-y 1) y4)))
                   (cond ((< g-x (- x1 max-ans-length g-count))
                          (g-loop (+ g-x g-count) g-y g-count g-num))
                         ((< g-y (- y4 1))
                          (g-loop 0 (+ g-y 1) g-count g-num)))))
             (if (< max-ans-length y1)
                 (let h-loop ((h-x 0)
                              (h-y 0)
                              (h-count ())
                              (h-num (if (< max-ans-length x1) 0.5 0)))
                   (gimp-image-select-polygon
                    image1
                    CHANNEL-OP-REPLACE
                    8
                    (if (even? h-x)
                        (vector h-x       h-y
                                (+ h-x 1) h-y
                                (+ h-x 1) (+ h-y max-ans-length 1)
                                h-x       (+ h-y max-ans-length 1))
                        (vector h-x       (- y1 h-y)
                                (+ h-x 1) (- y1 h-y)
                                (+ h-x 1) (- y1 (+ h-y max-ans-length 1))
                                h-x       (- y1 (+ h-y max-ans-length 1)))))
                   (set! h-count
                         (vector-ref (apply vector
                                            (gimp-drawable-histogram layer1
                                                            HISTOGRAM-VALUE
                                                            0.0
                                                            0.01))
                                     4))
                   (cond ((= h-count 0)
                          (set! h-count
                                (+ (truncate (* (/ 2 3) max-ans-length))
                                   (random 3)))
                          (gimp-drawable-set-pixel layer1
                                                   h-x
                                                   (if (even? h-x)
                                                       (- (+ h-y h-count) 1)
                                                       (- y1 (+ h-y h-count)))
                                                   4
                                                   #(0 0 0 255))))
                   (gimp-progress-update (+ h-num (* (- 1 h-num)
                                                     (/ (+ h-x 1) x4))))
                   (cond ((< h-y (- y1 max-ans-length h-count))
                          (h-loop h-x (+ h-y h-count) h-count h-num))
                         ((< h-x (- x4 1))
                          (h-loop (+ h-x 1) 0 h-count h-num)))
                 )
              )
             (gimp-selection-none image1)
            )
       )
      (cond ((= ensure-symmetry TRUE)
             ;(gimp-message "inside 677 block symmetry")
             (set! layer3 (car (gimp-layer-copy layer1 TRUE)))
             (gimp-image-insert-layer image1 layer3 0 -1)
             (gimp-item-transform-rotate-simple layer3 ROTATE-180 TRUE 0 0)
             (gimp-layer-set-opacity layer3 100)
             (gimp-layer-set-mode layer3 LAYER-MODE-DARKEN-ONLY)
             (set! layer1
                   (car (gimp-image-merge-down image1
                                               layer3
                                               EXPAND-AS-NECESSARY)))
            )
      )
      ;;
      ;; cause the lights to be orthogonally contiguous
      (gimp-image-resize image1 (+ (* 2 x1) 1) y1 0 0)
      (gimp-layer-resize layer1 (+ (* 2 x1) 1) y1 0 0)
      (set! layer4 (car (gimp-layer-copy layer1 TRUE)))
      (gimp-image-insert-layer image1 layer4 0 1)
      (gimp-item-transform-flip-simple layer4 ORIENTATION-HORIZONTAL TRUE 0)
      (gimp-drawable-offset layer4 TRUE OFFSET-BACKGROUND -1 0)
      (set! layer1
            (car (gimp-image-merge-down image1 layer1 EXPAND-AS-NECESSARY)))
      (gimp-edit-bucket-fill-full layer1
                                  BUCKET-FILL-BG
                                  LAYER-MODE-BEHIND-LEGACY
                                  100
                                  255
                                  FALSE
                                  TRUE
                                  SELECT-CRITERION-COMPOSITE
                                  (* 2 x1)
                                  0)
      (gimp-image-select-contiguous-color image1
                                          CHANNEL-OP-REPLACE
                                          layer1
                                          (* 2 x1)
                                          0)
      (gimp-selection-invert image1)
      (gimp-edit-fill layer1 FILL-FOREGROUND)
      (gimp-selection-none image1)
      (gimp-image-resize image1 x1 (+ (* 2 y1) 1) 0 0)
      (gimp-layer-resize layer1 x1 (+ (* 2 y1) 1) 0 0)
      (set! layer5 (car (gimp-layer-copy layer1 TRUE)))
      (gimp-image-insert-layer image1 layer5 0 1)
      (gimp-item-transform-flip-simple layer5 ORIENTATION-VERTICAL TRUE 0)
      (gimp-drawable-offset layer5 TRUE OFFSET-BACKGROUND 0 -1)
      (set! layer1
            (car (gimp-image-merge-down image1 layer1 EXPAND-AS-NECESSARY)))
      (gimp-edit-bucket-fill-full layer1
                                  BUCKET-FILL-BG
                                  LAYER-MODE-BEHIND-LEGACY
                                  100
                                  255
                                  FALSE
                                  TRUE
                                  SELECT-CRITERION-COMPOSITE
                                  0
                                  (* 2 y1))
      (gimp-image-select-contiguous-color image1
                                          CHANNEL-OP-REPLACE
                                          layer1
                                          0
                                          (* 2 y1))
      (gimp-selection-invert image1)
      (gimp-edit-fill layer1 FILL-FOREGROUND)
      (gimp-selection-none image1)
      (gimp-image-resize image1 x1 y1 0 0)
      (gimp-layer-resize layer1 x1 y1 0 0)
      (set! d-bk-p2
            (vector-ref (apply vector
                               (gimp-drawable-histogram layer1
                                               HISTOGRAM-VALUE
                                               0.0
                                               0.001))
                        5))
      (cond ((and (>= d-bk-p2 (- d-bk-p1 (* (/ 1 128) (+ (/ d-x 4) d-y 1))))
                  (<= d-bk-p2 (+ d-bk-p1 (* (/ 1 064) (+ (/ d-x 4) d-y 1)))))
             ())
            ((< d-x 3) (d-loop (+ d-x 1) d-y d-bk-p1 d-bk-p2))
            ((< d-y 3) (d-loop 0 (+ d-y 1) d-bk-p1 d-bk-p2))))
    ;;
    ;; locate the first cell of each answer
    (cond ((= add-clue-numbers TRUE)
           ;(gimp-message "inside 764 block")
           (gimp-image-resize image1 (+ x1 2) (+ y1 2) 1 1)
           (gimp-layer-resize layer1 (+ x1 2) (+ y1 2) 1 1)
           (gimp-edit-bucket-fill-full layer1
                                       BUCKET-FILL-FG
                                       LAYER-MODE-BEHIND-LEGACY
                                       100
                                       255
                                       FALSE
                                       TRUE
                                       SELECT-CRITERION-COMPOSITE
                                       0
                                       0)
           (set! layer6 (car (gimp-layer-copy layer1 TRUE)))
           (gimp-image-insert-layer image1 layer6 0 -1)
           (gimp-drawable-levels layer6 HISTOGRAM-VALUE 0.0 1.0 TRUE 1 0.001 0.001 TRUE) ;;0 255 1 1 0
           (set! layer7 (car (gimp-layer-copy layer6 TRUE)))
           (gimp-image-insert-layer image1 layer7 0 -1)
           (plug-in-convmatrix RUN-NONINTERACTIVE
                               image1
                               layer6
                               25
                               (vector 0 0 0 0 0
                                       0 0 0 0 0
                                       0 1 2 4 0
                                       0 0 0 0 0
                                       0 0 0 0 0)
                               0
                               1
                               0
                               5
                               #(1 1 1 1 1)
                               2)
           (plug-in-convmatrix RUN-NONINTERACTIVE
                               image1
                               layer7
                               25
                               (vector 0 0 0 0 0
                                       0 0 1 0 0
                                       0 0 2 0 0
                                       0 0 4 0 0
                                       0 0 0 0 0)
                               0
                               1
                               0
                               5
                               #(1 1 1 1 1)
                               2)
           (gimp-image-select-color image1 CHANNEL-OP-ADD layer7 '(1 1 1))
           (gimp-drawable-levels layer6 HISTOGRAM-VALUE 0.0 1.0 TRUE 1 0.003 0.003 TRUE) ;; 0 255 1 1 1 0
           (gimp-selection-none image1)
           (gimp-image-remove-layer image1 layer7)
           (gimp-image-resize image1 x1 y1 -1 -1)
           (gimp-layer-resize layer1 x1 y1 -1 -1)
           (gimp-layer-resize layer6 x1 y1 -1 -1)
         )
    )
    ;;
    (gimp-image-resize image1
                       (* x1 cell-width)
                       (* y1 cell-height)
                       0
                       0)
    (gimp-layer-scale layer1 (* x1 cell-width) (* y1 cell-height) FALSE)
    ;;
    ;; perform the colouring of lights and blocks
    (set! lights layer1)
    (gimp-item-set-name lights "Lights")
    (set! blocks (car (gimp-layer-copy lights TRUE)))
    (gimp-image-insert-layer image1 blocks 0 0)
    (gimp-item-set-name blocks "Blocks")
    (gimp-invert blocks)
    (gimp-layer-add-mask blocks
                         (car (gimp-layer-create-mask blocks ADD-MASK-COPY)))
    (gimp-layer-set-edit-mask blocks FALSE)
    (gimp-context-set-foreground block-color)
    (gimp-context-set-background light-color)
    (gimp-edit-fill blocks FILL-FOREGROUND)
    (gimp-edit-fill lights FILL-BACKGROUND)
    ;;
    ;; draw the cell borders and indent the blocks
    (cond ((> (+ cell-border-tn block-margins) 0)
           ;(gimp-message "inside 846 block")
           (gimp-context-set-foreground border-color)
           (set! cell-borders
                 (car (gimp-layer-new image1
                                      cell-width
                                      cell-height
                                      RGBA-IMAGE
                                      "Cell Borders"
                                      100
                                      LAYER-MODE-NORMAL)))
           (gimp-image-insert-layer image1 cell-borders 0 0)
           (gimp-edit-fill cell-borders FILL-FOREGROUND)
           (if (> cell-border-tn 0)
               (gimp-image-select-rectangle image1
                                            CHANNEL-OP-REPLACE
                                            cell-border-tn
                                            cell-border-tn
                                            (- cell-width
                                               (* 2 cell-border-tn))
                                            (- cell-height
                                               (* 2 cell-border-tn)))
               (gimp-image-select-rectangle image1
                                            CHANNEL-OP-REPLACE
                                            block-margins
                                            block-margins
                                            (- cell-width
                                               (* 2 block-margins))
                                            (- cell-height
                                               (* 2 block-margins))))
           (gimp-edit-clear cell-borders)
           (plug-in-tile RUN-NONINTERACTIVE
                         image1
                         cell-borders
                         (* x1 cell-width)
                         (* y1 cell-height)
                         0)
           (gimp-image-select-item image1 CHANNEL-OP-REPLACE cell-borders)
           (if (> cell-border-tn 0)
               (gimp-selection-grow image1 block-margins))
           (gimp-drawable-levels (car (gimp-layer-get-mask blocks))
                        HISTOGRAM-VALUE
                        0.0 1.0 TRUE 1.0 0.01 0.01 TRUE) ;; was 0 255 1 0 0
           (gimp-selection-none image1)
           (gimp-layer-add-mask
            cell-borders
            (car (gimp-layer-create-mask cell-borders
                                         ADD-MASK-ALPHA-TRANSFER)))
           (gimp-layer-set-edit-mask cell-borders FALSE)
           (if (= cell-border-tn 0)
               (gimp-image-remove-layer image1 cell-borders))
           )
    )
    ;;
    ;; number the answers
    (cond ((= add-clue-numbers TRUE)
           ;(gimp-message "Numbering answers start")
           (gimp-context-set-foreground text-color)
           (gimp-image-set-active-layer image1 blocks)
           (set! clue-numbers
                 (car (gimp-layer-new image1
                                      (* x1 cell-width)
                                      (* y1 cell-height)
                                      RGBA-IMAGE
                                      "Clue Numbers"
                                      100
                                      LAYER-MODE-NORMAL)))
           (gimp-image-insert-layer image1 clue-numbers 0 -1)
           (gimp-text-fontname image1
                               clue-numbers
                               0
                               0
                               "0\n1\n2\n3\n4\n5\n6\n7\n8\n9"
                               -1
                               TRUE
                               (* cell-height digit-size 0.01)
                               PIXELS
                               text-font)
           (plug-in-autocrop-layer RUN-NONINTERACTIVE
                                   image1
                                   (+ clue-numbers 1))
           ;(gimp-message "fine 1")
           (set! text-ofs-x
                 (- (max (round2 (/ cell-width 14) 0) 2)
                    (car (gimp-drawable-offsets (+ clue-numbers 1)))))
           (set! text-ofs-y
                 (- (max (round2 (/ cell-height 14) 0) 2)
                    (car (cdr (gimp-drawable-offsets (+ clue-numbers 1))))))
           (gimp-image-remove-layer image1 (+ clue-numbers 1))
           (gimp-progress-init " " 0)
           ;(gimp-message "Numbering answers loop start")
           (gimp-progress-set-text "Numbering answers")
           (let i-loop ((i-x-coord 0)
                        (i-y-coord 0)
                        (i-number 0))
              
              ;(gimp-message "fine 2 - but NOT working test")
              
              ; purpose of the next section is to determine if the block is black or can be numbered but not working
              (set! test1 (vector-ref (car (cdr (gimp-drawable-get-pixel layer6
                                                              i-x-coord 
                                                              i-y-coord)))
                           0) ;was 0
                     )
             ;(set! test1 (vector-ref (car (cddr (gimp-drawable-get-pixel layer6
             ;                                                 i-x-coord 
             ;                                                 i-y-coord)))
             ;              0) ;was 0
             ;        )
             ;(set! test1a (car (gimp-drawable-get-pixel layer6
             ;                                                 i-x-coord 
             ;                                                 i-y-coord)))
             ;(set! '(test1b,test1d) (car (test1a)))
             ;(set! test1c (cdr (test1a)))
                     
             ;(gimp-message "fine 3")
             ;(gimp-message (number->string test1))
             ;(gimp-message (number->string test1a))
             ;(gimp-message (number->string test1b))
             ;(gimp-message (number->string test1c))
             ;(gimp-message "fine 4")             
             ; this is the main if asking if the block is black but seems to alwyas generate 1 ; so always on
             (cond ((= (vector-ref
                        (car (cdr (gimp-drawable-get-pixel layer6
                                                           i-x-coord
                                                           i-y-coord)))
                        0) ; was 0
                       1)
                    ;(gimp-message "inside 973 block write the TEXT")
                    (set! i-number (+ i-number 1))
                    (gimp-text-fontname image1
                                        clue-numbers
                                        (+ (* i-x-coord cell-width)
                                           cell-border-tn
                                           text-ofs-x)
                                        (+ (* i-y-coord cell-height)
                                           cell-border-tn
                                           text-ofs-y)
                                        (atom->string i-number)
                                        -1
                                        TRUE
                                        (* cell-height digit-size 0.01)
                                        PIXELS
                                        text-font)
                    (gimp-floating-sel-anchor (+ clue-numbers 1 i-number))
                  )
             )
             (gimp-progress-update (/ (+ (* i-y-coord x1) (+ i-x-coord 1))
                                      (* x1 y1)))
                                      
             (cond ((< i-x-coord (- x1 1))
                        ;(gimp-message "inside increment 1a")
                        (i-loop (+ i-x-coord 1) i-y-coord i-number)
                   )
                    
                   ((< i-y-coord (- y1 1))
                        ;(gimp-message "inside increment 1b")
                        (i-loop 0 (+ i-y-coord 1) i-number)
                   )
             )
             ;(gimp-message "looping numbers...")
           )
           
           ;(gimp-message "numbers line 1008")
           
           (gimp-layer-add-mask
                clue-numbers
                (car (gimp-layer-create-mask clue-numbers
                                         ADD-MASK-ALPHA-TRANSFER)))
           (gimp-layer-set-edit-mask clue-numbers FALSE)
           (gimp-edit-fill clue-numbers FILL-FOREGROUND)
           (gimp-image-remove-layer image1 layer6)
        )
        ;(gimp-message "line 1018 numbers OK")
    )
    ;;
    ;; draw the grid border
    (cond ((> grid-border-tn 0)
            ;(gimp-message " grid border start")
           (gimp-context-set-foreground border-color)
           (gimp-image-resize image1
                              (+ (* 2 grid-border-tn) (* x1 cell-width))
                              (+ (* 2 grid-border-tn) (* y1 cell-height))
                              grid-border-tn
                              grid-border-tn)
           (set! grid-border
                 (car (gimp-layer-new image1
                                      (+ (* 2 grid-border-tn)
                                         (* x1 cell-width))
                                      (+ (* 2 grid-border-tn)
                                         (* y1 cell-height))
                                      RGBA-IMAGE
                                      "Grid Border"
                                      100
                                      LAYER-MODE-NORMAL)))
           (gimp-image-insert-layer image1 grid-border 0 0)
           (gimp-image-select-rectangle image1
                                        CHANNEL-OP-REPLACE
                                        grid-border-tn
                                        grid-border-tn
                                        (* x1 cell-width)
                                        (* y1 cell-height))
           (gimp-selection-invert image1)
           ;(gimp-message " grid border middle 1048")
           (gimp-layer-add-mask
            grid-border
            (car (gimp-layer-create-mask grid-border
                                         ADD-MASK-SELECTION)))
           (gimp-selection-none image1)
           (gimp-edit-fill grid-border FILL-FOREGROUND)
           (gimp-layer-set-edit-mask grid-border FALSE)
           ;(gimp-message " grid border end 1056")
         )
    )
    ;;
    ;; merge the layers together or else keep them separate
    (cond ((= separate-layers FALSE)
           ;(gimp-message "inside 1037 block seplayersFalse")
           (set! crossword
                 (car (gimp-image-merge-visible-layers image1
                                                       EXPAND-AS-NECESSARY)))
           (gimp-item-set-name crossword "Crossword Grid")
           (gimp-layer-flatten crossword)
           (gimp-image-set-active-layer image1 crossword))
          ((= separate-layers TRUE)
                ;(gimp-message "inside 1070 block seplayersTRue")
                (if (> grid-border-tn 0)
                    (gimp-image-set-active-layer image1 grid-border)
                )
                (if (> cell-border-tn 0)
                    (gimp-image-set-active-layer image1 cell-borders)
                )
                (if (= add-clue-numbers TRUE)
                    (gimp-image-set-active-layer image1 clue-numbers)
                )
                (gimp-image-set-active-layer image1 blocks)
                (gimp-image-set-active-layer image1 lights)
          )
    )
    ;;
    ;(gimp-message "good finish")
    (gimp-image-undo-enable image1)
    (gimp-display-new image1)
    (gimp-context-pop)
  )
)

(let* ((widget-a '("Generate random seed automatically"
                   "Retrieve last random seed used"
                   "Specify random seed explicitly"))
       (widget-b '(1431655764 1 2147483646 1 1549411 0 0)))
  ;;
  (script-fu-register
    "script-fu-crossword-grid"
    "Crossword Grid..."
    "Create an image filled with a random crossword grid pattern. There are still issues with the numbering. \nfile:crossword_grid.scm"
    "Gino D <ginodonig@gmail.com>"
    "Gino D"
    "2011-2014"
    ""
    SF-ADJUSTMENT _"Number of columns"                       '(15 8 32 1 1 0 0)
    SF-ADJUSTMENT _"Number of rows"                          '(15 8 32 1 1 0 0)
    SF-ADJUSTMENT _"Cell width (in pixels)"                  '(26 16 64 1 1 0 1)
    SF-ADJUSTMENT _"Cell height (in pixels)"                 '(26 16 64 1 1 0 1)
    SF-COLOR      _"Color of lights"                         '(255 255 255)
    SF-COLOR      _"Color of blocks"                         '(0 0 0)
    SF-COLOR      _"Color of borders"                        '(64 64 64)
    SF-ADJUSTMENT _"Cell border thickness (in pixels)"       '(1 0 4 1 1 0 1)
    SF-ADJUSTMENT _"Grid border thickness (in pixels)"       '(2 0 16 1 1 0 1)
    SF-ADJUSTMENT _"Block margins (in pixels)"               '(1 0 16 1 1 0 1)
    SF-ADJUSTMENT _"Maximum answer length"                   '(12 8 32 1 1 0 0)
    SF-TOGGLE     _"Ensure 180 degree rotational symmetry"   TRUE
    SF-TOGGLE     _"Add clue numbers"                        TRUE
    SF-ADJUSTMENT _"    Size of digit (in % of cell height)" '(45 25 75 1 1 0 0)
    SF-COLOR      _"    Text color"                          '(0 0 0)
    SF-FONT       _"    Font"                                "Arial Bold"
    SF-OPTION     _"Random behavior"                         widget-a
    SF-ADJUSTMENT _"    Specific random seed"                widget-b
    SF-TOGGLE     _"Keep layers separate"                    TRUE
))

(script-fu-menu-register "script-fu-crossword-grid"
                         "<Image>/File/Create/Patterns")
                          
; end of script