; hexmap.scm 
; by Jérémy Zurcher <jeremy@asynk.ch>
; based on hex_grid by Rob Antonishen
;
; Version 1.0 (2015) 
; Uodated for GIMP-2.10.22 by karlhof26 (Jan 2021)
;
; Description
;
;  Build hex map with nice options
;
;
; Issues
;
;  you must first select the pencil tool beforce calling this script ?!??!?!?
;
;
; License:
;
; Copyright (c) <2015> <Jérémy Zurcher>
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.
;
;===============================================================================



(define (search l x y)
  (if (null? l)
    #f
    (if (and (= (vector-ref (car l) 0) x)
             (= (vector-ref (car l) 1) y))
      #t
      (search (cdr l) x y)
    )
  )
)

(define (maybe_add l x y)
    (set! x (/ (trunc (* x 1000)) 1000))
    (set! y (/ (trunc (* y 1000)) 1000))
    (if (search l x y)
        l
        (list* (vector x y) l) 
    )
)

(define (crawl_hex_edges edges mask pressure)
    (gimp-message "crawl x")
 (if (not (null? edges))
    (begin
        (gimp-airbrush mask pressure 2 (car edges))
       ; Note recursion here
        (crawl_hex_edges (cdr edges) mask pressure)
    )
    (begin
        (gimp-message "edges are null")
    )
  )
)

(define (erase_hex_edges edges mask size)
    (let* (
            (dummy 0)
           )
    (gimp-message "erase hex edges")
    (gimp-context-set-brush-size (* 1.4 size))
    (gimp-context-set-foreground "black")
    (gimp-context-set-opacity 80)
    (gimp-context-set-brush "2. Hardness 025")
    (gimp-message "ready to crawl")
    
    (crawl_hex_edges edges mask 80)
    )
)

(define (build_border_path border_edges border_path)
  (let* (
      (i 0)
      (y 0)
      (s (length border_edges))
      (n (* s 3))
      (v (make-vector n 0))
    )
    
    (while (< i s)
        (vector-set! v y       (list-ref border_edges i))
        (vector-set! v (+ y 1) (list-ref border_edges (+ i 1)))
        (vector-set! v (+ y 2) (list-ref border_edges i))
        (vector-set! v (+ y 3) (list-ref border_edges (+ i 1)))
        (vector-set! v (+ y 4) (list-ref border_edges i))
        (vector-set! v (+ y 5) (list-ref border_edges (+ i 1)))
        (set! i (+ i 2))
        (set! y (+ y 6))
    )
    (gimp-vectors-stroke-new-from-points border_path 0 n v TRUE)
  )
)

(define (build_h_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom)
    ;(gimp-message "line 106")
  (if (and (>= x 0) (>= y 0) (<= x xBorder) (<= y yBorder))
    ; top ?
    (if (= y 0)
      (if (or (equal? bRight #t) (< x xBorder))
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 3)
                                     (vector-ref vy 3)
                                     (vector-ref vx 2)
                                     (vector-ref vy 2)
                                     (vector-ref vx 1)
                                     (vector-ref vy 1)
                                     ))
        )
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 5)
                                     (vector-ref vy 5)
                                     ))
        )
      )
      ; else bottom ?
      (if (= y yBorder)
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 7)
                               (vector-ref vy 7)
                               (vector-ref vx 6)
                               (vector-ref vy 6)
                               (vector-ref vx 5)
                               (vector-ref vy 5)
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               border_edges)
          )
          (if (< x xBorder)
            (set! border_edges (list*
                                 (vector-ref vx 7)
                                 (vector-ref vy 7)
                                 (vector-ref vx 6)
                                 (vector-ref vy 6)
                                 (vector-ref vx 5)
                                 (vector-ref vy 5)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
            (if (equal? bRight #t)
              (set! border_edges (list*
                                   (vector-ref vx 2)
                                   (vector-ref vy 2)
                                   (vector-ref vx 1)
                                   (vector-ref vy 1)
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 5)
                                   (vector-ref vy 5)
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   border_edges)
              )
              (set! border_edges (list*
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   border_edges)
              )
            )
          )
        )
        ; else left ?
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               border_edges)
          )
          ; else right ?
          (if (= x xBorder)
            (if (equal? bRight #t)
              (set! border_edges (append border_edges
                                         (list
                                           (vector-ref vx 2)
                                           (vector-ref vy 2)
                                           (vector-ref vx 1)
                                           (vector-ref vy 1)
                                           ))
              )
              (set! border_edges (append border_edges
                                         (list
                                           (vector-ref vx 4)
                                           (vector-ref vy 4)
                                           (vector-ref vx 5)
                                           (vector-ref vy 5)
                                           ))
              )
            )
          )
        )
      )
    )
  )
  border_edges
)

(define (build_v_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom)
  (if (and (>= x 0) (>= y 0) (<= x xBorder) (<= y yBorder))
    ; top ?
    (if (= y 0)
      ; top left
      (if (= x 0)
        (set! border_edges (append border_edges
                                   (list
                                     (vector-ref vx 1)
                                     (vector-ref vy 1)
                                     (vector-ref vx 2)
                                     (vector-ref vy 2)
                                     (vector-ref vx 3)
                                     (vector-ref vy 3)
                                     (vector-ref vx 4)
                                     (vector-ref vy 4)
                                     (vector-ref vx 5)
                                     (vector-ref vy 5)
                                     ))
        )
        (if (= x xBorder)
          (set! border_edges (append border_edges
                                     (list
                                       (vector-ref vx 4)
                                       (vector-ref vy 4)
                                       (vector-ref vx 5)
                                       (vector-ref vy 5)
                                       (vector-ref vx 6)
                                       (vector-ref vy 6)
                                       (vector-ref vx 7)
                                       (vector-ref vy 7)
                                       ))
          )
          (set! border_edges (append border_edges
                                     (list
                                       (vector-ref vx 4)
                                       (vector-ref vy 4)
                                       (vector-ref vx 5)
                                       (vector-ref vy 5)
                                       ))
          )
        )
      )
      ; else bottom ?
      (if (= y yBorder)
        (if (equal? bBottom #t)
          (if (= x 0)
            (begin
                (set! border_edges (list*
                                 (vector-ref vx 6)
                                 (vector-ref vy 6)
                                 (vector-ref vx 1)
                                 (vector-ref vy 1)
                                 (vector-ref vx 2)
                                 (vector-ref vy 2)
                                 (vector-ref vx 3)
                                 (vector-ref vy 3)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
                )
            )
            (begin
                (if (< x xBorder)
                    (set! border_edges (list*
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 7)
                                   (vector-ref vy 7)
                                   border_edges)
                    )
                    ; else
                    (set! border_edges (list*
                                   (vector-ref vx 4)
                                   (vector-ref vy 4)
                                   (vector-ref vx 5)
                                   (vector-ref vy 5)
                                   (vector-ref vx 6)
                                   (vector-ref vy 6)
                                   (vector-ref vx 7)
                                   (vector-ref vy 7)
                                   border_edges)
                    )
                )
            )
          )
          
          (if (< x xBorder)
            (set! border_edges (list*
                                 (vector-ref vx 5)
                                 (vector-ref vy 5)
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
            (set! border_edges (list*
                                 (vector-ref vx 4)
                                 (vector-ref vy 4)
                                 border_edges)
            )
          )
        )
        ; else left ?
        (if (= x 0)
          (set! border_edges (list*
                               (vector-ref vx 1)
                               (vector-ref vy 1)
                               (vector-ref vx 2)
                               (vector-ref vy 2)
                               (vector-ref vx 3)
                               (vector-ref vy 3)
                               (vector-ref vx 4)
                               (vector-ref vy 4)
                               border_edges)
          )
          ; else right ?
          (if (= x xBorder)
            (set! border_edges (append border_edges
                                       (list
                                         (vector-ref vx 4)
                                         (vector-ref vy 4)
                                         (vector-ref vx 5)
                                         (vector-ref vy 5)
                                         (vector-ref vx 6)
                                         (vector-ref vy 6)
                                         (vector-ref vx 7)
                                         (vector-ref vy 7)
                                         ))
            )
          )
        )
      )
    )
  )
  border_edges
)

(define (build_grid grid_path border_path width height sideLength orientation xOff yOff)
  (let*
    (
      (w (- width xOff))
      (h (- height yOff))
      (x (if (> xOff 0) -1 0))
      (y (if (> yOff 0) -1 0))
      (vx (make-vector 8 0))
      (vy (make-vector 8 0))
      (hex_edges '())
      (border_edges '())
      (hX 0)
      (hY 0)
      (xAdd 0)
      (yAdd 0)
      (xLast 0)
      (yLast 0)
      (xBorder 0)
      (yBorder 0)
      (bRight #t)
      (bBottom #t)
    )
    
    (if (= orientation 0)
      ; horizontal
      (begin
            ;(gimp-message "line 381")
            (set! xLast (trunc (/ w (* sideLength 3.0))))
            (set! yLast (trunc (/ h (* sideLength 1.73205))))
            (set! xBorder (trunc (/ (- w xOff) (* sideLength 3.0))))
            (set! yBorder (- yLast 1))
            (set! bRight (if (> (- w xOff (* (* xBorder sideLength) 3.0)) sideLength) #t #f))
            ; (set! bBottom #t) 
            (set! hX (vector (* sideLength 3.0) (* sideLength 2.0) (* sideLength 1.5) (* sideLength 0.5) 0 (* sideLength 0.5) (* sideLength 1.5) (* sideLength 2.0)))
            (set! hY (vector (* (* sideLength 1.73205) 0.5) (* (* sideLength 1.73205) 0.5) 0 0 (* sideLength 1.73205 0.5) (* sideLength 1.73205) (* sideLength 1.73205) (* sideLength 1.73205 0.5)))
      )
      (begin
            ;(gimp-message "line 391")
            (set! xLast (trunc (/ w (* sideLength 1.73205))))
            (set! yLast (trunc (/ h (* sideLength 3.0))))
            (set! xBorder (- xLast 1))
            (set! yBorder (trunc (/ (- h yOff) (* sideLength 3.0))))
            ; (set! bRight #t)
            (set! bBottom (if (> (- h yOff (* yBorder sideLength 3.0)) sideLength) #t #f))
            (set! hX (vector (* sideLength 1.73205 0.5) (* sideLength 1.73205 0.5) 0 0 (* sideLength 1.73205 0.5) (* sideLength 1.73205) (* sideLength 1.73205) (* sideLength 1.73205 0.5)))
            (set! hY (vector (* sideLength 3.0) (* sideLength 2.0) (* sideLength 1.5) (* sideLength 0.5) 0 (* sideLength 0.5) (* sideLength 1.5) (* sideLength 2.0)))
      )
    )
    
    ;(gimp-message "line 402")
    (gimp-progress-init "defining grid" -1)
    (while (<= y yLast)
      (gimp-progress-update (/ y yLast))
      (while (<= x xLast)
        (if (= orientation 0)
          ; horizontal
          (begin
            (set! xAdd (+ (* (* x sideLength) 3.0) xOff))
            (set! yAdd (+ (* y sideLength 1.73205) yOff))
          )
          (begin
            (set! xAdd (+ (* x sideLength 1.73205) xOff))
            (set! yAdd (+ (* y sideLength 3.0) yOff))
          )
        )
        (vector-set! vx 0 (+ (vector-ref hX 0) xAdd))
        (vector-set! vx 1 (+ (vector-ref hX 1) xAdd))
        (vector-set! vx 2 (+ (vector-ref hX 2) xAdd))
        (vector-set! vx 3 (+ (vector-ref hX 3) xAdd))
        (vector-set! vx 4 (+ (vector-ref hX 4) xAdd))
        (vector-set! vx 5 (+ (vector-ref hX 5) xAdd))
        (vector-set! vx 6 (+ (vector-ref hX 6) xAdd))
        (vector-set! vx 7 (+ (vector-ref hX 7) xAdd))
        (vector-set! vy 0 (+ (vector-ref hY 0) yAdd))
        (vector-set! vy 1 (+ (vector-ref hY 1) yAdd))
        (vector-set! vy 2 (+ (vector-ref hY 2) yAdd))
        (vector-set! vy 3 (+ (vector-ref hY 3) yAdd))
        (vector-set! vy 4 (+ (vector-ref hY 4) yAdd))
        (vector-set! vy 5 (+ (vector-ref hY 5) yAdd))
        (vector-set! vy 6 (+ (vector-ref hY 6) yAdd))
        (vector-set! vy 7 (+ (vector-ref hY 7) yAdd))
        ; hex path
        (gimp-vectors-stroke-new-from-points grid_path 0 (* 8 2 3)
                                             (vector
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 0) (vector-ref vy 0)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 1) (vector-ref vy 1)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 2) (vector-ref vy 2)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 3) (vector-ref vy 3)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 4) (vector-ref vy 4)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 5) (vector-ref vy 5)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 6) (vector-ref vy 6)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               (vector-ref vx 7) (vector-ref vy 7)
                                               ) FALSE
        )
        ; border
        (if (= orientation 0) ; horizontal
            (set! border_edges (build_h_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom))
            (set! border_edges (build_v_border_edges border_edges vx vy x y xBorder yBorder bRight bBottom))
        )
        ; hex edges
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 0) (vector-ref vy 0)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 1) (vector-ref vy 1)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 2) (vector-ref vy 2)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 3) (vector-ref vy 3)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 4) (vector-ref vy 4)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 5) (vector-ref vy 5)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 6) (vector-ref vy 6)))
        (set! hex_edges (maybe_add hex_edges (vector-ref vx 7) (vector-ref vy 7)))
        ; next loop values
        (set! x (+ x 1))
      )
      (set! y (+ y 1))
      (set! x (if (> xOff 0) -1 0))
    )
    (display border_edges)
    (build_border_path border_edges border_path)
        ;(gimp-message "returning")
        ;(gimp-display-new img)
    hex_edges
  )
)

(define (script-fu-hex-map-advanced orientation elm len xN yN xOff yOff erase gStroke gColour bStroke bColour bOpacity)
  (let* (
            (img 0)
            (gridLayer 0)
            (borderLayer 0)
            (mask 0)
            (width 0)
            (height 0)
            (grid_path 0)
            (hex_edges 0)
            (border_path 0)
            (border_edges '())
            (sideLength (cond ((equal? elm 0) len) ((equal? elm 1) (/ len 2.0)) ((equal? elm 2) (/ len 1.73205))))
            ;(brushTemp (car (gimp-brush-new "HexMapBrush")))
        )
    ;(gimp-message "started OK")
    (if (= orientation 0)
      ; horizontal
        (begin
            (set! height (+ (* 2 yOff) (* 1.73205 yN sideLength)))
            (set! width  (+ (* 2 xOff) (* (+ 0.5 (* 1.5 xN)) sideLength)))
        )
        (begin
            (set! width  (+ (* 2 xOff) (* 1.73205 xN sideLength)))
            (set! height (+ (* 2 yOff) (* (+ 0.5 (* 1.5 yN)) sideLength)))
        )
    )
    
    ; START
    (gimp-context-push)
    
    (set! img (car (gimp-image-new width height RGB)))
    (gimp-image-undo-group-start img)
    
    ; set brush
    ;(gimp-brush-set-shape brushTemp BRUSH-GENERATED-CIRCLE)
    ;(gimp-brush-set-angle brushTemp 0)
    ;(gimp-brush-set-aspect-ratio brushTemp 1)
    ;(gimp-brush-set-hardness brushTemp 1)
    ;(gimp-brush-set-spacing brushTemp 0)
    ;(gimp-brush-set-spikes brushTemp 2) ; was 1
    ;(gimp-brushes-refresh)
    
    (gimp-context-set-brush "Circle (01)") ; was HexMapBrush
    (gimp-context-set-opacity 100)
    (gimp-context-set-brush-size 2.0)
    (gimp-context-set-dynamics "Dynamics Off")
    (gimp-context-set-paint-mode LAYER-MODE-NORMAL)
    
    ; paths
    (set! grid_path (car (gimp-vectors-new img "Hex Grid")))
    (gimp-image-add-vectors img grid_path -1)
    (set! border_path (car (gimp-vectors-new img "Map Border")))
    (gimp-image-add-vectors img border_path -1)
    ;(gimp-displays-flush)
    ;(gimp-display-new img)
    
    (set! hex_edges (build_grid grid_path border_path width height sideLength orientation xOff yOff))
    ;(gimp-display-new img)
    
    ; grid layer
    (set! gridLayer (car (gimp-layer-new img width height RGBA-IMAGE "Grid" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer img gridLayer 0 -1)
    (gimp-context-set-brush-size gStroke)
    (gimp-context-set-foreground gColour)
    (if (> gStroke 0)
        (begin
            (gimp-edit-stroke-vectors gridLayer grid_path)
            ;(gimp-path-to-selection img "Map Border" 2 0 0 0 0)
            ;(gimp-selection-invert img)
            ;(gimp-edit-clear gridLayer)
        )
    )
    ;(gimp-display-new img)
    
    ; border layer
    (set! borderLayer (car (gimp-layer-new img width height RGBA-IMAGE "Border" 100 LAYER-MODE-NORMAL)))
    (gimp-image-insert-layer img borderLayer 0 -1)
    ; transparent border
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-bucket-fill borderLayer 0 0 bOpacity 20 0 0 0)
    ; border stroke
    (if (> bStroke 0)
      (begin
        (gimp-context-set-brush-size bStroke)
        (gimp-context-set-foreground bColour)
        (gimp-edit-stroke-vectors borderLayer border_path)
      )
    )
    
    (gimp-path-to-selection img "Map Border" 2 0 0 0 0)
    (gimp-selection-invert img)
    (gimp-edit-clear gridLayer)
    ;(gimp-display-new img)
    
    (gimp-selection-none img)
    ;(gimp-brush-delete brushTemp)
    
; REMOVED BY karlhof26 - no need for this as I see.
;    ; grid mask
    (if (> erase 0)
        (begin
            (set! mask (car (gimp-layer-create-mask gridLayer ADD-MASK-WHITE)))
           (gimp-layer-add-mask gridLayer mask)
            (erase_hex_edges hex_edges mask sideLength)
            (if (= erase 2)
                (gimp-drawable-invert mask)
            )
            (gimp-message "Layer mask in use - disbale mask to see hexgrid")
        )
    )
    
    (gimp-display-new img)
    (gimp-image-clean-all img)
    
    ; END
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gc) ; garbage collect
  )
)

; (define (script-fu-hex_mapp)
;   (script-fu-hex_map 0 0 100 5 5 140 50 2 "black" 6 "red") 
; )

(script-fu-register "script-fu-hex-map-advanced"
                    "Hex Map..."
                    "Draws a hex grid on a layer of the image. Erase, if used, creates a mask that must be disabled to see the hexgrid.\nfile:hexmap.scm"
                    "Jérémy Zurcher"
                    "Copyright 2015, Jérémy Zurcher"
                    "Nov 2015"
                    ""
                    SF-OPTION     "Hex Orientation"         '("Horizontal" "Vertical")
                    SF-OPTION     "Element to Specify"      '("Side" "Point to Point" "Side to Side")
                    SF-ADJUSTMENT "Length of Element"       '(100 2 400 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Horizontal Hex (#)"      '(18 2 500 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Vertical Hex (#)"        '(10 2 500 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Horizontal Offset (px)"  '(50 0 399 0.5 10 1 SF-SPINNER)
                    SF-ADJUSTMENT "Vertical Offset (px)"    '(50 0 399 0.5 10 1 SF-SPINNER)
                    SF-OPTION     "Erase (Mask out areas)"                   '("None" "Points" "Segments")
                    SF-ADJUSTMENT "Line Width (px)"         '(2 1 20 1 10 0 SF-SPINNER)
                    SF-COLOR      "Line Colour"             '(244 244 244)
                    SF-ADJUSTMENT "Border Width (px)"       '(6 0 20 1 10 0 SF-SPINNER)
                    SF-COLOR      "Border Colour"           '(244 244 244) ;'(69 70 11)
                    SF-ADJUSTMENT "Border Opacity (px)"     '(40 0 100 1 10 0 SF-SPINNER)
)

(script-fu-menu-register "script-fu-hex-map-advanced" "<Toolbox>/Script-Fu/Render/Pattern")

; end of script