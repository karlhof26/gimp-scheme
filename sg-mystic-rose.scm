; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.  
;


(define (script-fu-sg-mystic-rose 
            image
            drawable
            number-of-points
            thickness
            randomb
            colormodea
            palette
            gradient
            create-path?)
  (let* (   (create-path (not (zero? create-path?)))
            (color-mode (+ (* 2 colormodea) randomb))
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-context-get-foreground)))
            (rad-x (floor (/ (- width thickness) 2)))
            (rad-y (floor (/ (- height thickness) 2)))
            (center-x (floor (/ width 2)))
            (center-y (floor (/ height 2)))
            (points-list 
              (let loop ((point number-of-points)
                      (points-list '()) )
               (if (zero? point)
                 points-list
                  (loop (- point 1)
                     (cons (cons (+ center-x (* rad-x (cos (/ (* 2 *pi* point) number-of-points))))
                                 (+ center-y (* rad-y (sin (/ (* 2 *pi* point) number-of-points)))) )
                           points-list ))))
            )
            (path 0)
            (stroke 0)
            (brush "kh") ; was ""
            (prog-count 0) 
            (prog-end (* 2 number-of-points (- number-of-points 1)))
            
            (colorb '(0 0 0))
            
        )
        (gimp-image-undo-group-start image)
        (gimp-context-push)
        (set! old-fg (car (gimp-context-get-foreground)))
        (gimp-message "started")
        (gimp-context-set-paint-method "gimp-paintbrush")
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL)
        (set! brush (car (gimp-brush-new "sg mystic temporary")))
        (gimp-brush-set-shape brush BRUSH-GENERATED-CIRCLE)
        (gimp-brush-set-hardness brush 0.99)
        (gimp-brush-set-aspect-ratio brush 1.0)
        (gimp-brush-set-spacing brush 20.0)
        (gimp-brush-set-radius brush (round (/ thickness 2)))
        ;(gimp-brushes-refresh)
        
        ;(gimp-brush-set-radius brush 2.0)
        (gimp-context-set-brush brush) ; was brush not "temporary"
        
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-opacity 100.0)
        
        ;(gimp-message "line 56")
        (set! path (car (gimp-vectors-new image (string-append (number->string number-of-points)
                                                                           "pt mystic rose" ))))
        (gimp-image-add-vectors image path 0)
        (let next-start-point ((start points-list))
            (unless (null? start)
                (let next-end-point ((end (cdr points-list)))
                    (unless (null? end)
                        (set! stroke (car (gimp-vectors-bezier-stroke-new-moveto path (caar start) (cdar start))))
                        (gimp-vectors-bezier-stroke-lineto path stroke (caar end) (cdar end))
                        (gimp-progress-update (/ prog-count prog-end))
                        (set! prog-count (+ prog-count 1))
                        (next-end-point (cdr end)) 
                    )
                )
                (next-start-point (cdr start)) 
            )
        )
        ;(gimp-message "line 86")
        (gimp-displays-flush)
    
      (let ((num-strokes (car (gimp-vectors-get-strokes path)))
          )
          ;(gimp-message "line 91")
         (let next-stroke ((strokes (vector->list (cadr (gimp-vectors-get-strokes path))))
                        (color-index 0) )
          (unless (null? strokes)
            ;(gimp-message "line 95")
            (gimp-progress-update (/ prog-count prog-end))
            (set! prog-count (+ prog-count 1))
            ;(gimp-message (number->string prog-count))
            (let ((points (caddr (gimp-vectors-stroke-get-points path (car strokes))))
                        (num-palette-entries (car (gimp-palette-get-colors palette))) 
                     )
                    ;(gimp-message "line 95")
                    (cond 
                        ((= color-mode 0) ; foreground 
                            (gimp-context-set-foreground '(25 40 30))
                            ;(gimp-message "line 104 foreground")
                            (gimp-context-set-foreground old-fg)
                            ;(gimp-message "line 106 using fg")
                        )
                        ((= color-mode 1) ; random color 
                            (gimp-context-set-foreground (list (rand 255) (rand 255) (rand 255))) ; were all random 256 
                            ;(gimp-message "line 100 random color")
                        )
                        ((= color-mode 2) ; palette - color-index is offset into palette
                            ;(gimp-message "line 103 - palette")
                            (gimp-context-set-foreground 
                                (car (gimp-palette-entry-get-color palette (floor (modulo color-index (- num-palette-entries 1))))) )
                        )
                        ((= color-mode 3) ; palette random - choose random color from specified palette
                            ;(gimp-message "line 108 - palette random")
                            (gimp-context-set-foreground 
                                (car (gimp-palette-entry-get-color palette (rand (- num-palette-entries 1)))) ) ; was rand dum-pal-entr
                        )
                        ((= color-mode 4) ; gradient - color-index is offset into specified gradient
                            ;(gimp-message "line 113 - gradient color")
                            (if (< color-index num-strokes)
                                (begin
                                    ;(gimp-message "ratio OK")
                                )
                                (begin
                                    (gimp-message "ratio WRONG CRASH")
                                    (quit)
                                )
                            )
                            (let* ((color-vector (cadr (gimp-gradient-get-custom-samples gradient 
                                                                             1 
                                                                             (vector  (/ color-index num-strokes))
                                                                             FALSE )))
                                    (color (map (lambda (x) (* x 255)) (butlast (vector->list color-vector))))
                                    (opacity (* 100 (vector-ref color-vector 3)))
                                  )
                                (gimp-context-set-foreground color)
                                (gimp-context-set-opacity opacity) 
                            )
                        )
                        ((= color-mode 5) ; gradient random - random color from specified gradient
                            ;(gimp-message "line 134 - gradient random")
                            (let* ((color-vector (cadr (gimp-gradient-get-custom-samples gradient 
                                                                             1 
                                                                             (vector (/ (random num-strokes) num-strokes))
                                                                             FALSE )))
                                    (color (map (lambda (x) (* x 255)) (butlast (vector->list color-vector))))
                                    (opacity (* 100 (vector-ref color-vector 3)))
                                  )
                                (gimp-context-set-foreground color)
                                (gimp-context-set-opacity opacity) 
                            )
                        )
                    )
                    ;(gimp-message "painting")
                    ;(gimp-paintbrush  drawable 
                    ;         0              ; was FALSE  
                    ;         4 
                    ;         (vector (vector-ref points 2)
                    ;                 (vector-ref points 3)
                    ;                 (vector-ref points 8)
                    ;                 (vector-ref points 9) )
                    ;         PAINT-CONSTANT
                    ;         0 )  ; was 0
                    (gimp-paintbrush-default  drawable  
                             4 
                             (vector (vector-ref points 2)
                                     (vector-ref points 3)
                                     (vector-ref points 8)
                                     (vector-ref points 9) )
                            
                              )  ; was 0
                             
                    ;(gimp-message "next-stroke")
                    (if (equal? prog-count prog-end)
                                (begin
                                    ;(gimp-message "Time to end") 
                                    (gimp-displays-flush)
                                )
                                (begin
                                    ;(gimp-message "doing next stroke")
                                    (next-stroke (cdr strokes) (+ color-index 1))
                                )
                                
                    )
                    
                    
            )
          )
        )
      )
    ;(gimp-brush-delete brush) 
    
    ;(if (= create-path FALSE)
    ;    (begin
    ;        (gimp-image-remove-vectors image path)
    ;    )
    ;)
    
    (gimp-message "Good finish OK")
    (gimp-context-pop)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-sg-mystic-rose"
    "Mystic rose"
    "Create a mystic rose within the current layer. \n Uses current brush settings \nfile:sg-mystic-rose.scm"
    "Saul Goode"
    "Saul Goode"
    "July 2011"
    "RGB RGBA"
    SF-IMAGE      "Image"    0
    SF-DRAWABLE   "Layer"    0
    SF-ADJUSTMENT "Number of points"    '(12 3 179 1 10 0 1)
    SF-ADJUSTMENT "Line thickness (not used)"      '(4 1 20 1 3 0 1)
    SF-TOGGLE     "Random?"             FALSE
    SF-OPTION     "Color method"        '("Foreground" "Palette" "Gradient")
    SF-PALETTE    "Palette"             "Default"
    SF-GRADIENT   "Gradient"            "Full saturation spectrum CW"
    SF-TOGGLE     "Keep the path?"       TRUE
)

(script-fu-menu-register "script-fu-sg-mystic-rose"
    "<Toolbox>/Script-Fu/Render/"
)
