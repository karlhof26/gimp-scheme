; hex_grid.scm 
; Karlhof26

; Version 2.0 (2020 03 01)
;
; Changes:
; 1.1 - General clean up, allow specifying the element the length applies to
;       and offsets.
; 2.0 - Clean up and alignment for Gimp 2.10.18

; Description
;
; renders a hex grid on the current layer
;

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script-fu-hex_grid img inLayer inElement inLength inOrientation inStroke inXoff inYoff inColour)
  (let*
        (
            (width (car (gimp-image-width img)))
            (height (car (gimp-image-height img)))
            (varPath (car (gimp-vectors-new img "Hex Grid")))
            (sidelength (cond ((equal? inElement 0) inLength) 
                              ((equal? inElement 1) (/ inLength 2.0))
                              ((equal? inElement 2) (/ inLength 1.73205))))
            (brushTemp 0)
            (x_points 0)
            (y_points 0)
            (xCount 0)
            (yCount 0)
            (x (if (> inXoff 0) -1 0))
            (y (if (> inYoff 0) -1 0))
            (xadd 0)
            (yadd 0)
        )
        ;  it begins here
        (gimp-context-push)
        (gimp-image-undo-group-start img)
        
        (if (= inOrientation 0) ;horizontal
            (begin
                (set! x_points (list (* sidelength 3.0) (* sidelength 2.0) (* sidelength 1.5) (* sidelength 0.5) 0 (* sidelength 0.5) (* sidelength 1.5) (* sidelength 2.0)))
                (set! y_points (list (* (* sidelength 1.73205) 0.5) (* (* sidelength 1.73205) 0.5) 0 0 (* (* sidelength 1.73205) 0.5) (* sidelength 1.73205) (* sidelength 1.73205) (* (* sidelength 1.73205) 0.5)))
                (set! xCount (trunc (/ width (* sidelength 3.0))))
                (set! yCount (trunc (/ height (* sidelength 1.73205))))
            )
            (begin   ;vertical
                (set! x_points (list (* (* sidelength 1.73205) 0.5) (* (* sidelength 1.73205) 0.5) 0 0 (* (* sidelength 1.73205) 0.5) (* sidelength 1.73205) (* sidelength 1.73205) (* (* sidelength 1.73205) 0.5) ))
                (set! y_points (list (* sidelength 3.0) (* sidelength 2.0) (* sidelength 1.5) (* sidelength 0.5) 0 (* sidelength 0.5) (* sidelength 1.5) (* sidelength 2.0)))
                    (set! xCount (trunc (/ width (* sidelength 1.73205))))
                    (set! yCount (trunc (/ height (* sidelength 3.0))))
            )
        )
        ;(gimp-message "ok to line 66")
        (gimp-image-add-vectors img varPath -1)
        
        (while (<= y yCount)
            (while (<= x xCount)
                (if (= inOrientation 0) ;horizontal
                    (begin
                        (set! xadd (+ (* (* x sidelength) 3.0) inXoff))
                        (set! yadd (+ (* (* y sidelength) 1.73205) inYoff))
                    )
                    (begin
                        (set! xadd (+ (* (* x sidelength) 1.73205) inXoff))
                        (set! yadd (+ (* (* y sidelength) 3.0) inYoff))
                    )
                )
                (gimp-vectors-stroke-new-from-points varPath 0 (* (* 8 2) 3) (vector 
                                                               (+ (list-ref x_points 0) xadd) (+ (list-ref y_points 0) yadd)   
                                                               (+ (list-ref x_points 0) xadd) (+ (list-ref y_points 0) yadd)   
                                                               (+ (list-ref x_points 0) xadd) (+ (list-ref y_points 0) yadd)   
                                                               (+ (list-ref x_points 1) xadd) (+ (list-ref y_points 1) yadd)   
                                                               (+ (list-ref x_points 1) xadd) (+ (list-ref y_points 1) yadd)   
                                                               (+ (list-ref x_points 1) xadd) (+ (list-ref y_points 1) yadd)   
                                                               (+ (list-ref x_points 2) xadd) (+ (list-ref y_points 2) yadd)   
                                                               (+ (list-ref x_points 2) xadd) (+ (list-ref y_points 2) yadd)   
                                                               (+ (list-ref x_points 2) xadd) (+ (list-ref y_points 2) yadd)   
                                                               (+ (list-ref x_points 3) xadd) (+ (list-ref y_points 3) yadd)   
                                                               (+ (list-ref x_points 3) xadd) (+ (list-ref y_points 3) yadd)   
                                                               (+ (list-ref x_points 3) xadd) (+ (list-ref y_points 3) yadd)   
                                                               (+ (list-ref x_points 4) xadd) (+ (list-ref y_points 4) yadd)   
                                                               (+ (list-ref x_points 4) xadd) (+ (list-ref y_points 4) yadd)   
                                                               (+ (list-ref x_points 4) xadd) (+ (list-ref y_points 4) yadd)   
                                                               (+ (list-ref x_points 5) xadd) (+ (list-ref y_points 5) yadd)   
                                                               (+ (list-ref x_points 5) xadd) (+ (list-ref y_points 5) yadd)   
                                                               (+ (list-ref x_points 5) xadd) (+ (list-ref y_points 5) yadd)   
                                                               (+ (list-ref x_points 6) xadd) (+ (list-ref y_points 6) yadd)   
                                                               (+ (list-ref x_points 6) xadd) (+ (list-ref y_points 6) yadd)   
                                                               (+ (list-ref x_points 6) xadd) (+ (list-ref y_points 6) yadd)   
                                                               (+ (list-ref x_points 7) xadd) (+ (list-ref y_points 7) yadd)   
                                                               (+ (list-ref x_points 7) xadd) (+ (list-ref y_points 7) yadd)   
                                                               (+ (list-ref x_points 7) xadd) (+ (list-ref y_points 7) yadd)   
                                                              ) FALSE)
                (set! x (+ x 1))
            )   
            (set! y (+ y 1))
            (set! x (if (> inXoff 0) -1 0))
        )
        
        (gimp-context-set-foreground inColour)
        (gimp-context-set-opacity 100)
        ;(gimp-message "ok to line 115")
        
        (gimp-context-set-paint-method "gimp-paintbrush")
        (set! brushTemp (car (gimp-brush-new "Temp Stroke Circle Brush")))
        (gimp-brush-set-shape brushTemp BRUSH-GENERATED-CIRCLE)
        (gimp-brush-set-hardness brushTemp 0.99)
        (gimp-brush-set-radius brushTemp (+ (/ inStroke 2) 1.0))
        (gimp-brush-set-spacing brushTemp 20.0)
        (gimp-brush-set-spikes brushTemp 2)
        (gimp-brush-set-aspect-ratio brushTemp 1.0)
        (gimp-brush-set-angle brushTemp 1.0)
        
        (gimp-context-set-brush "Temp Stroke Circle Brush") ; was brushTemp variable
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL)
        
        (gimp-edit-stroke-vectors inLayer varPath)
        
        
        
        ;(gimp-image-remove-vectors img varPath)
        (gimp-brush-delete brushTemp)
        
        ;done
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gimp-context-pop)
        (gc); garbage cleanup
  )
)

(script-fu-register "script-fu-hex_grid"
        "<Toolbox>/Script-Fu/Render/Pattern/Hex Grid"
        "Draw a hex grid on the image. \n hexgrid.scm"
        "karlhof26"
        "karlhof26"
        "March 2020"
        "RGB* GRAY*"
        SF-IMAGE      "image"              0
        SF-DRAWABLE   "drawable"           0
        SF-OPTION     "Element to Specify"  '("Side" "Point to Point" "Side to Side")
        SF-ADJUSTMENT "Length of Element"   '(70 2 400 1 10 0 SF-SPINNER)
        SF-OPTION     "Hex Orientation"     '("Horizontal" "Vertical")
        SF-ADJUSTMENT "Line Width (px)"     '(2 1 400 1 10 0 SF-SPINNER)
        SF-ADJUSTMENT "Horizontal Offset"   '(0 0 399 0.5 10 1 SF-SPINNER)
        SF-ADJUSTMENT "Vertical Offset"     '(0 0 399 0.5 10 1 SF-SPINNER)
        SF-COLOR      "Color"               "black"
)

;end of script