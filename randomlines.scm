;; randomlines.scm -*-scheme-*- 
;; Using the selected tool (Brush, Pen or Airbrush) use the 
;; tool a specified number of times to produce a  "random" pattern
;; More info at http://members.optusnet.com.au/~charles57/GIMP
;; Version 1.0
;;
;; Copyright (C) 2008 by Charles Cave <charlesweb@optusnet.com.au>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (script-fu-randomlines    img drw
                                  margin
                                  brush
                                  brush-size
                                  color
                                  iterations
                                  minlength
                                  maxlength
                                  discardborder)

  (let* (
         (x1 0)
         (y1 0)
         (rndangle 0)
         (rndlength 0)
         (px1 0)
         (px2 0)
         (py1 0)
         (py2 0)
         
            (x2 (car (gimp-image-width img)))
            (y2 (car (gimp-image-height img)))
            
            (*randompoint* (cons-array 4 'double))
            (drw-width 0)
            (drw-height 0)
        )
        
    ; define drawable area for the algorithm
    (set! x1 (+ x1 margin))
    (set! x2 (- x2 margin))
    (set! y1 (+ y1 margin))
    (set! y2 (- y2 margin))
    
    (set! drw-width  (- x2 x1))
    (set! drw-height (- y2 y1))
    
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    
    ; Set new color, brush, opacity, paint mode.
    (gimp-context-set-foreground color)
    (gimp-context-set-brush (car brush))
    (gimp-context-set-brush-size brush-size)
    (gimp-context-set-opacity (car (cdr brush)))
    (gimp-context-set-paint-mode (car (cdr (cdr (cdr brush)))))
    
    (set! rndlength minlength)    ; this will get overwritten if
                                  ; maxlength > minlength
    ;; the work happens in this loop
    
    (if (< maxlength minlength)
        (gimp-message "The maximum line length can not be less than the minimum length")
        (begin
         ;; only execute the following loop if maxlength is >= minlength
            (while (> iterations 1)
                (set! iterations (- iterations 1))
                (set! px1 (+ x1 (random drw-width )))
                (set! py1 (+ y1 (random drw-height)))
                (aset *randompoint* 0 px1)
                (aset *randompoint* 1 py1)
                
                ;; generate a random angle in radians (0 < angle < 2 *  PI)
                (set! rndangle (/ (random 360) (* 2 *pi*)))
                ;; generate a random length between minlength and maxlength
                (if (> maxlength minlength)
                    (set! rndlength (+ minlength (random (- maxlength minlength)))))
                
                (set! px2 (+ px1 (* (cos rndangle) rndlength)))
                (set! py2 (+ py1 (* (sin rndangle) rndlength)))
                
                ;; calculate coordinates of this second point
                ;; if toggle to discard,.... check if point is in margin
                ;; area and if so, don't draw it.
                
                (aset *randompoint* 2 px2)
                (aset *randompoint* 3 py2)
                
                (if (= discardborder FALSE)
                    (gimp-pencil drw 4 *randompoint*))
                
                (if (and (= discardborder TRUE) (>= px2 x1) (<= px2 x2)
                            (>= py2 y1) (<= py2 y2))
                    (gimp-pencil drw 4 *randompoint*)
                )
                
            )
        )
    )
    
    
    ;; end of while loop
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    
    (gimp-context-pop)
  ) 
)


(script-fu-register "script-fu-randomlines"
                      "Random Lines"
                      "Draws a specified number of random lines using a nominated brush. \nfile:randomlines.scm"
                      "Charles Cave <charlesweb@optusnet.com.au>"
                      "Charles Cave"
                      "February 2008"
                      "RGB*, INDEXED*, GRAY*"
                      SF-IMAGE          "Image"         0
                      SF-DRAWABLE       "Drawable"      0
                      SF-ADJUSTMENT     "Margin (pixels)" '(0 0 100 1 6 0 0)
                      SF-BRUSH          "Brush"         '("Circle (01)" 100 1 0)
                      SF-ADJUSTMENT     "Brush size"     '(10 1 200 1 1 0 0)
                      SF-COLOR          "Color"          "black"
                      SF-ADJUSTMENT     "Iterations"     '(10 1 10000 10 100 0 0)
                      SF-ADJUSTMENT     "Minimum Line Length" '(5 1 500 1 100 0 0)
                      SF-ADJUSTMENT     "Maximum Line Length" '(25 1 3000 1 100 0 0)
                      SF-TOGGLE         "Discard lines outside margin" FALSE
                      )

  (script-fu-menu-register "script-fu-randomlines"
                           "<Image>/Script-Fu2/Artistic/")


;end of script