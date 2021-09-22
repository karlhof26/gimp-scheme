;; random.scm -*-scheme-*- 
;; Use a brush to paint to paint random pixels a nominated number
;; of times to produce a "random" pattern/.
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


(define (script-fu-random   img drw
                            margin
                            brush
                            brushsize
                            color
                            iterations)
        
    (let* (
            (x1 0)
            (y1 0)
            (ctr 0)
            (x2 (car (gimp-image-width img)))
            (y2 (car (gimp-image-height img)))
            
            (*randompoint* (cons-array 2 'double))
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
        
        (gimp-context-set-foreground color)
        (gimp-context-set-brush (car brush))
        (gimp-context-set-brush-size brushsize)
        (gimp-context-set-opacity (car (cdr brush)))
        (gimp-context-set-paint-mode (car (cdr (cdr (cdr brush)))))
        
        (gimp-progress-set-text "Rendering Random brush strokes")
        (gimp-message (number->string (rand 1000)))
        
        (while (< ctr iterations)
            (set! ctr (+ ctr 1))
            (aset *randompoint* 0 (+ x1 (random drw-width )))
            (aset *randompoint* 1 (+ y1 (random drw-height)))
            (gimp-pencil drw 2 *randompoint*)
        )
        
        
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        
        (gimp-context-pop)
    )
)

(script-fu-register "script-fu-random"
                    "Random Points using brush"
                    "Draws a specified number of random points using a nominated brush. \nfile:random.scm"
                    "Charles Cave <charlesweb@optusnet.com.au>"
                    "Charles Cave"
                    "February 2008"
                    "RGB* INDEXED* GRAY*"
                    SF-IMAGE        "Image"         0
                    SF-DRAWABLE     "Drawable"      0
                    SF-ADJUSTMENT   "Margin (pixels)" '(0 0 100 1 6 0 0)
                    SF-BRUSH        "Brush"         '("Circle (01)" 100 1 0)
                    SF-ADJUSTMENT   "Brush size"     '(10 1 100 1 1 0 0)
                    SF-COLOR        "Color"          "black"
                    SF-ADJUSTMENT   "Iterations"     '(10 1 10000 10 100 0 0) 
    )

(script-fu-menu-register "script-fu-random"
                         "<Image>/Script-Fu/Artistic")

;end of script