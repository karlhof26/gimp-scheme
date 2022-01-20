;; starpolygon.scm -*-scheme-*- 
;;
;; A Star Polygon is a figure identified as {p/q}, with p,q being positive
;; integers,formed by connecting with straight lines every q th point out
;; of p regularly spaced points lying on the circumference of a circle.
;; For example, a Star Polygon {6,2} is drawn by drawing 6 evenly spaced points 
;; on a circle then drawing lines between every 2 points.

;; Reference http://mathworld.wolfram.com/StarPolygon.html
;; Script at http://members.optusnet.com.au/~charles57/GIMP
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

(define (script-fu-starpolygon     img drw
                                   p q radius roffset
                                   xoffset yoffset
                                   pointsonly brush
                                   color brushsize)
    (let* (
            (anglesector 0)   
            (niter 0)
            (anglecurr 0)
            (thisx 0)
            (thisy 0)
            (itrpol 0)   
            (xcenter (/ (car (gimp-image-width img)) 2))
            (ycenter (/ (car (gimp-image-height img)) 2))
            (pi 3.14159265)

            (*linepoints* (cons-array 4 'double))
            (*polygon* (cons-array 600 'double))
          )
        
        (gimp-context-push)
        (gimp-image-undo-group-start img)
        
        (gimp-context-set-foreground color)
        (gimp-context-set-brush (car brush))
        (gimp-context-set-brush-size brushsize)
        
        (gimp-context-set-opacity (car (cdr brush)))(tracing 0)
        (gimp-context-set-paint-mode (car (cdr (cdr (cdr brush)))))
        
        ;; Trig functions in Scheme use radians for angular measurement
        ;; The formula to convert degrees to radians is
        ;; Radians = degress * PI / 180
        (set! anglecurr (/ (* (- 180 roffset) pi) 180))
        (set! anglesector (/ (* 2 pi) p))   
        
        ;; Step 1. Determine the co-ordinates of the points on the circle
        (while (< niter p)
            (set! thisy (+ yoffset (+ ycenter (* radius (sin (- (/ pi 2) anglecurr))))))
            (set! thisx (+ xoffset (+ xcenter (* radius (sin anglecurr)))))
            
            (aset *polygon* itrpol thisx)
            (set! itrpol (+ 1 itrpol))
            (aset *polygon* itrpol thisy)
            (set! itrpol (+ 1 itrpol))
            
            (if (= pointsonly TRUE)
                (begin 
                    (aset *linepoints* 0 thisx) 
                    (aset *linepoints* 1 thisy) 
                    (gimp-pencil drw 2 *linepoints*) 
                )
            )
            
            (set! niter (+ 1 niter))
            (set! anglecurr (+ anglecurr anglesector))
        )
        
    ;; Step 2. Draw the Star Polygon  
    (set! niter 0)
    (while (< niter p)
         (aset *linepoints* 0 (aref *polygon* (* niter 2)))
         (aset *linepoints* 1 (aref *polygon* (+ 1 (* niter 2))))
         (aset *linepoints* 2 (aref *polygon* (* (modulo (+ niter q) p) 2)))
         (aset *linepoints* 3 (aref *polygon* (+ 1 (* (modulo (+ niter q) p) 2))))
         (if (= pointsonly FALSE )
             (gimp-pencil drw 4 *linepoints*))
         (set! niter (+ 1 niter))
    )
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gc) ; memory cleanup ; array was used
 ) 
)

(script-fu-register "script-fu-starpolygon"
                    "Star Polygon"
                    "Draws a Star Polygon. \nfile:starpolygon.scm"
                    "Charles Cave <charlesweb@optusnet.com.au>"
                    "Charles Cave"
                    "May 2008"
                    "RGB*, INDEXED*, GRAY*"
                    SF-IMAGE       "Image"         0
                    SF-DRAWABLE    "Drawable"      0
                    SF-ADJUSTMENT _"P (points on circle)"   '(7 3 300 1 10 0 1)
                    SF-ADJUSTMENT _"Q (density)"            '(4 1 300 1 10 0 1)
                    SF-ADJUSTMENT _"Radius "                '(120 0 1000 1 10 0 0)
                    SF-ADJUSTMENT _"Offset (degrees) "      '(0 -180 180 1 10 0 0)
                    SF-ADJUSTMENT _" X Offset (pixels) "    '(0 -300 300 1 10 0 0)
                    SF-ADJUSTMENT _" Y Offset (pixels) "    '(0 -300 300 1 10 0 0)
                    SF-TOGGLE     _"Draw points only"       FALSE

                    SF-BRUSH      "Brush"         '("Circle (01)" 100 1 0)
                    SF-COLOR      "Color"          "blue"
                    SF-ADJUSTMENT "Brush size"    '(1 0 300 1 10 0 0)
                    )

(script-fu-menu-register "script-fu-starpolygon"
                         "<Toolbox>/Script-Fu/Draw/")
                         
; end of script
