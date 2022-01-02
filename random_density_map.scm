; random_density_map.scm
; by Rob Antonishen 
; http://ffaat.pointclark.net

; Version 2.1 (20090724)

; Description
;
; Script to draw a number of random points with the currently 
; selected brush, using a density mask.
; Will appear in Filters->Map Menu
;

; Changes
; 2.0 - conplete rewritten
; 2.1 - aded option to choose paintbrush or pencil

; originally  based on random.scm
; by Charles Cave <charlesweb@optusnet.com.au>
; http://members.optusnet.com.au/~charles57/GIMP

; License:
;
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html
(define (script-fu-random-density-map img inLayer inDensityMap inInvertMap inUseAlpha inDensity inMargin inSpacing inJitter inJitterAmt inTool)
  (let* (
        (x1 0)
        (y1 0)
        (ctr 0)
        (x2 (car (gimp-drawable-width inLayer)))
        (y2 (car (gimp-drawable-height inLayer)))
        (xMap (car (gimp-drawable-width inDensityMap)))
        (yMap (car (gimp-drawable-height inDensityMap)))
        (x 0)
        (y 0)
        (map-type 0)
        (get-val 0)
        (test 0)
        (thresh 0)
        (*newpoint* (cons-array 2 'double))
        (drw-width 0)
        (drw-height 0)
        (xcount 0)
        (ycount 0)
        (lc 0)
        )
        
    ;get map type:  RGB-IMAGE (0), RGBA-IMAGE (1), GRAY-IMAGE (2), GRAYA-IMAGE (3), INDEXED-IMAGE (4), INDEXEDA-IMAGE (5) 
    (set! map-type (car (gimp-drawable-type inDensityMap)))
    
    
    ; make sure maps isn't indexed
    (if (or (= map-type INDEXED-IMAGE) (= map-type INDEXEDA-IMAGE))
        (gimp-message "Density Map can not be indexed!") ; error
        (if (or (not (= x2 xMap)) (not (= y2 yMap)))
            (gimp-message "Density Map must be the same size as the active layer!") ; error
            ;it begins here
            (begin 
                ; define drawable area for the algorithm
                (set! x1 (+ x1 inMargin))
                (set! x2 (- x2 inMargin))
                (set! y1 (+ y1 inMargin))
                (set! y2 (- y2 inMargin))  
                (set! drw-width  (- x2 x1))
                (set! drw-height (- y2 y1))		 
                (gimp-context-push)
                (gimp-image-undo-group-start img)
                
                ; set up progress bar
                (gimp-progress-set-text _"Rendering Random Density Map")
                (while (< ycount drw-height)
                    (begin
                        (while (< xcount drw-width)
                            (begin
                                ;get the pixel value
                                (set! x (+ xcount x1))
                                (set! y (+ ycount y1))
                                (if (and (= inJitter TRUE) (> inJitterAmt 0))
                                    (begin
                                        (set! x (min (max (- (+ x (random (/ (* inSpacing inJitterAmt 1000) 1000))) (*  inSpacing 0.5 inJitterAmt)) x1) x2))
                                        (set! y (min (max (- (+ y (random (/ (* inSpacing inJitterAmt 1000) 1000))) (*  inSpacing 0.5 inJitterAmt)) y1) y2))
                                    )
                                )
                                (set! get-val (cadr (gimp-drawable-get-pixel inDensityMap x y)))
                                
                                ; pick a number, any number and scale it by the density function
                                (set! thresh (/ (* (random 255.0) 100.0) inDensity))
                                
                                (if (and (= inUseAlpha TRUE) (= (car (gimp-drawable-has-alpha inDensityMap)) TRUE))   ; if using alpha
                                    (begin
                                        (set! test (aref get-val 3))   ; test threshold against alpha ( index 3)
                                        (gimp-context-set-foreground (list (aref get-val 0) (aref get-val 1) (aref get-val 2)))  ;set brush colour
                                    )
                                    (if (or (= map-type GRAY-IMAGE) (= map-type GRAYA-IMAGE))  ;  else if greyscale
                                        (set! test (aref get-val 0))  ; test threshold against colour ( index 0)
                                        (if (or (= map-type RGB-IMAGE) (= map-type RGBA-IMAGE))  ; else if colour 
                                            (set! test (/ (+ (aref get-val 0) (aref get-val 1) (aref get-val 2)) 3.0)) ; test threshold against average of r g and b
                                            (set! test 255) ;  else threshold to full value (always draw)
                                        )
                                    )
                                )
                                (if (= inInvertMap TRUE)  ; reverse the map if invert option
                                    (set! test (- 255 test))
                                )
                                (if (< thresh test)    ; compare threshold against random value
                                    (begin
                                        (aset *newpoint* 0 x)   ; set the paint array
                                        (aset *newpoint* 1 y)
                                        (cond
                                            ((equal? inTool 0) (gimp-paintbrush-default inLayer 2 *newpoint*)) ; paint point with paintbrush
                                            ((equal? inTool 1) (gimp-pencil inLayer 2 *newpoint*)) ; paint point with paintbrush
                                        )
                                    )
                                )
                                (set! xcount (+ xcount inSpacing))
                            )
                        )
                        (set! ycount (+ ycount inSpacing))
                        (gimp-progress-update (/ (* xcount ycount) (* drw-height drw-width) ))  ;update progress bar
                        (gimp-displays-flush)
                        (set! xcount 0)
                    )
                )
                
                (gimp-progress-end)
                (gimp-image-undo-group-end img)
                (gimp-displays-flush)
                (gimp-context-pop)
            )
        )
    )
  )  
)

(script-fu-register "script-fu-random-density-map"
                    "<Image>/Script-Fu2/Artistic/Random Density Map..."
                    "Draw a number of random points with the currently selected brush, using a density mask. Density according to whiteness or darkness in map image. Use paintool settings. \nfile:random_density_map.scm"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "April 2008"
                    "RGB*, GRAY*"
                    SF-IMAGE       "Image"         0
                    SF-DRAWABLE    "Drawable"      0
                    SF-DRAWABLE    "Density Map"   -1
                    SF-TOGGLE      "Invert Map"    FALSE
                    SF-TOGGLE      "Use map alpha channel for Density and draw with map colour"  FALSE
                    SF-ADJUSTMENT  "Density"           '(100 1 100 1 10 2 0) 
                    SF-ADJUSTMENT  "Border Margin (pixels)"       '(1 1 100 1 6 0 0)
                    SF-ADJUSTMENT  "Point Spacing"     '(10 1 100 1 10 0 0) 
                    SF-TOGGLE      "Apply Jitter"      TRUE
                    SF-ADJUSTMENT  "Jitter Amount"     '(0.5 0 2 0.01 0.1 2 0) 
                    SF-OPTION      "Paint Tool"        '("Paintbrush" "Pencil")
)

; end of script