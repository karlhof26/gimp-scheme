; 
; Filtered B&W for GIMP  
; Copyright (C) 2006-8 Lukasz Komsta http://www.komsta.net/
; 
; --------------------------------------------------------------------
; version 0.1  2006/07/12
;     - Initial relase
; version 0.11 2008/08/08
;     - a hack to work with GIMP 2.4 (local variables issue)
;version 0.12 2020/04/26
;     - a hack to work with GIMP 2.10.18 
; --------------------------------------------------------------------
;
; This script converts color images to B&W using channel mixer settings,
; which simulate the use of color filters in B&W photography.
;
; Avoid setting intensity to 1, besause it totally filters one of
; RGB channels and increases noise.
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


(define (script-fu-filtered-bw img drawable filter saturation mono factor boost rotdegree)
    (define (floor x) (- x (fmod x 1))) 
    
    ; This function taken from the other scripts

    (define (hsv-to-rgb color)
        (let* (
                (h (car color))
                (s (cadr color))
                (v (caddr color))
            )
            
            ;flip the hue around ( to align to the options list)
            (set! h (+ h 180))
            (if (>= h 360)
                (set! h (- h 360))
            )
            
            (if (= s 0)
                (begin
                    ;(gimp-message "line45 s=0")
                    (list v v v)
                )
                (begin
                    ;(gimp-message "line49 s!=0")
                        (let* (
                                (quad (/ h 60))
                                (i (floor quad))
                                (f (- quad i))
                                (p (* v (- 1 s)))
                                (q (* v (- 1 (* s f))))
                                (t (* v (- 1 (* s (- 1 f)))))
                        )
                        ;debug
                        ;(gimp-message "line58")
                        ;(gimp-message (number->string i))
                        
                        (cond
                            ((= i 0) (list v t p))
                            ((= i 1) (list q v p))
                            ((= i 2) (list p v t))
                            ((= i 3) (list p q v))
                            ((= i 4) (list t p v))
                            
                           ;(t (list v p q))
                           (else (list v p q))
                        
                        )
                        
                    )
                )
            )
           
        )
    )
    
    
    
    (let* (
            (saturation (/ saturation 100))
            (hue (+ (+ (* filter 30) rotdegree) 7)) ; the plus 7 is to adjust to better start position
            (rgb (hsv-to-rgb (list hue saturation 1)))
            (r (car rgb))
            (g (cadr rgb))
            (b (caddr rgb))
            (sum (+ (+ r g) b) )
            
            (finr 0) ; final factor red
            (fing 0) ; final factor green
            (finb 0) ; final factor blue
            (fr 0)   ; final red unfactored
            (fg 0)   ; final green unfactored
            (fb 0)   ; final blue unfactored
        )
        ;(gimp-message (number->string b))
        
        (set! boost (/ boost 10))
            (set! r (* (/ r sum) boost))
            (set! g (* (/ g sum) boost))
            (set! b (* (/ b sum) boost))
            ;debug
            ;(gimp-message (number->string r))
            
            
            (set! factor (/ factor 100))
            
            (set! finr (min (* r factor) 2.0))
            (set! fing (min (* g factor) 2.0))
            (set! finb (min (* b factor) 2.0))
            (set! fr (min r 2.0))
            (set! fg (min g 2.0))
            (set! fb (min b 2.0))
        
        (gimp-image-undo-group-start img)
        ;(gimp-selection-none img)
        ;(plug-in-colors-channel-mixer 1 img drawable FALSE r g b r g b r g b)
        (plug-in-colors-channel-mixer 1 img drawable mono finr fg fb  fr fing fb  fr fg finb)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gc); garbage cleanup; memory cleanup
    )
    
)

(script-fu-register "script-fu-filtered-bw"
    "<Toolbox>/Script-Fu/Colors/Filtered BW"
    "Simulate B&W photo with color filter. Each color represents 30deg in the color wheel. Turn on monochrome to keep strict B&W. \nfile: filtered-bw_02.scm"
    "Lukasz Komsta"
    "karlhof26"
    "2006 and 2020"
    "RGB* GRAY*"
    SF-IMAGE    "Image"              0
    SF-DRAWABLE "Layer to blur"      0
    SF-OPTION    "Filter"            '("Orange" "Yellow" "Lime-green" "Green" "Cold-green" "Cyan" "Light blue" "Blue" "Cold-violet" "Hot-violet" "Magenta" "Red") 
    SF-ADJUSTMENT   "Saturation"     '(75 0 100 1 1 1 0)
    SF-TOGGLE       "Monochrome"                    FALSE
    SF-ADJUSTMENT   "Grey percent (100=Grey)"       '(75 0 100 1 1 1 0)
    SF-ADJUSTMENT   "Color Boost"                   '(10 1 30 1 1 1 0)
    SF-ADJUSTMENT   "Color Extra Rotate degrees"    '(0 0 30 1 1 1 0)
)

;end of script