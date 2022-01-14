;;; Description:
;;; Creates a colored ice effect on an object or text layer with an alpha channel.
;;; -----------------------------------------------------------------------------
;;; Change Log:
;;; Version-1.0
;;; Version- 2.0 - Fix lightness slider
;;; version 2.1 - add RGB Color selection ; fluffybunny 2013
;;; 
; Information can be found at GimpChat.com
;
; License: GPLv3
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; To view a copy of the GNU General Public License
; visit: http://www.gnu.org/licenses/gpl.html

; Thank yous go out to Saulgoode, Graechan, and Samj at GimpChat.com for their help.

        ;Check Gimp version
(define (gimp-version-meets? check)
    (let ((c (append (map string->number (strbreakup check ".")) '(0 0 0)))
          (v (append (map string->number (strbreakup (car (gimp-version)) ".")) '(0 0 0)))
         )
        (if (> (car v) (car c)) #t
            (if (< (car v) (car c)) #f
                (if (> (cadr v) (cadr c)) #t
                    (if (< (cadr v) (cadr c)) #f
                        (if (>= (caddr v) (caddr c)) #t #f)
                    )
                )
            )
        )
    )
)

;;;; new stuff
; Define RGB to HSV functions
;
(define (ice-anything-do-rgb-to-hsv color)
        (let* (
                (r (car color))
                (g (cadr color))
                (b (caddr color))
                (cmin (min r (min g b)))
                (cmax (max r (max g b)))
                (diff 0.0)
                (rc 0.0)
                (gc 0.0)
                (bc 0.0)
                (h 0.0)
              )
            
            (set! diff (- cmax cmin))
            (if (= diff 0.0)
                (set! diff (+ diff 1.0))
            )
            (set! rc (/ (- cmax r) diff))
            (set! gc (/ (- cmax g) diff))
            (set! bc (/ (- cmax b) diff))
            (set! h  (/ (if (= r cmax)
                                (- bc gc)
                                (if (= g cmax)
                                    (+ 2.0 (- rc bc))
                                    (+ 4.0 (- gc rc))
                                )
                        )
                            6.0))
            
            (list (if (= cmin cmax)
                    0
                    (* 360 (if (< h 0.0)
                            (+ h 1.0)
                            h
                        )
                    )
                )
                (if (= cmin cmax)
                    0
                    (/ (- cmax cmin) cmax)
                )
                cmax
            )
        )
)

    ; RGB to HSV in gimp ranges
(define (ice-anything-conv-rgb-to-hsv color)
        (let*
            (
                (r (car color))
                (g (cadr color))
                (b (caddr color))
                (hsv (ice-anything-do-rgb-to-hsv (list (/ r 255.0) (/ g 255.0) (/ b 255.0))))
                (h (car hsv))
                (s (cadr hsv))
                (v (caddr hsv))
            )
            (list h (* s 100.0) (* v 100.0))
        )
    )

(define (script-fu-ice-anything image
								   drawable
								   icelength
								   turbulence
								   color)
;; new stuff
    (let*  (
                (hsv 0)
                (hue 0)
                (saturation 0)
                (value 0)
           )
        
        ;;begin image undo and reset
        (gimp-context-push)
        (gimp-image-undo-group-start image)   
       
        ;;prepare the text or object
        (gimp-layer-resize-to-image-size drawable) ; Resize the layer to image size
        (gimp-image-select-item image 0 drawable) ; Select the item on the layer
        ;(plug-in-plasma RUN-NONINTERACTIVE image drawable 0 turbulence) ;create ice grain effect
        (plug-in-solid-noise 1 image drawable TRUE TRUE 12999857 5 turbulence turbulence)
        
        (gimp-drawable-desaturate drawable 1) ;desat image with luminosity
        
        (gimp-displays-flush)
       
        ;;begin curves points array
        (gimp-curves-spline drawable HISTOGRAM-VALUE 14 (list->vector '(0 0 13 62 69 74 100 120 168 144 185 221 255 255)))
        (gimp-selection-none image) ; deselect the item on the layer   
        (gimp-image-rotate image 0) ;rotate to prepare for wind filter
        
        (gimp-displays-flush)
        
        ;;add Icicles
        (plug-in-wind RUN-NONINTERACTIVE image drawable 2 1 icelength 0 1)
        (gimp-image-rotate image 2) ;rotate back to original position
        
        (gimp-displays-flush)
        ;;color the ice 
        ;;;; new stuff
        (set! hsv (ice-anything-conv-rgb-to-hsv color))
        (set! hue (car hsv))
        (set! saturation (cadr hsv))
        (set! value (caddr hsv))
        (set! value (- value 100))
        ;(gimp-message (string-append "HSV " (number->string hue) " " (number->string saturation) " " (number->string value)))
        
        (gimp-drawable-colorize-hsl drawable hue saturation value) ;hue  ;saturation ;lightness
       
        ;;set all user settings back to before script ran or undo filter
        (gimp-displays-flush)
        (gimp-context-pop)
        (gimp-image-undo-group-end image)
        
    )
)

;register menu location and information   
(script-fu-register "script-fu-ice-anything"
        "<Image>/Script-Fu/Alpha-to-Logo/Ice Anything"
        "Creates an ice effect on a object or text layer. \nfile:ice-anything-gimp-2.8_RD.scm"
        "Rod Detmer"
        "Rod Detmer"
        "June 2013"
        "RGB*"
        SF-IMAGE       "Image"            0
        SF-DRAWABLE    "Drawable"         0
        SF-ADJUSTMENT      "Icicle Length"  '(5 1 100 1 1 0 1)         
        SF-ADJUSTMENT      "Ice Turbulence" '(6.0 0.1 12.0 1 1 1 1)
        ; new stuff
        SF-COLOR      "Ice Color"    '(117 182 203)
)

; end of script