;;
;; Borders, V2.8  User version A as the main one.
;
;; Martin Egger (martin.egger@gmx.net)
;; (C) 2012, Bern, Switzerland
;
;; This script was tested with Gimp 2.10.18
;;
;; New versions will be distributed from <http:// registry.gimp.org/> only
;;
;;This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses>. 
;;
; ;Define the function

(define (script-fu-eg-border-b inImage
                inLayer
                inOuterPercentWidth
                inSymmetric
                inPortrait
                ininnerPercent
                inOuterColor
                ininnerColor
                inFeather)
        ;;
        
  (let* (
            (TheImage (car (gimp-image-duplicate inImage)))
            (TheLayer (car (gimp-image-flatten TheImage)))
            (isrgb (car (gimp-drawable-is-rgb TheLayer)))
            (TheWidth (car (gimp-image-width TheImage)))
            (TheHeight (car (gimp-image-height TheImage)))
            ;
            ; W-Border = % of widht, H-Border = % of height (asymmetric)
            ;
            (outer-border-width (/ (* TheWidth inOuterPercentWidth) 100))
            (outer-border-top-height (/ (* TheHeight inOuterPercentWidth) 100))
            ;
            ; W-Border and H-Border = % of widht (symmetric)
            ;
            (outer-border-top-height (+ (* outer-border-width inSymmetric) (* outer-border-top-height (* -1 (- inSymmetric 1)))))
            (outer-border-bottom-height outer-border-top-height)
            ;
            ; W-Border and Top-H-Border = % of widht, Bottom-H-Border = (1.41*(TheWidth+2*Border) - Border - TheHeight) (asymmetric)
            ;
            (outer-border-top-height (+ (* outer-border-width inPortrait) (* outer-border-top-height (* -1 (- inPortrait 1)))))
            (temp-bottom (- (* 1.414214 (+ TheWidth (* 2 outer-border-width))) TheHeight outer-border-top-height))
            (outer-border-bottom-height (+ (* temp-bottom inPortrait) (* outer-border-top-height (* -1 (- inPortrait 1)))))
            ;
            (inner-border-width (/ (* (+ (* TheHeight inPortrait) (* TheWidth (* -1 (- inPortrait 1)))) ininnerPercent) 100))
            ;
            (image-width (+ TheWidth  (* 2 outer-border-width)))
            (image-height (+ TheHeight outer-border-bottom-height outer-border-top-height))
            (Old-FG-Color (car (gimp-context-get-foreground)))
        )
    
        (cond
            ((= isrgb FALSE) (gimp-image-convert-rgb TheImage))
        )
        (gimp-image-undo-disable TheImage)
        (gimp-selection-none TheImage)
        (gimp-item-set-name TheLayer "WithBorder")
        ;
        ; Generate the border
        ;
        (gimp-image-resize TheImage image-width image-height outer-border-width outer-border-top-height)
        ;
        (let* (
                (BorderLayer (car (gimp-layer-new TheImage image-width image-height RGBA-IMAGE "TempLayer" 100 LAYER-MODE-NORMAL)))
            )
            (gimp-image-insert-layer TheImage BorderLayer 0 -1)
            (gimp-edit-clear BorderLayer)
            ;
            (gimp-context-set-feather FALSE)
            ;
            (gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE 0 0 image-width image-height)
            (gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
            (gimp-context-set-foreground inOuterColor)
            (gimp-edit-fill BorderLayer FILL-FOREGROUND) 
            ;
            (if (> ininnerPercent 0) 
                    (begin
                        (gimp-context-set-feather inFeather)
                        (gimp-context-set-feather-radius (* 1.4 inner-border-width) (* 1.4 inner-border-width))
                        (gimp-image-select-rectangle TheImage CHANNEL-OP-REPLACE (- outer-border-width inner-border-width) (- outer-border-top-height inner-border-width) (+ TheWidth (* inner-border-width 2)) (+ TheHeight (* inner-border-width 2)))
                        (gimp-context-set-feather FALSE)
                        (gimp-image-select-rectangle TheImage CHANNEL-OP-SUBTRACT outer-border-width outer-border-top-height TheWidth TheHeight) 
                        (gimp-context-set-foreground ininnerColor)
                        (gimp-edit-fill BorderLayer FILL-FOREGROUND)
                    )
            )
            (gimp-image-merge-down TheImage BorderLayer CLIP-TO-IMAGE)
        )
        ;
        (gimp-selection-none TheImage)
        (gimp-display-new TheImage)
        (gimp-image-undo-enable TheImage)
        (gimp-context-set-foreground Old-FG-Color)
        
        ;
        ; Finish work
        ; 
        (gimp-displays-flush)
        (gc)
  ) ;;; let
  
  
)
 
; Register the function with the GIMP

(script-fu-register "script-fu-eg-border-b"
   "<Toolbox>/Script-Fu/Decor/Border Egger Original..."
   "Generate a border around an image. Version B \n:file egger-BorderB_02.scm"
   "Martin Egger martin.egger_gmx.net"
   "Martin Egger, Bern, Switzerland"
   "2012"
   "RGB* GRAY*"
   SF-IMAGE         "The Image" 0
   SF-DRAWABLE      "The Layer" 0
   SF-ADJUSTMENT    "Outer border size -width in percent" '(18 1.0 100 1.0 0 2 0)
   SF-TOGGLE        "Use symmetric outer borders" FALSE
   SF-TOGGLE        "Use portrait style border" FALSE
   SF-ADJUSTMENT    "inner border size -in percent" '(0.20 0.0 10.0 0.1 0 2 0)
   SF-COLOR         "Outer border color" '(255 255 255)
   SF-COLOR         "Inner border color" '(0 0 0)
   SF-TOGGLE        "Feather inner border" FALSE
)

; end of script