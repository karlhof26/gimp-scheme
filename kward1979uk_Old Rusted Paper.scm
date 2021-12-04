; Old Rusted Paper
; Created by kward1979 
;  
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
;
; Rel 0.02 - Updated to Gimp 2.10.24

(define (add-text t-colour theImage text font-size font) 
    (gimp-context-set-foreground t-colour)
    (let* (
            (selection-bounds (gimp-selection-bounds theImage))
            (sx1 (cadr selection-bounds))
            (sy1 (caddr selection-bounds))
            (sx2 (cadr (cddr selection-bounds)))
            (sy2 (caddr (cddr selection-bounds)))
            (text-layer (car (gimp-text-fontname theImage -1 0 0 text 0 TRUE font-size PIXELS font)))
            (swidth  (- sx2 sx1))
            (sheight (- sy2 sy1))
            (hdiff (/ (- sheight (car (gimp-drawable-height text-layer))) 2 ))
            (wdiff (/ (- swidth (car (gimp-drawable-width text-layer))) 2 ))
        )
        (gimp-layer-translate  text-layer (+ sx1 wdiff) (+ sy1 hdiff) )
    )
)

(define (rusted-paper inImage inlayer distress p-colour shadow text-req text font font-size t-colour)
    (gimp-image-undo-group-start inImage)
    (let* (
            (OldFG (car (gimp-context-get-foreground)))
            (OldBG (car (gimp-context-get-background)))
            (theImage inImage)
            (theHeight (car (gimp-image-height theImage)))
            (theWidth (car (gimp-image-width theImage)))
            
            (draw-offset-x (car (gimp-drawable-offsets inlayer)))
            (draw-offset-y (cadr (gimp-drawable-offsets inlayer)))
            (has-sel       (car (gimp-drawable-mask-intersect inlayer)))
            (sel-offset-x  (cadr (gimp-drawable-mask-intersect inlayer)))
            (sel-offset-y  (caddr (gimp-drawable-mask-intersect inlayer)))
            (offset-x      0)
            (offset-y      0)
          )
        (if (= 1 (car (gimp-selection-is-empty theImage)))  
            (gimp-selection-all theImage)
        )
        
        (let* (
                (paper-layer (car (gimp-layer-new theImage theWidth theHeight
                    RGBA-IMAGE "paper" 100 LAYER-MODE-NORMAL)))
                (rust-layer (car (gimp-layer-new theImage theWidth theHeight
                    RGBA-IMAGE "rust" 100 LAYER-MODE-OVERLAY)))
                (spots-layer (car (gimp-layer-new theImage theWidth theHeight
                    RGBA-IMAGE "spots" 100 LAYER-MODE-OVERLAY)))
                (noise-layer (car (gimp-layer-new theImage theWidth theHeight
                    RGBA-IMAGE "noise" 100 LAYER-MODE-OVERLAY)))
                (noise-layer2 (car (gimp-layer-new theImage theWidth theHeight
                    RGBA-IMAGE "noise" 100 LAYER-MODE-SOFTLIGHT)))
                
              )
            (gimp-image-insert-layer theImage paper-layer 0 0)
            
            (gimp-drawable-fill paper-layer 3)
            
            (gimp-image-insert-layer theImage rust-layer 0 0)
            
            (gimp-drawable-fill rust-layer 3)
            
            (gimp-image-insert-layer theImage spots-layer 0 0)
            
            (gimp-drawable-fill spots-layer 3)
            
            (gimp-image-insert-layer theImage noise-layer 0 0)
            
            (gimp-drawable-fill noise-layer 3)
            
            (gimp-image-insert-layer theImage noise-layer2 0 1)
            
            (gimp-drawable-fill noise-layer2 3)
            ;(gimp-message "line83")
            (gimp-displays-flush)
            
            (if (= distress TRUE)
                (script-fu-distress-selection theImage paper-layer 127 8 4 2 TRUE TRUE)
                
            )
            
            (gimp-context-set-foreground p-colour)
            (gimp-edit-bucket-fill paper-layer 0 0 100 0 0 0 0)
            
            (gimp-context-set-pattern "Slate")
            (gimp-edit-bucket-fill rust-layer 2 0 100 0 0 0 0)
            
            
            (gimp-context-set-brush "Circle Fuzzy (13)")
            (gimp-context-set-foreground '(0 0 0))
            (gimp-drawable-edit-stroke-selection spots-layer)
            
            ;(gimp-message "line102")
            
            ;(plug-in-plasma 1 theImage noise-layer 13690514 3)
            (gimp-context-set-foreground p-colour)
            (gimp-edit-fill noise-layer FILL-FOREGROUND)
            ;(script-fu-difference-clouds 1 theImage noise-layer)
            (plug-in-rgb-noise 1 theImage noise-layer TRUE FALSE 0.6 0.6 0.9 0.2)
            
            (gimp-drawable-desaturate noise-layer DESATURATE-LIGHTNESS)
            (plug-in-sel-gauss 1 theImage noise-layer 5 100)
            (gimp-layer-set-mode noise-layer LAYER-MODE-SOFTLIGHT)
            
            
            
            (plug-in-solid-noise 1 theImage noise-layer2 TRUE FALSE 19567421 3 5.0 5.0)
            
            
            (if (= shadow TRUE)
                (begin
                 ;(gimp-message "To drop Shadow")
                 (script-fu-drop-shadow theImage noise-layer 8 8 15 '(0 0 0) 80 1)
                )
            )
            (if (= text-req TRUE)
                (add-text t-colour theImage text font-size font)
            )
            (gimp-displays-flush) 
            (gc)
        )
    )
    (gimp-image-undo-group-end inImage)
)

(script-fu-register "rusted-paper"
    "<Image>/Script-Fu2/Selection Effects/Rusted-paper..."
    "Take a users selection and turns it into a aged paper effect with the option of text. \nfile: kward1979uk_Old Rusted Paper.scm"
        "Karl Ward"
        "Karl Ward"
        "Oct 2005"
        ""
        
        SF-IMAGE      "SF-IMAGE" 0
        SF-DRAWABLE   "SF-DRAWABLE" 0
        SF-TOGGLE     "Distress selection" TRUE
        SF-COLOR      "Paper Colour" '(207 194 162)
        SF-TOGGLE     "Apply drop-shadow" TRUE
        SF-TOGGLE     "Text Required" FALSE
        SF-STRING     "Text (IF NO TEXT LEAVE BLANK)" ""
        SF-FONT       "Font" ""
        SF-ADJUSTMENT "Font-size" '(15 10 300 1 10 0 1)
        SF-COLOR      "TEXT Colour" '(0 0 0)
)

;end of script