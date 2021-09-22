; Technicolor rel 0.01
; Created by Graechan
; Comments directed to http://gimpchat.com or http://gimpscripts.com
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
; Rel 0.01 - Initial Release 
;
(define (script-fu-technicolor 
                               image
                               layer
                               mode
							   gradient
							   blend-repititions
							   displace-repititions
							   keep-selection
							   conserve
							   )
	
    (gimp-image-undo-group-start image)	

 (let* (
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (paint-layer (car (gimp-layer-new image width height RGBA-IMAGE "Paint-Layer" 100 mode)))
            (noise (car (gimp-layer-new image width height RGBA-IMAGE "Solid Noise" 100 LAYER-MODE-NORMAL)))
            (sel (car (gimp-selection-is-empty image)))
            (selection-channel 0)
            (cnt blend-repititions)
            (width-box 0)
            (height-box 0)
            (ver 2.8)
            (x1 0)
            (y1 0)
            (x2 0)
            (y2 0)
        )
    (cond ((not (defined? 'gimp-image-get-item-position)) (set! ver 2.6))) ;define the gimp version
    
    (gimp-context-push)
    (gimp-context-set-paint-method "gimp-paintbrush")
    (if (= ver 2.8) (gimp-context-set-dynamics "Dynamics Off"))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    (if (= sel TRUE)
        (begin
            (cond ((= ver 2.8) (gimp-image-select-item image 2 layer)) 
                (else (gimp-selection-layer-alpha layer))
            ) ;endcond
        )
    ) ;endif
    
    ;;;;create selection-channel (gimp-selection-load selection-channel)	
    (set! selection-channel (car (gimp-selection-save image)))
    (cond ((= ver 2.8) (gimp-item-set-name selection-channel "selection-channel"))
        (else (gimp-drawable-set-name selection-channel "selection-channel"))
    ) ;endcond
    (gimp-selection-none image)
    
    ;;;;begin the script here--------------------------------------------------------------------------------------------------------	
    (include-layer image paint-layer layer 0)	;stack 0=above 1=below
    (gimp-drawable-fill paint-layer FILL-BACKGROUND)
    
    
    (gimp-context-set-gradient gradient) 
    (while (> cnt 0)
        (set! width-box (round (random width )))
        (set! height-box (round (random height)))
                    ; set the arrays
        (set! x1 width-box)    
        (set! y1 height-box)
        (set! x2 (+ width-box 5))
        (set! y2 (+ height-box 5))	
        
        (gimp-edit-blend paint-layer BLEND-CUSTOM LAYER-MODE-DIFFERENCE GRADIENT-CONICAL-SYMMETRIC 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE x1 y1 x2 y2)
        
        (set! cnt (- cnt 1))
    ) ;endwhile
    
    (cond ((= ver 2.8)                         ;insert or add the new layer
            (gimp-image-insert-layer image noise 0 1)) ;new 2.8
            (else (gimp-image-add-layer image noise 1)) ;new 2.6
    ) ;endcond
    
    (plug-in-solid-noise 1 image noise FALSE FALSE 1611597286 1 (/ width 100) (/ height 100))
    
    (gimp-image-set-active-layer image paint-layer)
    (set! cnt displace-repititions)
    (while (> cnt 0)
        (plug-in-displace 1 
                    image 
                    paint-layer ;drawable 
                    0 ;amount-x Displace multiplier for X or radial direction 
                    50 ;amount-y Displace multiplier for Y or tangent (degrees) direction
                    TRUE ;do-x Displace in X or radial direction?
                    TRUE ;Displace in Y or tangent direction?
                    noise ;displace-map-x Displacement map for X or radial direction[drawable] 
                    noise ;displace-map-y Displacement map for Y or tangent direction[drawable]
                    2) ;Edge behavior { WRAP (1), SMEAR (2), BLACK (3) }
        (set! cnt (- cnt 1))
    ) ;endwhile
    
    (gimp-selection-load selection-channel)
    (gimp-selection-shrink image 1)
    (gimp-selection-invert image)
    (gimp-edit-clear paint-layer)
    (gimp-selection-none image)
    
    
    (gimp-image-remove-layer image noise)
    (if (= keep-selection TRUE) (gimp-selection-load selection-channel))
    (gimp-image-remove-channel image selection-channel)
    (gimp-curves-spline paint-layer 0 10 #(0 0 86 50 128 129 172 207 255 255))
    (if (= conserve FALSE) (set! layer (car (gimp-image-merge-down image paint-layer EXPAND-AS-NECESSARY))))
    
    (gimp-displays-flush)
    (gimp-context-pop)
    (gimp-image-undo-group-end image)
    
 )
) 

(script-fu-register "script-fu-technicolor"        		    
  "Technicolor"
  "Chages layer or selection to Technicolors. \nfile:Technicolor.scm"
  "Graechan"
  "Graechan - http://gimpchat.com"
  "May 2014"
  "RGB*"
  SF-IMAGE      "image"      0
  SF-DRAWABLE   "drawable"   0
  SF-ENUM "Paint LayerMode" '("LayerModeEffects" "normal-mode")
  SF-GRADIENT   "Effect Gradient" "Full saturation spectrum CCW"
  SF-ADJUSTMENT "Blend Repititions" '(12 0 50 1 1 0 0)
  SF-ADJUSTMENT "Displace Repititions" '(8 0 20 1 1 0 0)
  SF-TOGGLE     "Keep selection"          FALSE
  SF-TOGGLE     "Keep the Layers"   FALSE  
)

(script-fu-menu-register "script-fu-technicolor" "<Image>/Script-Fu/Patterns")


