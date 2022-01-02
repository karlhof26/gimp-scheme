; Smokin rel 0.04
; Created by Graechan
;
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
; Rel 0.02 - Added transparency fill to smoke layer so it's less like to fail using a selection
; Rel 0.03 - Added a 'Breeze distortion' setting and a transparent background option
; Rel O.04 - added 'Background Type' options to script and included some bugfixes to make the script more intuitive
;
;
(define (script-fu-smokin-logo 
                               text
                               letter-spacing
                               line-spacing
                               font-in 
                               font-size
                               color
                               warp
                               length 
                               angle
                               breeze
                               trans-in
                               conserve)
    
  (let* (
            (width 100)
            (height 100)
            (offx 0)
            (offy 0)
            (image (car (gimp-image-new 256 256 RGB)))         
            (border (/ font-size 4))
            (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
            (size-layer (car (gimp-text-fontname image -1 0 0 text border TRUE font-size PIXELS font)))
            (final-width (car (gimp-drawable-width size-layer)))
            (final-height (car (gimp-drawable-height size-layer)))
            (text-layer 0)
            (text-selection 0)
            (blur 20)
            (bkg-layer 0)
            (trans (cond 
                    ((= trans-in 0) 1)
                    ((= trans-in 1) 2)
                    ((= trans-in 2) 0)
                    (trans)
                   )
            )
        )
        
    (gimp-context-push)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;;;;Add the text layer for a temporary larger Image size
    (set! text-layer (car (gimp-text-fontname image -1 0 0 text (round (/ 350 4)) TRUE 350 PIXELS font)))
    ;;;;adjust text 
    (gimp-text-layer-set-justification text-layer 2)
    (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
    (gimp-text-layer-set-line-spacing text-layer line-spacing)
    ;;;;set the new width and height
    (set! width (car (gimp-drawable-width text-layer)))
    (set! height (car (gimp-drawable-height text-layer)))    
    (gimp-image-remove-layer image size-layer)
    (gimp-image-resize-to-layers image)
    
    ;;;;set the text clolor    
    (gimp-selection-layer-alpha text-layer)
    ;;;;create text-selection (gimp-selection-load text-selection)    
    (set! text-selection (car (gimp-selection-save image)))	
    (gimp-drawable-set-name text-selection "text-selection")
    (gimp-selection-none image)
    (gimp-edit-clear text-layer)
    (plug-in-gauss-rle2 RUN-NONINTERACTIVE image text-selection blur blur)
    (gimp-levels text-selection 0 105 149 1.00 0 255)
    (gimp-selection-load text-selection)
    (gimp-edit-fill text-layer FILL-FOREGROUND)
    (gimp-selection-none image)
    
    ;;;; apply the text effect
    (if (= trans-in 2)
        (begin
            (script-fu-lava image text-layer 10 10 7 "German flag smooth" FALSE FALSE FALSE)
            (set! text-layer (car (gimp-image-get-active-layer image)))
        )
    )
    
    ;;;;resize the text-layer
    (gimp-image-set-active-layer image text-layer)
    (gimp-layer-resize-to-image-size text-layer)    
    
    
    (script-fu-smokin image text-layer
                                color
                                warp
                                length 
                                angle
                                (round (/ breeze 2))
                                trans
                                conserve)
    
    ;;;;Scale Image to it's original size;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (gimp-image-scale image final-width final-height)
    (set! width (car (gimp-image-width image)))
    (set! height (car (gimp-image-height image)))   
    
    (if (= conserve FALSE)
        (begin
            (set! text-layer (car (gimp-image-merge-visible-layers image 0)))
            (gimp-drawable-set-name text-layer text)
        )
    )
    (gimp-image-remove-channel image text-selection)
    
    (gimp-context-pop)
    (gimp-display-new image)
    
  )
)
 
(script-fu-register "script-fu-smokin-logo"
    "Smokin"
    "Can create text in the form of smoke. \nfile:Smokin_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "May 2013"
    ""
    SF-TEXT       "Text"    "Smokin"
    SF-ADJUSTMENT "Letter Spacing" '(-10 -100 100 1 5 0 0)
    SF-ADJUSTMENT "Line Spacing" '(0 -100 100 1 5 0 0)
    SF-FONT       "Font"               "Arial Bold"
    SF-ADJUSTMENT "Font size (pixels)" '(350 6 500 1 1 0 1)
    SF-COLOR      "Smoke color"         '(171 171 255)
    SF-ADJUSTMENT "Warp" '(30 0 100 1 5 0 0)
    SF-ADJUSTMENT "Motion Length" '(50 1 256 1 5 0 0)
    SF-ADJUSTMENT "Motion Angle" '(90 0 360 1 5 0 0)
    SF-ADJUSTMENT "Breeze Distotion" '(0 0 20 1 5 0 0)
    SF-OPTION "Background Type" '("Black" "Transparent" "With Lava Text")
    SF-TOGGLE     "Keep the Layers"   FALSE
)
  
(script-fu-menu-register "script-fu-smokin-logo" "<Image>/Script-Fu/Logos")
 
(define (script-fu-smokin image layer
                               color
                               warp
                               length 
                               angle
                               breeze
                               trans
                               conserve
        )
        
    (if (and (= trans 0) (> (car (gimp-image-get-layers image)) 1)) (set! trans 2))
    (gimp-image-undo-group-start image)
    (gimp-layer-resize-to-image-size layer)
    
 (let* (
            (smoke-layer (car (gimp-layer-copy layer TRUE)))
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
            (original-width width)
            (original-height height)
            (area (* 1000 1000))
            (alpha (car (gimp-drawable-has-alpha layer)))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (car (gimp-drawable-get-name layer)))
            (layerList (vector->list (cadr (gimp-image-get-layers image))))
            (layerId 0)
            (selection-channel 0)
            (copy-layer 0)
            (visible 0)
            (pasted 0)
            (visible-copy 0)
            (visible-copy1 0)
        )
    (while (not (null? layerList))
        (set! layerId (car layerList))
        (gimp-drawable-set-visible layerId FALSE)
        (set! layerList (cdr layerList))
    )
    
    (gimp-image-insert-layer image smoke-layer 0 0)
    (gimp-drawable-set-name smoke-layer "Smoke")
    (gimp-image-insert-layer image bkg-layer 0 1)
    (gimp-item-set-visible layer FALSE)
    
    (gimp-context-push)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-background '(255 255 255))
    
    ;;;;scale image to given area if required
    (gimp-image-scale image 
        (max 1 (min 262144 (round (* width (sqrt (/ area (* width height)))))))
        (max 1 (min 262144 (round (* height (sqrt (/ area (* width height))))))))
    (set! width (car (gimp-image-width image)))
    (set! height (car (gimp-image-height image)))
    
    
    (if (= alpha FALSE) (gimp-layer-add-alpha smoke-layer))
    
    ;;;;check that a selection was made if not attempt to make one
    (if (= sel TRUE) (gimp-selection-layer-alpha smoke-layer))
    
    ;;;; create the breeze
    (if (> breeze 0)
        (begin
            (script-fu-distress-selection image 
                                smoke-layer 
                                127       ;Threshold (bigger 1<-->255 smaller)
                                15    ;Spread (8 0 1000 1 10 0 1)
                                breeze        ;Granularity (1 is low) (4 1 25 1 10 0 1)
                                2         ;Smooth (2 1 150 1 10 0 1)
                                TRUE      ;Smooth horizontally TRUE
                                TRUE)
        )
    )   ;Smooth vertically TRUE
    
    ;;;;create selection-channel (gimp-selection-load selection-channel)
    (set! selection-channel (car (gimp-selection-save image)))
    (gimp-edit-fill smoke-layer FILL-FOREGROUND)
    (gimp-image-set-active-layer image smoke-layer)
    
    
    ;;;;begin the script;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (gimp-context-set-background color)
    (gimp-drawable-fill bkg-layer FILL-FOREGROUND)
    (gimp-drawable-fill smoke-layer  FILL-TRANSPARENT)
    (gimp-edit-fill smoke-layer FILL-BACKGROUND)
    (gimp-selection-none image)
    
    (if (> warp 0)
        (begin
            (gimp-image-set-active-layer image smoke-layer)
            (plug-in-gauss-rle2 RUN-NONINTERACTIVE image smoke-layer warp warp)
            ;(gimp-curves-spline smoke-layer HISTOGRAM-ALPHA 8 #(0 0 158 19 174 242 255 255))
            (gimp-drawable-curves-spline smoke-layer HISTOGRAM-ALPHA 8 #(0.0 0.0 0.619 0.074 0.682 0.949 1.0 1.0))
        )
    )
    
    (plug-in-gauss-rle2 RUN-NONINTERACTIVE image smoke-layer 18 18)
    ;(gimp-curves-spline smoke-layer HISTOGRAM-ALPHA 8 #(0 0 117 56 197 237 255 255))
    (gimp-drawable-curves-spline smoke-layer HISTOGRAM-ALPHA 8 #(0.0 0.0 0.458 0.219 0.772 0.929 1.0 1.0))
    
    (set! copy-layer (car (gimp-layer-copy smoke-layer TRUE)))
    (gimp-image-insert-layer image copy-layer 0 -1)
    (gimp-drawable-set-name copy-layer "Motion blurred Layer")
    (gimp-layer-set-mode copy-layer LAYER-MODE-DIFFERENCE)
    (plug-in-mblur 1 image copy-layer 0 length angle (/ width 2) (/ height 2))
    (gimp-layer-set-offsets copy-layer 0 (/ length 2))
    (gimp-layer-resize-to-image-size copy-layer)
    
    (gimp-drawable-set-visible bkg-layer FALSE)
    (gimp-selection-none image)
    (set! visible (car (gimp-layer-new image (car (gimp-image-width image)) (car (gimp-image-height image)) RGBA-IMAGE "Visible" 100 LAYER-MODE-HSV-SATURATION)))
    (gimp-image-insert-layer image visible 0 -1)
    (gimp-edit-copy-visible image)
    (set! pasted (car (gimp-edit-paste visible FALSE)))
    (gimp-floating-sel-anchor pasted)
    (gimp-drawable-set-visible bkg-layer TRUE)
    
    (plug-in-ripple 1 image visible 72 9 ORIENTATION-VERTICAL 0 1 TRUE FALSE)
    (plug-in-ripple 1 image visible 72 4 ORIENTATION-HORIZONTAL 0 1 TRUE FALSE)
    
    (set! visible-copy (car (gimp-layer-copy visible TRUE)))
    (gimp-image-add-layer image visible-copy -1)
    (gimp-drawable-set-name visible-copy "visible-copy")
    (gimp-layer-set-mode visible-copy LAYER-MODE-SCREEN)
    (gimp-layer-set-opacity visible 30)
    
    (set! visible-copy1 (car (gimp-layer-copy visible TRUE)))
    (gimp-image-add-layer image visible-copy1 -1)
    (gimp-drawable-set-name visible-copy1 "visible-copy1")
    (gimp-layer-set-mode visible-copy1 LAYER-MODE-SCREEN-LEGACY)
    (gimp-drawable-set-visible layer TRUE)
    
    
    
    ;;;finish the script
    (gimp-image-scale image original-width original-height)
    
    (if (= conserve FALSE)
        (begin
            (set! smoke-layer (car (gimp-image-merge-down image smoke-layer EXPAND-AS-NECESSARY)))
            (set! smoke-layer (car (gimp-image-merge-down image copy-layer EXPAND-AS-NECESSARY)))
            (set! smoke-layer (car (gimp-image-merge-down image visible EXPAND-AS-NECESSARY)))
            (set! smoke-layer (car (gimp-image-merge-down image visible-copy EXPAND-AS-NECESSARY)))
            (set! smoke-layer (car (gimp-image-merge-down image visible-copy1 EXPAND-AS-NECESSARY)))
            (gimp-image-remove-channel image selection-channel)
            (if (or (= trans 0) (= trans 2))
                (plug-in-colortoalpha 1 image smoke-layer '(0 0 0))
            )
            (if (< trans 2) (set! layer (car (gimp-image-merge-down image smoke-layer EXPAND-AS-NECESSARY))))
            (if (= trans 2) (gimp-image-remove-layer image layer))
            (set! layer (car (gimp-image-get-active-layer image)))
            (gimp-drawable-set-name layer (string-append layer-name "-" "Smokin"))
        )
    )
    
    (set! layerList (vector->list (cadr (gimp-image-get-layers image))))
    (while (not (null? layerList))
        (set! layerId (car layerList))
        (gimp-drawable-set-visible layerId TRUE)
        (set! layerList (cdr layerList))
    )
    
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    (gimp-context-pop)
    
 )
)

(script-fu-register "script-fu-smokin" 
    "Smokin"
    "Can create smoke in the shape of your image. \nfile:Smokin_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "May 2013"
    "RGB*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-COLOR      "Smoke color"         '(171 171 255)
    SF-ADJUSTMENT "Warp" '(10 0 100 1 5 0 0)
    SF-ADJUSTMENT "Motion Length" '(50 1 256 1 5 0 0)
    SF-ADJUSTMENT "Motion Angle" '(90 0 360 1 5 0 0)
    SF-ADJUSTMENT "Breeze Distotion" '(0 0 25 1 5 0 0)
    SF-OPTION "Background Type" '("Original Image" "Black" "Transparent")
    SF-TOGGLE     "Keep the Layers"   FALSE
)

(script-fu-menu-register "script-fu-smokin" "<Image>/Script-Fu/Alpha-to-Logo")

; end of script
