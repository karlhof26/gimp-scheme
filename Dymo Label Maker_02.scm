; Dymo Label Maker rel 0.01  
; Created by Graechan
; Comments directed to http://gimpchat.com or http://gimpscripts.com
; many thanks to hearty for the great tutorial that made this script possible
; Tutorial at http://www.gimpchat.com/viewtopic.php?f=23&t=6736 
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
(define (script-fu-dymo-label-maker 
                                      text 
                                      font-in 
                                      font-size
                                      opacity
                                      type
                                      color
                                      pattern
                                      gradient
                                      conserve)
                                        
  (let* (
            (image (car (gimp-image-new 256 256 RGB)))         
            (border (/ font-size 4))
            (font (if (> (string-length font-in) 0) font-in (car (gimp-context-get-font))))
            (size-layer (car (gimp-text-fontname image -1 0 0 text border TRUE font-size PIXELS font)))
            (final-width (car (gimp-drawable-width size-layer)))
            (final-height (car (gimp-drawable-height size-layer)))
            (text-layer 0)
            (width 0)
            (height 0)
            (bkg-layer 0)
            (selection 0)
            (text-selection 0)
            (color-layer 0)
            (cloud-layer 0)
            (grain-layer 0)
            (shadow-layer 0)         
        )
        
        ;(gimp-message "start OK")
        (gimp-context-push)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        ;;;;Add the text layer for a temporary larger Image size
        (set! text-layer (car (gimp-text-fontname image -1 0 0 text (round (/ 120 4)) TRUE 120 PIXELS font)))
        (set! width (car (gimp-drawable-width text-layer)))
        (set! height (car (gimp-drawable-height text-layer)))    
        (gimp-image-remove-layer image size-layer)
        (gimp-image-resize-to-layers image)
        
        ;;;;centre text on line
        (gimp-text-layer-set-justification text-layer 2)
        
        (gimp-image-select-rectangle image
                        CHANNEL-OP-REPLACE
                        (car (gimp-drawable-offsets text-layer))
                        (cadr (gimp-drawable-offsets text-layer))
                        width
                        height
                         ;operation{ CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) }
                        )  ;featherFalse radius10
        
        ;;;;create selection channel (gimp-selection-load selection)    
        (set! selection (car (gimp-selection-save image)))	
        (gimp-item-set-name selection "selection")
        ;    (gimp-image-set-active-layer image text-layer)	
        
        ;;;;set the text clolor    
        (gimp-context-set-foreground '(128 128 128))
        (gimp-selection-layer-alpha text-layer)
        (set! text-selection (car (gimp-selection-save image))) ;(gimp-selection-load text-selection)
        (gimp-edit-fill text-layer FILL-FOREGROUND)
        (gimp-selection-none image)
        
        ;;;;start of script;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (set! color-layer (car (gimp-layer-new image width height RGBA-IMAGE "Color" 100 LAYER-MODE-NORMAL-LEGACY)))
        (gimp-image-add-layer image color-layer (+ (car (gimp-image-get-layer-position image text-layer)) 1)) ;under
        (gimp-context-set-background color)
        (gimp-context-set-pattern pattern)
        (gimp-context-set-gradient gradient)
        (if (= type 1) (gimp-drawable-fill color-layer FILL-PATTERN))
        (if (= type 0) (gimp-drawable-fill color-layer FILL-BACKGROUND))
        (if (= type 2) 
            (begin
                (gimp-selection-none image)
                (gimp-drawable-fill color-layer FILL-BACKGROUND)
                (gimp-edit-blend color-layer BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 0 (/ height 2) width (/ height 2))
            )
        )
        
        
        (set! cloud-layer (car (gimp-layer-new image width height RGBA-IMAGE "Cloud" opacity LAYER-MODE-SCREEN)))
        (gimp-image-add-layer image cloud-layer (+ (car (gimp-image-get-layer-position image text-layer)) 1)) ;under
        (plug-in-solid-noise 1 image cloud-layer FALSE FALSE 1176670063 1 2 2)
        
        (set! grain-layer (car (gimp-layer-new image width height RGBA-IMAGE "Grain" 100 LAYER-MODE-OVERLAY)))
        (gimp-image-add-layer image grain-layer (+ (car (gimp-image-get-layer-position image text-layer)) 1)) ;under
        (gimp-context-set-background '(128 128 128))
        (gimp-drawable-fill grain-layer FILL-BACKGROUND)
        (plug-in-rgb-noise 1 image grain-layer FALSE FALSE 0.2 0.2 0.2 0)
        
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image text-layer 20 20)
        (plug-in-emboss 1 image text-layer 90 45 20 TRUE)
        (plug-in-unsharp-mask 1 image text-layer 5.0 0.5 0)
        (gimp-drawable-brightness-contrast text-layer 0.15 0.15)  ;was 45 45
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image text-layer 5 5)
        
        (script-fu-drop-shadow image color-layer 1 1 15 '(0 0 0) 60 TRUE)
        (set! shadow-layer (car (gimp-layer-new image width height RGBA-IMAGE "shadow" 100 LAYER-MODE-NORMAL)))
        (gimp-image-add-layer image shadow-layer (+ (car (gimp-image-get-layer-position image color-layer)) 1)) ;under
        (set! shadow-layer (car (gimp-image-merge-down image shadow-layer EXPAND-AS-NECESSARY)))
        (gimp-image-set-active-layer image shadow-layer)
        (plug-in-autocrop 1 image shadow-layer)
        
        (gimp-selection-load text-selection)
        (gimp-selection-invert image)
        (gimp-selection-feather image 20)
        (gimp-edit-clear grain-layer)
        (gimp-edit-clear cloud-layer)
        (gimp-selection-none image)	
        
        ;;;;end of script;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
        
        ;;;;Scale Image to it's original size;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (gimp-image-scale-full image final-width final-height 2)
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
                
        ;;;;resize the text-layer
        (gimp-image-set-active-layer image text-layer)
        (gimp-layer-resize-to-image-size text-layer)
        
        (if (= conserve FALSE)
            (begin
                (set! color-layer (car (gimp-image-merge-down image color-layer EXPAND-AS-NECESSARY)))
                (set! color-layer (car (gimp-image-merge-down image cloud-layer EXPAND-AS-NECESSARY)))
                (set! color-layer (car (gimp-image-merge-down image grain-layer EXPAND-AS-NECESSARY)))
                (set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
            )
        )
        (gimp-drawable-set-name text-layer text)
        
        (gimp-context-pop)
        (gimp-displays-flush)
        (gimp-display-new image) 
  )
) 

  
(script-fu-register "script-fu-dymo-label-maker"
    "Dymo Label Maker"
    "Create an image with a text layer over a pattern layer. Makes an image like the old Dymo tape used as labels. \n file:Dymo Lable Maker_02.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "Mar 2013"
    ""
    SF-STRING       "Label Text"    "Gimp"
    SF-FONT         "Font"          "Arial Bold"
    SF-ADJUSTMENT   "Font size (pixels)" '(120 6 500 1 1 0 1)
    SF-ADJUSTMENT   "Label Ager"        '(30 0 100 1 10 0 0)
    SF-OPTION       "Label Bkg Type"    '("Color" "Pattern" "Gradient")
    SF-COLOR        "Label Color"       '(255 0 0)
    SF-PATTERN      "Label Pattern"     "Pine?"
    SF-GRADIENT     "Label Gradient"    "Abstract 3"
    SF-TOGGLE       "Keep the Layers"   FALSE  
)

(script-fu-menu-register "script-fu-dymo-label-maker" "<Toolbox>/Script-Fu/Logos")

;end of script