; Impact rel 0.05
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
; Rel 0.02 - Added gradient option using GradMap
; Rel 0.03 - Added "Active Gradient" to Type selections an adj for "Effect Amount"
; Rel 0.04 - Improved scripts output and Added a selectable bkg-color
; Rel 0.05 - Bugfix with "Active Gradient"  
;
;
;

(define (script-fu-impact image layer
                                type
                                tint-color
                                gradient
                                blur
                                size
                                position
                                shadows
                                midtones
                                highlights
                                cyan-red 
                                magenta-green 
                                yellow-blue
                                bkg-color
                                trans
                                conserve
                            )
    (gimp-image-undo-group-start image)						  
    
 (let* (
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (layer-width width)
            (layer-height height)
            
            (area (* (* width height) 5))
            (alpha (car (gimp-drawable-has-alpha layer)))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (car (gimp-drawable-get-name layer)))
            (selection-channel 0)
            (border-layer 0)
            (bkg-layer 0)
            (text-selection 0)
            (border-selection 0)
            (tint-layer 0)
            (bkg-col-layer 0)
            (grad-layer 0)
            (color '(252 252 252)) ; "White"
            (offx 0)
            (offy 0)
        )
        
        
        (gimp-context-push)
        (gimp-context-set-default-colors)
        
        
        
        ;;;;scale image to given area if required
        (gimp-image-resize image ;	(gimp-image-scale image 
            (max 1 (min 262144 (round (* width (sqrt (/ area (* width height)))))))
            (max 1 (min 262144 (round (* height (sqrt (/ area (* width height))))))) 0 0)
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
        
        ;;;;centre the  layer	
        (set! offx (/ (- width layer-width) 2))
        (set! offy (/ (- height layer-height) 2))    
        (gimp-layer-set-offsets layer offx offy)	
        
        (if (= alpha FALSE) (gimp-layer-add-alpha layer))
        
        ;;;;check that a selection was made if not make one	
        (if (= sel TRUE) (gimp-selection-layer-alpha layer))
        
        (gimp-selection-invert image)
        (gimp-edit-clear layer)
        (gimp-selection-invert image)
        
        ;;;;create selection-channel (gimp-selection-load selection-channel)
        (set! selection-channel (car (gimp-selection-save image)))	
        (gimp-image-set-active-layer image layer)	
        
        ;;;;begin the script	
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 (+ (car (gimp-image-get-layer-position image layer)) 1)) ;under
        (gimp-context-set-foreground '(0 0 0))
        (gimp-drawable-fill bkg-layer FILL-FOREGROUND)
        (the-stroke image bkg-layer 0 color 100 0 size position '(127 255 255) '(127 255 127) 0 "Sunrise" 30 1.00 0 0 "Pine?")
        (set! bkg-layer (car  (gimp-image-get-active-layer image)))
        (gimp-selection-none image)
        (plug-in-gauss-rle2 RUN-NONINTERACTIVE image bkg-layer (- 22 blur) (- 22 blur))
        (plug-in-polar-coords 1 image bkg-layer 0 .5 FALSE TRUE FALSE)
        (gimp-image-rotate image 0)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (gimp-drawable-invert bkg-layer FALSE)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (gimp-drawable-invert bkg-layer FALSE)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (gimp-levels bkg-layer 0 8 130 1.00 0 255)
        (plug-in-wind 1 image bkg-layer 18 1 100 0 1)
        (gimp-image-rotate image 2)
        (plug-in-polar-coords 1 image bkg-layer 0 .5 FALSE TRUE TRUE)
        
        
        (if (= type 1)
            (begin
                
                (if (= shadows TRUE) (gimp-color-balance bkg-layer 0 TRUE cyan-red magenta-green yellow-blue))
                (if (= midtones TRUE) (gimp-color-balance bkg-layer 1 TRUE cyan-red magenta-green yellow-blue))
                (if (= highlights TRUE) (gimp-color-balance bkg-layer 2 TRUE cyan-red magenta-green yellow-blue))
            )
        )
        
        
        (gimp-fuzzy-select bkg-layer 1 1 15 2 TRUE FALSE 0 TRUE)
        (gimp-edit-clear bkg-layer)
        (gimp-selection-none image)
        (plug-in-autocrop 1 image bkg-layer)
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
        
        (if (= type 2)
            (begin
                (gimp-context-set-foreground tint-color)
                (set! tint-layer (car (gimp-layer-new image width height RGBA-IMAGE "Tint-Color" 100 LAYER-MODE-OVERLAY)))
                (gimp-image-insert-layer image tint-layer 0 (car (gimp-image-get-layer-position image bkg-layer))) ;over
                (gimp-drawable-fill tint-layer FILL-FOREGROUND)
                (if (= conserve FALSE) (set! bkg-layer (car (gimp-image-merge-down image tint-layer EXPAND-AS-NECESSARY))))
            )
        )
        
        (if (> type 2)
            (begin
                (set! grad-layer (car (gimp-layer-copy bkg-layer TRUE)))
                (gimp-image-insert-layer image grad-layer 0 (car (gimp-image-get-layer-position image bkg-layer))) ;over
                (gimp-layer-set-mode grad-layer LAYER-MODE-GRAIN-MERGE)
                (gimp-context-set-foreground old-fg)
                (gimp-context-set-background old-bg)
                (if (= type 3) (gimp-context-set-gradient gradient))
                (if (= type 4) (gimp-context-set-gradient (car(gimp-context-get-gradient))))
                (plug-in-gradmap 1 image grad-layer)
                (set! bkg-layer (car (gimp-image-merge-down image grad-layer EXPAND-AS-NECESSARY)))
            )
        )
        
        (if (= trans FALSE)
            (begin
                (set! bkg-col-layer (car (gimp-layer-new image width height RGBA-IMAGE "Bkg-Color" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image bkg-col-layer 0 (+ (car (gimp-image-get-layer-position image bkg-layer)) 1)) ;under
                (gimp-context-set-foreground bkg-color)
                (gimp-drawable-fill bkg-col-layer FILL-FOREGROUND)
                (set! bkg-layer (car (gimp-image-merge-down image bkg-layer EXPAND-AS-NECESSARY)))
                (gimp-drawable-set-name bkg-layer "Background")
            )
        )
        
        ;;;;finish the script	
        (if (= conserve FALSE) (set! layer (car (gimp-image-merge-down image layer EXPAND-AS-NECESSARY))))
        (gimp-drawable-set-name layer layer-name)
        (gimp-image-remove-channel image selection-channel)
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        
 )
)

(script-fu-register "script-fu-impact"              
  "Impact..."
  "Creates 'Blast' like lines around a layer with alpha or selection. Requires a layer with transparency. \nfile:Impact.scm"
  "Graechan"
  "Graechan - http://gimpchat.com"
  "April 2013"
  "RGB*"
  SF-IMAGE      "image"             0
  SF-DRAWABLE   "drawable"          0
  SF-OPTION     "Type"              '("White" "Impact" "Color" "Blend Gradient" "Active Gradient")
  SF-COLOR      "Tint Color"        '(0 0 255)
  SF-GRADIENT   "Blend Gradient"    "Incandescent"
  SF-ADJUSTMENT	"Effect Amount"     '(10 0 20 1 5 0 0)
  SF-ADJUSTMENT	"Size"              '(6 1 999 1 10 0 1)
  SF-ADJUSTMENT	"Position (0 = inside, 100 = outside)"      '(100 0 100 1 10 1 0)
  SF-TOGGLE     "SHADOWS"           TRUE
  SF-TOGGLE     "MIDTONES"          TRUE
  SF-TOGGLE     "HIGHLIGHTS "       TRUE
  SF-ADJUSTMENT "Impact (cyan-red)"                         '(0 -100 100 1 10 0 0)
  SF-ADJUSTMENT "Impact (magenta-green)"                    '(0 -100 100 1 10 0 0)
  SF-ADJUSTMENT "Impact (yellow-blue)"                      '(0 -100 100 1 10 0 0)
  SF-COLOR      "Background color"          '(0 0 0)
  SF-TOGGLE     "Transparent Background"    FALSE
  SF-TOGGLE     "Keep the Layers"           FALSE
)

(script-fu-menu-register "script-fu-impact" "<Image>/Script-Fu/Alpha-to-Logo")

(define (script-fu-impact-logo
                                      text
                                      letter-spacing
                                      line-spacing
                                      grow
                                      color 
                                      font-in 
                                      font-size
                                      type ;("White" "Impact" "Color" "Gradient")
                                      tint-color
                                      gradient
                                      blur
                                      size
                                      position
                                      cyan-red 
                                      magenta-green 
                                      yellow-blue
                                      bkg-color
                                      trans
                                      conserve)
                                        
  (let* (
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
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
        )
         
        (gimp-context-push)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        ;;;;Add the text layer for a temporary larger Image size
        (set! text-layer (car (gimp-text-fontname image -1 0 0 text (round (/ 150 4)) TRUE 150 PIXELS font)))
        ;;;;adjust text on line
        (gimp-text-layer-set-justification text-layer 2)
        (gimp-text-layer-set-letter-spacing text-layer letter-spacing)
        (gimp-text-layer-set-line-spacing text-layer line-spacing)
        (set! width (car (gimp-drawable-width text-layer)))
        (set! height (car (gimp-drawable-height text-layer)))    
        (gimp-image-remove-layer image size-layer)
        (gimp-image-resize-to-layers image)
        
        ;;;;set the text clolor    
        (gimp-context-set-foreground color)
        (gimp-selection-layer-alpha text-layer)
        (gimp-edit-fill text-layer FILL-FOREGROUND)
        
        (set! bkg-layer (car (gimp-layer-new image width height RGBA-IMAGE "white border" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bkg-layer 0 (+ (car (gimp-image-get-layer-position image text-layer)) 1)) ;under
        (gimp-selection-grow image grow)
        ;(gimp-context-set-foreground "White")
        (gimp-context-set-foreground '(255 255 254)) ; "White"
        (gimp-edit-fill bkg-layer FILL-FOREGROUND)
        (gimp-selection-none image)
        (set! text-layer (car (gimp-image-merge-down image text-layer EXPAND-AS-NECESSARY)))
        (gimp-drawable-set-name text-layer text)
        
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-background old-bg)
        
        ;(gimp-message "Impact line289")
        ;(gimp-display-new image)
        ;(gimp-displays-flush)
        ;(quit)
        
        (script-fu-impact image text-layer
                               type ;("White" "Impact" "Color" "Blend Gradient" "Active Gradient")
                               tint-color
                               gradient
                               blur
                               size
                               position
                               TRUE ;SHADOWS
                               TRUE ;MIDTONES
                               TRUE ;HIGHLIGHTS
                               cyan-red 
                               magenta-green 
                               yellow-blue
                               bkg-color
                               trans
                               conserve)
        
        (set! text-layer (car  (gimp-image-get-active-layer image)))
        
        ;;;;Scale Image to it's original size;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (gimp-image-scale-full image final-width final-height 2)
        (set! width (car (gimp-image-width image)))
        (set! height (car (gimp-image-height image)))
        
        (gimp-context-pop)
        (gimp-display-new image)
        
    )
  )
  
(script-fu-register "script-fu-impact-logo"
    "Impact Logo..."
    "Creates a logo with blast lines. \nfile:Impact.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "April 2013"
    ""
    SF-TEXT         "Text"                  "IMPACT"
    SF-ADJUSTMENT   "Letter Spacing"        '(0 -100 100 1 5 0 0)
    SF-ADJUSTMENT   "Line Spacing"          '(0 -100 100 1 5 0 0)
    SF-ADJUSTMENT   "White Text-Border Size"        '(1 1 10 1 1 0 0)
    SF-COLOR        "Text color"            '(0 0 0)
    SF-FONT         "Font"                  "Arial Bold"
    SF-ADJUSTMENT   "Font size (pixels)"    '(150 6 500 1 1 0 1)
    SF-OPTION       "Type"                  '("White" "Impact" "Color" "Blend Gradient" "Active Gradient")
    SF-COLOR        "Tint Color"            '(0 0 255)
    SF-GRADIENT     "Blend Gradient" "Incandescent"
    SF-ADJUSTMENT   "Effect Amount"             '(10 0 20 1 5 0 0)
    SF-ADJUSTMENT   "Size"                      '(6 1 999 1 10 0 1)
    SF-ADJUSTMENT   "Position (0 = inside, 100 = outside)"      '(100 0 100 1 10 1 0)
    SF-ADJUSTMENT   "Impact (cyan-red)"         '(0 -100 100 1 10 0 0)
    SF-ADJUSTMENT   "Impact (magenta-green)"    '(0 -100 100 1 10 0 0)
    SF-ADJUSTMENT   "Impact (yellow-blue)"      '(0 -100 100 1 10 0 0)
    SF-COLOR        "Background color"          '(0 0 0)
    SF-TOGGLE       "Transparent Background"    FALSE
    SF-TOGGLE       "Keep the Layers"           FALSE
)

(script-fu-menu-register "script-fu-impact-logo" "<Image>/Script-Fu/Logos")

(define (the-stroke image layer
                  type ; Color(0) Gradient(1) Pattern(2)
                  color
                  opacity
                  mode
                  size
                  position
                  fg-color ;
                  bg-color ;
                  blend-mode ;
                  gradient ;
                  angle ;
                  scale ;
                  style ;
                  repeat ;
                  pattern ;
        )
    
    ;(gimp-image-undo-group-start image)    
    
 (let* (            
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (original-width width)
            (original-height height)
            (area (* 1000 1000))
            (alpha (car (gimp-drawable-has-alpha layer)))
            (sel (car (gimp-selection-is-empty image)))
            (typeA (car (gimp-drawable-type-with-alpha layer)))
            (layer-name (car (gimp-drawable-get-name layer)))
            (inc (if (> position 0) (* (/ size 100) position) 0))
            (stroke-layer (car (gimp-layer-new image (+ width (* inc 2)) (+ height (* inc 2)) typeA "Stroke Layer" 100 LAYER-MODE-NORMAL)))
            (alpha-selection 0)
            (inner-selection 0)
            (outer-selection 0)
            (mask-selection 0)
            (stroke-mask 0)
            (result-layer 0)
            (visible 0)
            (selection-bounds 0)
            (radians (/ (* (* 2 *pi*) angle) 360))
            (x-distance (* (* (* 0.5 scale) width) (sin radians)))
            (y-distance (* (* (* 0.5 scale) height) (cos radians)))
            (x-center (/ width 2))
            (y-center (/ height 2))
            (x1 (- x-center x-distance))
            (y1 (- y-center y-distance))
            (x2 (+ x-center x-distance))
            (y2 (+ y-center y-distance))
        )
        
        (gimp-image-insert-layer image stroke-layer 0 -1)
        (if (> position 0) (gimp-layer-set-offsets stroke-layer (- 0 inc) (- 0 inc)))
        
        (gimp-context-push)
        (gimp-context-set-default-colors)
        
        (if (= alpha FALSE) (gimp-layer-add-alpha layer))
        
        ;;;;check that a selection was made if not make one	
        (if (= sel TRUE) (gimp-selection-layer-alpha layer))
        
        ;;;;create selection-channel (gimp-selection-load alpha-selection)    
        (set! alpha-selection (car (gimp-selection-save image)))	
        
        ;;;;begin the script
        (if (> position 0)
            (begin
                (gimp-image-resize-to-layers image)
                (gimp-selection-grow image inc)
                (set! outer-selection (car (gimp-selection-save image)))
                (gimp-edit-fill stroke-layer FILL-BACKGROUND)
                (gimp-image-set-active-layer image stroke-layer)
                (plug-in-autocrop-layer 1 image stroke-layer)
                (gimp-drawable-fill stroke-layer FILL-TRANSPARENT)
                (gimp-image-resize-to-layers image)
                (gimp-selection-load alpha-selection)
                (gimp-selection-shrink image (- size inc))
                (set! inner-selection (car (gimp-selection-save image)))
            )
        )
        
        (if (= position 0)
            (begin
                (gimp-selection-load alpha-selection)
                (set! outer-selection (car (gimp-selection-save image)))
                (gimp-selection-shrink image size)
                (set! inner-selection (car (gimp-selection-save image)))
            )
        )
        
        ;;;;create the selection for mask	
        (gimp-selection-load outer-selection)
        (gimp-selection-combine inner-selection 1)
        (set! mask-selection (car (gimp-selection-save image)))
    ;   (gimp-selection-none image)
        
        (gimp-context-set-pattern pattern)
        (gimp-context-set-foreground fg-color)
        (gimp-context-set-background bg-color)
        (if (= type 0) (gimp-context-set-background color))
        (gimp-context-set-gradient gradient)
        
        (gimp-image-set-active-layer image stroke-layer)
        
        (if (= type 0) (gimp-edit-fill stroke-layer FILL-BACKGROUND))
        (if (= type 1) (gimp-edit-blend stroke-layer blend-mode LAYER-MODE-NORMAL style 100 0 repeat FALSE FALSE 0 0 0 x1 y1 x2 y2))
        (if (= type 2) (gimp-edit-fill stroke-layer FILL-PATTERN))
        (gimp-selection-none image)
        (set! layer (car (gimp-image-merge-down image stroke-layer EXPAND-AS-NECESSARY)))
        
        
        ;(gimp-displays-flush)
        ;(gimp-image-undo-group-end image)
        (gimp-context-pop)
        
 )
)

;end of script