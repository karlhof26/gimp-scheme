; Two-Strip Technicolor effect
; From www.gimpforphotos.com
; Adapted to script-fu by Patrick David <patdavid@gmail.com>
;
;  
;
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Define the function
;
;

(define (script-fu-two-strip-technicolor Image Drawable MergeDown Normalize Option Swap)
    
    
    (let* (      ; define variables
            (RedColor '(255 0 0))
            (CyanColor '(0 255 255))
            (YellowColor '(255 255 0))
            (GreenColor '(0 0 255)) ; I know it is blue
            (RedLayer (car (gimp-layer-copy Drawable TRUE)) )
            (CyanLayer (car (gimp-layer-copy Drawable TRUE)) )
            (YellowLayer (car (gimp-layer-copy Drawable TRUE)) )
            (GreenLayer (car (gimp-layer-copy Drawable TRUE)) )
            
            (RedFill (car (gimp-layer-copy Drawable TRUE)) )
            (CyanFill (car (gimp-layer-copy Drawable TRUE)) )
            (YellowFill (car (gimp-layer-copy Drawable TRUE)) )
            (GreenFill (car (gimp-layer-copy Drawable TRUE)) )
            (randamt 1)
            (Final)
        )
            (gimp-image-undo-group-start Image)
            
            (gimp-image-insert-layer Image CyanLayer 0 -1)
            (gimp-context-set-foreground CyanColor)
            (gimp-drawable-fill CyanFill FILL-FOREGROUND)
            (gimp-layer-set-mode CyanFill LAYER-MODE-MULTIPLY)
            (gimp-image-insert-layer Image CyanFill 0 -1)
            (set! CyanLayer (car (gimp-image-merge-down Image CyanFill 0)) )
            (gimp-drawable-desaturate CyanLayer DESATURATE-LIGHTNESS)
            (gimp-drawable-colorize-hsl CyanLayer 180 50 0)
            
            (if (= Option 0)
                (begin
            (gimp-image-insert-layer Image RedLayer 0 -1)
            (gimp-context-set-foreground RedColor)
            
            (gimp-image-insert-layer Image RedFill 0 -1)
            (gimp-drawable-fill RedFill FILL-FOREGROUND)
            (gimp-layer-set-mode RedFill LAYER-MODE-MULTIPLY)
            ;(gimp-image-insert-layer Image RedFill 0 -1)
            (set! RedLayer (car (gimp-image-merge-down Image RedFill 0)) )
            (gimp-layer-set-mode RedLayer LAYER-MODE-DIFFERENCE)
                )
            )
            
            (if (= Option 1)
                (begin
                    (gimp-image-insert-layer Image YellowLayer 0 -1)
                    (gimp-context-set-foreground YellowColor)
                    
                    (gimp-image-insert-layer Image YellowFill 0 -1)
                    (gimp-drawable-fill YellowFill FILL-FOREGROUND)
                    (gimp-layer-set-mode YellowFill LAYER-MODE-MULTIPLY)
                    ;(gimp-image-insert-layer Image YellowFill 0 -1)
                    (set! YellowLayer (car (gimp-image-merge-down Image YellowFill 0)) )
                    (gimp-layer-set-mode YellowLayer LAYER-MODE-DIFFERENCE)
                )
            )
            
            (if (= Option 2)
                (begin
                    (gimp-image-insert-layer Image GreenLayer 0 -1)
                    (gimp-context-set-foreground GreenColor)
                    
                    (gimp-image-insert-layer Image GreenFill 0 -1)
                    (gimp-drawable-fill GreenFill FILL-FOREGROUND)
                    (gimp-layer-set-mode GreenFill LAYER-MODE-MULTIPLY)
                    ;(gimp-image-insert-layer Image YellowFill 0 -1)
                    (set! GreenLayer (car (gimp-image-merge-down Image GreenFill 0)) )
                    (gimp-layer-set-mode GreenLayer LAYER-MODE-DIFFERENCE)
                    
                    (gimp-drawable-hue-saturation GreenLayer HUE-RANGE-ALL (rand 170) 0 0 0)
                    (gimp-displays-flush)
                )
            )
            
            (if (= Option 3)
                (begin
                    (gimp-image-insert-layer Image GreenLayer 0 -1)
                    (gimp-context-set-foreground RedColor) ; Deliberate Red not Green
                    
                    (gimp-image-insert-layer Image GreenFill 0 -1)
                    (gimp-drawable-fill GreenFill FILL-FOREGROUND)
                    (set! randamt (rand 170))
                    (gimp-drawable-hue-saturation GreenLayer HUE-RANGE-ALL randamt 0 0 0)
                    (gimp-layer-set-mode GreenFill LAYER-MODE-MULTIPLY)
                    ;(gimp-image-insert-layer Image YellowFill 0 -1)
                    (set! GreenLayer (car (gimp-image-merge-down Image GreenFill 0)) )
                    (gimp-layer-set-mode GreenLayer LAYER-MODE-DIFFERENCE)
                    
                    (gimp-drawable-hue-saturation CyanLayer HUE-RANGE-ALL randamt 0 0 0)
                    (gimp-displays-flush)
                )
            )
            (if (= Swap TRUE)
                (begin
                    (gimp-image-raise-layer Image CyanLayer)
                    (gimp-layer-set-mode CyanLayer LAYER-MODE-DIFFERENCE)
                    (cond ((= Option 0)
                            (gimp-layer-set-mode RedLayer LAYER-MODE-NORMAL)
                          )
                          ((= Option 1)
                            (gimp-layer-set-mode YellowLayer LAYER-MODE-NORMAL)
                          )
                          ((= Option 2)
                            (gimp-layer-set-mode GreenLayer LAYER-MODE-NORMAL)
                          )
                          ((= Option 3)
                            (gimp-layer-set-mode GreenLayer LAYER-MODE-NORMAL)
                          )
                    )
                )
            )
            
            (if (= MergeDown TRUE)
                (begin 
                    ;;(gimp-drawable-set-visible Drawable FALSE)
                    (set! Final (car (gimp-image-merge-visible-layers Image 0)) )
                    (gimp-drawable-set-name Final "Two-strip Technicolor")
                    (if (= Normalize TRUE)
                        (plug-in-normalize 1 Image Final)
                        
                        ;(gimp-drawable-hue-saturation Final HUE-RANGE-ALL (rand 170) 0 0 0)
                    )
                    ;;(gimp-drawable-set-visible Drawable TRUE)
                )
            )
            
            
            (gimp-displays-flush)
            (gimp-image-undo-group-end Image)
    )
)

; Finally register our script with script-fu.
(script-fu-register "script-fu-two-strip-technicolor"
                    "Two-color Combo Colorise..."
                    "Apply two color technicolor effect from gimpforphotos.com. Quite a muted effect. \nfile:two-strip-technicolor.scm"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2011-06-11"
                    "RGB*"
            SF-IMAGE        "Image"         0
            SF-DRAWABLE     "Drawable"      0
            ;SF-OPTION "Mute Method" '("Luminosity Layers" "Solid Layers with Masks" "Desaturated Overlay" )
            SF-TOGGLE       "Merge Down"    FALSE
            SF-TOGGLE       "Normalize when done (only applicable when merged down)"    FALSE
            SF-OPTION       "Color combo"   '("Red-Green" "Blue-Yellow" "Red-Green" "Red-Random")
            SF-TOGGLE       "Swap"          FALSE
)

(script-fu-menu-register "script-fu-two-strip-technicolor" "<Toolbox>/Script-Fu/Colors")
