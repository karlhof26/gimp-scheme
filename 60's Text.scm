; 60's text effect Rev. 2.2  
; creates text with psychedelic color patterns
;
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; 3d-outline creates outlined border of a text with patterns
; Copyright (C) 1998 Hrvoje Horvat
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

;===============================================================
;== 60's Text Script based on my tutorial of the same name    ==
;== Revision 2.0 Made compatable with Gimp 2.4                ==
;== Revision 2.1 Gave gradient selections                     ==
;== Revision 2.2 Gave gradient types and reversals            ==
;== Script by Gregory M. Ross                                 ==
;===============================================================

(define (script-fu-60s-text thestring
                            font-name
                            font-size
                            bg-color
                            gradient1
                            reverse1
                            style1
                            gradientdirectionoption
                            gradient2
                            reverse2
                            style2
                            mapgradient
                            mapgradientreverse
                            secondgradientaction
                            mapgradientaction
                            dropshadowaction)

  (let* (
            (text-ext (gimp-text-get-extents-fontname thestring font-size 0 font-name)) ; Get extents of the bounding box for the specified text in pixels.
            (wide (+ (car text-ext) 20))                                               ; wide = text width + 20 pixels
            (high (+ (cadr text-ext) 20))                                              ; high = text height + 20 pixels
            (img (car (gimp-image-new wide high 0)))                                   ; define new rgb image called "img"
            (bg-layer (car (gimp-layer-new img wide high 0 "Background" 100 0)))       ; define new rgb layer named "Background" 100% Opacity Normal mode
            (gradient-layer (car (gimp-layer-new img wide high 1 "Gradient" 100 10)))  ; define new rgba layer named "Gradient"
            (gradient2-layer (car (gimp-layer-new img wide high 1 "Gradient2" 100 LAYER-MODE-DIFFERENCE)))  ; define new rgba layer named "Gradient2" ; was difference
            (dupe-layer (car (gimp-layer-new img wide high 1 "Gradient Map" 100 6)))   ; define new rgba layer named "Gradient Map"
            (text-layer (car (gimp-layer-new img wide high 1 "Text" 100 0)))           ; define new rgba layer named "Text"
            (shadow-layer (car (gimp-layer-new img wide high 1 "Shadow" 80 0)))        ; define new rgba layer named "Shadow" with 80% opacity
            
            (widthfactor1 0.5)
            (heightfactor1 0.8)
            (widthfactor2 0.1)
            (heightfactor2 0.5)
        )
        
        
        
        (gimp-image-undo-group-start img)                                                     ; disable undo since there doesn't need to be one
        
        ; Set colors and create layers
        
        (gimp-context-set-default-colors)
        (gimp-context-set-background bg-color)
        (gimp-image-insert-layer img bg-layer 0 1)
        (gimp-image-insert-layer img shadow-layer 0 -1)
        (gimp-image-insert-layer img text-layer 0 -1)
        (gimp-image-insert-layer img gradient-layer 0 -1)
        (gimp-image-insert-layer img gradient2-layer 0 -1)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-clear bg-layer)
        (gimp-edit-clear shadow-layer)
        (gimp-edit-clear text-layer)    
        (gimp-edit-clear gradient-layer)
        (gimp-floating-sel-anchor (car (gimp-text-fontname img text-layer 10 10 thestring 0 TRUE font-size PIXELS font-name)))
        
        ; Create gradient effect 
        
        (gimp-context-set-gradient gradient1)
        
        ;(gimp-message (number->string style1))
        ;(gimp-message (number->string wide))
        ;(gimp-message (number->string high))
        ;(gimp-edit-blend gradient-layer 3 0 style1 100 0 0 reverse1 FALSE 0 0 TRUE 0 0 wide high)
        ;(gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE 0 0 wide high)
        
        ;(gimp-message "checking direction option")
        (cond ((= gradientdirectionoption 0)
                ;(gimp-message "GradDir Option0")
                (set! widthfactor1 0.0)
                (set! heightfactor1 0.5)
                (set! widthfactor2 1.0)
                (set! heightfactor2 0.5)
              )
              ((= gradientdirectionoption 1)
                ;(gimp-message "GradDir Option1 vertical")
                (set! widthfactor1 0.5)
                (set! heightfactor1 0.0)
                (set! widthfactor2 0.5)
                (set! heightfactor2 1.0)
              )
              ((= gradientdirectionoption 2)
                ;(gimp-message "GradDir Option2 halfway down")
                (set! widthfactor1 0.0)
                (set! heightfactor1 0.5)
                (set! widthfactor2 0.0)
                (set! heightfactor2 1.0)
              )
              ((= gradientdirectionoption 3)
                ;(gimp-message "GradDir Option3 High slant")
                (set! widthfactor1 0.10)
                (set! heightfactor1 0.20)
                (set! widthfactor2 0.12)
                (set! heightfactor2 0.40)
              )
              ((= gradientdirectionoption 4)
                ;(gimp-message "GradDir Option4 Good for radial")
                (set! widthfactor1 0.0)
                (set! heightfactor1 0.5)
                (set! widthfactor2 0.25)
                (set! heightfactor2 0.65)
              )
              ((= gradientdirectionoption 5)
                ;(gimp-message "GradDir Option5 TopL BottomR")
                (set! widthfactor1 0.0)
                (set! heightfactor1 0.0)
                (set! widthfactor2 1.0)
                (set! heightfactor2 0.99)
              )
              ((= gradientdirectionoption 6)
                ;(gimp-message "option 6")
                (set! widthfactor1 (/ (rand 99) 100))
                (set! heightfactor1 (/ (rand 99) 100))
                (set! widthfactor2 (/ (rand 99) 100))
                (set! heightfactor2 (/ (rand 99) 100))
                ;(gimp-message (number->string widthfactor1))
              )
              ((= gradientdirectionoption 7) ; narrow line
                ;(gimp-message "option 7")
                (set! widthfactor1 0.46)
                (set! heightfactor1 0.5)
                (set! widthfactor2 0.54)
                (set! heightfactor2 0.55)
              )
        )
        
        
        (cond 
              ((= style1 0)
                ;(gimp-message "style=0")
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 1 reverse1 FALSE 0 0 TRUE (* wide widthfactor1) (* high heightfactor1) (* wide widthfactor2) (* high heightfactor2))
              )
              ((= style1 1)
                ;(gimp-message "style=1") ; bi-linear - have added a repeat and angled - will consider a set of options (widths and heights) for each
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 1 reverse1 FALSE 0 0 TRUE 0 (* high 0.5) (* wide 0.25) (* high 0.65))
              )
              ((= style1 2)
                ;(gimp-message "style=2") ; radial
                ;(gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE 0 0 wide high)
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 1 reverse1 FALSE 0 0 TRUE (* wide widthfactor1) (* high heightfactor1) (* wide widthfactor2) (* high heightfactor2))
              )
              ((= style1 3)
                ;(gimp-message "style=3") ; square
                ;(gimp-message "Squaredebug")
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 REPEAT-SAWTOOTH reverse1 FALSE 0 0 TRUE (* wide 0.5) (* high 0.5) (* wide 0.53) (* high 0.2))
              )
              ((= style1 4)
                ;(gimp-message "style=4") ; conical sym
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE (* wide 0.50) (* high 0.45) (* wide 0.81) (* high 0.65))
              )
              ((= style1 5)
                ;(gimp-message "style=5") ; conical (asymm)
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE (* wide 0.5) (* high 0.5) (* wide 0.75) (* high 0.5))
              )
              ((= style1 6)
                ;(gimp-message "style=6") ; shaped (angular)
                ;(gimp-message "Squaredebug")
                ;(gimp-layer-set-mode gradient-layer LAYER-MODE-NORMAL)
                
                ; NB shaped gradients require a selection
                (gimp-selection-all img)
                ;(gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE (* wide 0.5) (* high 0.5) (* wide 0.8) (* high 0.5))
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY style1 100 0 0 TRUE TRUE 1 1 TRUE (* wide 0.1) (* high 0.1) (* wide 0.85) (* high 0.5))
              )
              ((= style1 7)
                ;(gimp-message "style=7") ; shaped (?)
                (gimp-selection-all img)
                (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE (* wide 0.5) (* high 0.5) (* wide 0.75) (* high 0.5))
              )
              (else
                (begin
                    ;(gimp-message "style=else")
                    (gimp-selection-all img)
                    (gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-NORMAL style1 100 0 0 reverse1 FALSE 0 0 TRUE (* wide 0.25) 0 (* wide 0.5) (* high 0.5))
                )
              )
        )
        
        ;(gimp-message "line142")
        ;(gimp-display-new img)
        ;(quit) 
        
        (if (= secondgradientaction TRUE)
            (begin
                ; added to address various styles requiring a selection
                (gimp-selection-all img)
                
                (gimp-context-set-gradient gradient2)
                ;(gimp-edit-blend gradient-layer BLEND-CUSTOM LAYER-MODE-DIFFERENCE style2 100 0 0 reverse2 FALSE 0 0 TRUE 0 (/ high 2) (/ wide 2) (/ high 2))
                ;(gimp-edit-blend gradient2-layer BLEND-CUSTOM LAYER-MODE-DIFFERENCE style2 100 0 REPEAT-SAWTOOTH reverse2 FALSE 0 0 TRUE 1 (* high 0.25) (* wide 0.15) (* high 0.25))
                (gimp-edit-blend gradient2-layer BLEND-CUSTOM LAYER-MODE-DIFFERENCE style2 100 0 REPEAT-SAWTOOTH reverse2 FALSE 0 0 TRUE (* wide widthfactor1) (* high heightfactor1) (* wide widthfactor2) (* high heightfactor2))
                ; Linear-lght
                (gimp-selection-none img)
            )
        )
        
        
        ; Create Gradient Map 
        (if (= mapgradientaction TRUE)
            (begin
                
                ;(gimp-context-set-gradient "Three bars sin")
                (gimp-context-set-gradient mapgradient)
                (gimp-item-set-visible bg-layer FALSE)
                (gimp-edit-copy-visible img) 
                (gimp-image-insert-layer img dupe-layer 0 -1)
                (gimp-edit-clear dupe-layer)
                (gimp-image-raise-layer-to-top img dupe-layer)
                (gimp-edit-paste dupe-layer img)
                (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel img)))
                (plug-in-gradmap 1 img dupe-layer)
            )
        )
        
        (if (= secondgradientaction TRUE)
            (begin
                (gimp-layer-set-mode gradient2-layer LAYER-MODE-ADDITION)
                (gimp-layer-set-opacity gradient2-layer 62.0)
            )
        )
        
        
        ;(gimp-message "DEBUG crashout")
        ;(gimp-display-new img)
        ;(quit) 
        
        ; Clean up jagged edges
        
        (gimp-selection-layer-alpha text-layer)
        (gimp-selection-shrink img 1)
        (gimp-selection-invert img)
        (if (= mapgradientaction TRUE)
            (begin
                (gimp-edit-clear dupe-layer)
            )
        )
        (gimp-edit-clear gradient-layer)
        (gimp-edit-clear text-layer)
        (gimp-selection-invert img)
        
        ; Create drop shadow
        (if (= dropshadowaction TRUE)
            (begin
                ; (gimp-bucket-fill shadow-layer 0 0 100 0 FALSE 0 0)
                (gimp-edit-bucket-fill shadow-layer BUCKET-FILL-FG 0 100 0 FALSE 0 0)
                (gimp-selection-none img)
                (plug-in-gauss 1 img shadow-layer 20 20 0)
                (gimp-drawable-offset shadow-layer FALSE 1 8 8)
            )
        )
        
        ; Turn on background layers visibility
        
        (gimp-item-set-visible bg-layer TRUE)
        
        ; Re-enable undo and display new image
        
        (gimp-image-undo-group-end img)
        (gimp-display-new img)
        (gimp-displays-flush)
  )
)

; Setup user interface

(script-fu-register "script-fu-60s-text"
                "60's Text"
                "Gives text a psychadelic 60's effect. \nfile: 60's Text.scm"
                "Gregory M. Ross"
                "Gregory M. Ross"
                "March 23, 2009"
                ""
                SF-STRING     "Text" "Gimp60Gimp"
                SF-FONT       "Font" "Arial Bold"
                SF-ADJUSTMENT "Font Size (pixels)" '(100 2 1000 1 10 0 1)
                SF-COLOR      "Background Color" '(255 255 255)
                SF-GRADIENT   "Gradient 1"   "Full saturation spectrum CCW"
                SF-TOGGLE     "Gradient reverse"   FALSE
                SF-OPTION     "Gradient Style"      '("Linear" "Bi-linear" "Radial" "Square"
                                        "Conical (sym)" "Conical (asym)"
                                        "Shaped (angular)" "Shaped (spherical)"
                                        "Shaped (dimpled)" "Spiral (cw)"
                                        "Spiral (ccw)")
                SF-OPTION     "Gradient Direction option"      '("Horizontal" "Vertical" "Option2 - Halfway Down" "Option3 - High slant"
                                        "Option4 - Good for Radial"
                                        "Option5 - slanted topL to bottomR" "Option6 - random" "Option7 - narrow")
                SF-GRADIENT   "Gradient 2"   "Three bars sin"
                SF-TOGGLE     "Gradient reverse"   FALSE
                SF-OPTION     "Gradient Style"      '("Linear" "Bi-linear" "Radial" "Square"
                                        "Conical (sym)" "Conical (asym)"
                                        "Shaped (angular)" "Shaped (spherical)"
                                        "Shaped (dimpled)" "Spiral (cw)"
                                        "Spiral (ccw)")
                SF-GRADIENT   "Map Gradient"   "Three bars sin"
                SF-TOGGLE     "Map Gradient reverse"        FALSE
                SF-TOGGLE     "2) Do SecondGradient Step"        TRUE
                SF-TOGGLE     "3) Do MapGradient Gradient Step"  TRUE
                SF-TOGGLE     "4) Do Drop shadow Step"           TRUE
)
(script-fu-menu-register "script-fu-60s-text"
            "<Toolbox>/Script-Fu/Logos")

; end of script