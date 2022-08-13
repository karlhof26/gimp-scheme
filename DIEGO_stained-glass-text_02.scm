; Script to implement Conbagui Tutorial on Gimpchat (see Stained Glass Text topic) 
;
; Flow implemented to get the final result:
; 
; *** STEP 1A background 
;   ;prepare the BG but do not fill it until the size is properly adjusted
; *** STEP 1B text layer
;   ;create the stained-glass-text layer
; *** STEP 2  grow the selection
; *** STEP 3A duplicate the text layer and set both invisible
; *** STEP 3B select none in the text layer 
; *** added step: adjust image size	
;   ;NOTE! image size has to be adjusted to the created text size 
;   ;(script does not use a fixed size, BUT A DYNAMIC ONE based on entered text)	
; *** STEP 4  drop shadow on the text layer 
; *** STEP 5  inner shadow on the text layer 
; *** STEP 6A inner glow on the text layer 
;             note: not possible to set the gradient, used colour...
; *** STEP 6B ...then added gradient overlay on the layer 
; *** STEP 7  stroke the text layer	
; *** STEP 8A select the stroke layer
; *** STEP 8B emboss the stroke layer 
; *** STEP 9  overlay the text layer with the selected glass pattern
; *** STEP 10A select the pattern layer	
; *** STEP 10B emboss the pattern layer	
; *** STEP 11  satin on the top layer (duplicated text layer)
; *** STEP 12  gradient overlay on the top layer (duplicated text layer)
; 
; Fully dependent on Layereffects script. layerfx_02.scm
; ----------------------------------------------------------------
; ----------------------------------------------------------------
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(define (script-fu-stained-glass-txt
                theText
                textSize
                textFont
                strokeColor
                theBackgroundPattern
                theGlassPattern
                stainedGradient
                doFlatten
    )
    
    
    ; Variable init
    (let* 
        (
            (imageWidth 400)
            (imageHeight 200)
            (textSpacing)
            (theNewImage (car (gimp-image-new imageWidth imageHeight RGB)))
            (theBackgroundLayer)
            (theGlassLayer)
            (theDupGlassLayer)
            (theStrokeLayer)
            (thePatternLayer)
            (theMergedLayer)
            (theDisplay)
        )
        
        (gimp-message-set-handler 2)
        ; Initialize an undo, so the process can be undone with a single undo
        (gimp-image-undo-group-start theNewImage)
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        ; *** STEP 1A background	
        ;prepare the BG but do not fill it until the size is properly adjusted
        (set! theBackgroundLayer (car (gimp-layer-new theNewImage imageWidth imageHeight
                            RGBA-IMAGE "Wall" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer theNewImage theBackgroundLayer 0 0)
        ; *** STEP 1B text layer
        ;create the stained-glass-text layer
        (set! theGlassLayer (car (gimp-text-fontname theNewImage -1 0 0 
                            theText 10 TRUE textSize PIXELS textFont)))
        (set! textSpacing (/ textSize 7))   
        (gimp-item-set-name theGlassLayer "VITRO") ; to know the name of all subsequent layers created by layerfx
        (gimp-text-layer-set-letter-spacing theGlassLayer textSpacing)
        (gimp-text-layer-set-line-spacing theGlassLayer textSpacing)    ; Set Line Spacing
        (gimp-text-layer-set-justification theGlassLayer TEXT-JUSTIFY-CENTER)  ; Text Jusification 
        
        ; *** STEP 2  grow the selection
        ;(gimp-selection-all) ; added by karlhof26
        (gimp-selection-grow theNewImage 15)
        (gimp-image-select-item theNewImage CHANNEL-OP-ADD theGlassLayer)
        (gimp-edit-fill theGlassLayer FILL-FOREGROUND) ;fill the extended areas
        (gimp-message "line 105")
        
        ; *** STEP 3A duplicate the text layer and set both invisible	
        (set! theDupGlassLayer (car (gimp-layer-new-from-drawable theGlassLayer theNewImage)))
        (gimp-image-insert-layer theNewImage theDupGlassLayer 0 -1)
        (gimp-item-set-name theDupGlassLayer "VITRO-duplicate")
        (gimp-item-set-visible theGlassLayer FALSE)
        (gimp-item-set-visible theDupGlassLayer FALSE)
        ; *** STEP 3B select none in the text layer 
        (gimp-image-set-active-layer theNewImage theGlassLayer)
        (gimp-selection-none theNewImage)
        ; *** added step: adjust image size	
        ;NOTE! image size has to be adjusted to the created text size (script does not use a fixed size)
        (gimp-message "line 119")
        
        (gimp-image-resize-to-layers theNewImage)
        (set! imageWidth (car (gimp-image-width theNewImage)))      ;update
        (set! imageWidth (+ imageWidth (* 2 textSpacing)))
        (set! imageHeight (car (gimp-image-height theNewImage)))    ;update
        ;(gimp-message (number->string imageHeight))
        (set! imageHeight (+ imageHeight (* 2 textSpacing)))
        ;(gimp-message (number->string imageHeight))
        (gimp-image-resize theNewImage imageWidth imageHeight textSpacing textSpacing)
        (gimp-layer-resize-to-image-size theGlassLayer)             ;update
        (gimp-layer-resize-to-image-size theDupGlassLayer)          ;update
        (gimp-layer-resize-to-image-size theBackgroundLayer)        ;update
        
        (gimp-message "line 133")
        ; >>>  now fill the BG, size has been properly adjusted
        (gimp-image-set-active-layer theNewImage theBackgroundLayer)
        (gimp-context-set-pattern theBackgroundPattern)
        (gimp-edit-fill theBackgroundLayer FILL-PATTERN)
        ; >>> restore the active layer
        (gimp-image-set-active-layer theNewImage theGlassLayer)
        ; *** STEP 4  drop shadow on the text layer 
        ;(python-layerfx-drop-shadow RUN-NONINTERACTIVE
        ;    theNewImage theGlassLayer '(0 0 0) 100 0 0 MULTIPLY-MODE 8 14 120 20 TRUE FALSE)
        (gimp-message "off to drop-shadow")
        (script-fu-layerfx-drop-shadow theNewImage theGlassLayer
            '(0 0 0) 100 0 0 LAYER-MODE-MULTIPLY-LEGACY 8 14 120 20 TRUE FALSE)
        (gimp-message "backfrom to drop-shadow")
        (gimp-message "line 147")
        
        ; *** STEP 5  inner shadow on the text layer
        (script-fu-layerfx-inner-shadow theNewImage theGlassLayer
            '(0 0 0) 62 0 0 LAYER-MODE-SOFTLIGHT-LEGACY 0 4 12 120 12 FALSE)
        ;(python-layerfx-inner-shadow RUN-NONINTERACTIVE
        ;        theNewImage theGlassLayer '(0 0 0) 62 0 0 SOFTLIGHT-MODE 0 4 12 120 12 FALSE)
        
        ; *** STEP 6A inner glow on the text layer (note: not possible to set the gradient, used colour)
        (script-fu-layerfx-inner-glow theNewImage theGlassLayer
            '(200 200 100) 50 1 0 LAYER-MODE-DODGE-LEGACY 0 5 27 FALSE)
        ;(python-layerfx-inner-glow RUN-NONINTERACTIVE
        ;        theNewImage theGlassLayer '(200 200 100) 50 1 0 DODGE-MODE 0 5 27 FALSE)
        (gimp-message "line 156")
        
        ; *** STEP 6B ...then added gradient overlay on the layer 
        (gimp-message "reminder to FIX Gradient overlay...skipped for now")
        (script-fu-layerfx-gradient-overlay
            theNewImage theGlassLayer 0 LAYER-MODE-OVERLAY-LEGACY stainedGradient  ; mode is 0 to 3 (0= Custom (but mapped to 3) 1= FG-BG HSV, 3=Custom i.e. use AGrainbow) 
            0 1 0 ; type repeat reverse
            50 LAYER-MODE-NORMAL-LEGACY ; opacity blendingmode (in the gradient) 
            500.0 400.0 ; X Y
            123.0 100.0 ; angle width
            FALSE ) ; MERGE
                    ;[mode is traditional; 0 is default; 1 is dissolve; 3 is multiply]
                    ; [style: 0 is linear]]
                    ; [[repeat: 0 is none 1 is sawtooth]]
       ; (script-fu-layerfx-gradient-overlay
       ;         theNewImage theGlassLayer 0 0 "AG ribbon1" 1 0 FALSE 75 OVERLAY-MODE
       ;         0.0 0.0 0 100 FALSE)
       ; '(250 0 255) '(123 180 254)
        
        ;;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE
        ;;        theNewImage theGlassLayer "AG_ribbon1" 1 0 FALSE 75 OVERLAY-MODE
        ;;        0.0 0.0 0 100 FALSE)
        (gimp-message "line 182")
        (gimp-displays-flush)
        
        ; *** STEP 7  stroke the text layer
        ;(define CGP (list 3 64 3)) ; define the list for a colour or gradient or pattern: DARK GREEN
        (gimp-message "off to stroke")
        (script-fu-layerfx-stroke
                theNewImage theGlassLayer strokeColor 100 LAYER-MODE-NORMAL-LEGACY 10 100 FALSE)
        (gimp-message "back from stroke")
        
        ;(python-layerfx-stroke RUN-NONINTERACTIVE
        ;        theNewImage theGlassLayer strokeColor 100 NORMAL-MODE 10 100 FALSE) ; does not take the pattern, only a color!!!
        (gimp-message "line 194")
        (gimp-displays-flush)
        
        ; *** STEP 8A select the stroke layer
        (set! theStrokeLayer (car (gimp-image-get-layer-by-name theNewImage "VITRO-stroke")))  
        ;(gimp-message (number->string theStrokeLayer))
        (gimp-image-set-active-layer theNewImage theStrokeLayer)
        ;(gimp-context-set-pattern "Leather") ;addition 
        ;(gimp-image-select-item theNewImage CHANNEL-OP-ADD theStrokeLayer)	;alpha to selection
        ;(gimp-edit-fill theStrokeLayer FILL-PATTERN)
        
        ; *** STEP 8B emboss the stroke layer
        (script-fu-layerfx-bevel-emboss
                theNewImage theStrokeLayer 2 65 1 15 5 120 30 0
                '(255 255 255) LAYER-MODE-OVERLAY-LEGACY 66 '(0 0 0) LAYER-MODE-MULTIPLY-LEGACY 62
                0 ; contour
                FALSE FALSE)
                
        ;(python-layerfx-bevel-emboss RUN-NONINTERACTIVE
        ;        theNewImage theStrokeLayer 2 65 1 15 5 120 30 0
        ;        '(255 255 255) OVERLAY-MODE 66 '(0 0 0) MULTIPLY-MODE 62 6 TRUE
        ;        "texturemate abstract09" 100 100 FALSE FALSE)	
        ; *** STEP 9  overlay the text layer with the selected glass pattern
        (gimp-message "line 217")
        (gimp-image-set-active-layer theNewImage theGlassLayer)
        (gimp-image-select-item theNewImage CHANNEL-OP-ADD theGlassLayer) ;alpha to selection
        (gimp-context-set-pattern theGlassPattern)
        (gimp-message "line 221")
        
        (script-fu-layerfx-pattern-overlay
                theNewImage theGlassLayer theGlassPattern 100 LAYER-MODE-NORMAL-LEGACY 50 0 0)
                
        ;(python-layerfx-pattern-overlay RUN-NONINTERACTIVE
        ;        theNewImage theGlassLayer theGlassPattern 100 NORMAL-MODE 50 0 0)
        (gimp-message "line 228")
        
        ; *** STEP 10A select the pattern layer
        (set! thePatternLayer (car (gimp-image-get-layer-by-name theNewImage "VITRO-pattern")))
        (gimp-message (number->string theStrokeLayer))
        (gimp-message (number->string thePatternLayer))
        (gimp-message "line 234")
        (gimp-displays-flush)
        
        (gimp-image-set-active-layer theNewImage thePatternLayer)
        ; *** STEP 10B emboss the pattern layer
        (script-fu-layerfx-bevel-emboss
                theNewImage thePatternLayer 1 65 1 9 9 120 30 0
                '(255 250 255) LAYER-MODE-DODGE-LEGACY 75 '(0 0 0) LAYER-MODE-OVERLAY-LEGACY 45 
                0
                FALSE FALSE)
        ;(python-layerfx-bevel-emboss RUN-NONINTERACTIVE
        ;        theNewImage thePatternLayer 1 65 1 9 9 120 30 0
        ;        '(255 255 255) DODGE-MODE 75 '(0 0 0) OVERLAY-MODE 45 6 TRUE
        ;        "Bubbleglass01" 150 50 FALSE FALSE)
        (gimp-message "line 249")        
        ; *** STEP 11  satin on the top layer (duplicated text layer)
        (gimp-message (number->string theDupGlassLayer))
        (gimp-message (number->string thePatternLayer))
        (gimp-item-set-visible theDupGlassLayer TRUE)
        (gimp-image-set-active-layer theNewImage theDupGlassLayer)
        (gimp-message "line 255")
        
        (script-fu-layerfx-satin
                theNewImage theDupGlassLayer '(255 255 250) 75 9 8 10.0 20 2 TRUE FALSE)
        ;(python-layerfx-satin RUN-NONINTERACTIVE
        ;        theNewImage theDupGlassLayer '(255 255 255) 75 9=SOFTLIGHT(mapped to 19) 8 20 20 5.5 TRUE FALSE)
        ; added by karlhof26 
        (gimp-layer-set-opacity theDupGlassLayer 60.0)
        
        (gimp-message "line 264")  
        ; *** STEP 12  gradient overlay on the top layer (duplicated text layer) 
        (gimp-message "reminder to FIX Gradient overlay...skipped for now")
       ; (script-fu-layer-effects-gradient-overlay
       ;     theNewImage theGlassLayer '(250 0 255) '(123 180 254) 3 "AG rainbow3" ; mode is 0 to 3 (1= FG-BG HSV, 3=Custom i.e. use AGrainbow) 
       ;     0.0 4.03 90 ; angle scale(0.0 to 5.0) opacity
       ;     0 0 1) ; mode, style and repeat
       ;             ;[mode is traditional; 0 is default; 1 is dissolve; 3 is multiply]
       ;             ; [style: 0 is linear]] 
       ;             ; [[repeat: 0 is none 1 is sawtooth]]
        (script-fu-layerfx-gradient-overlay
            theNewImage theGlassLayer 0 LAYER-MODE-SCREEN-LEGACY stainedGradient ; mode is 0 to 3 (0= Custom (but mapped to 3) 1= FG-BG HSV, 3=Custom i.e. use AGrainbow) 
            0 1 1 ; type repeat reverse
            80 LAYER-MODE-NORMAL-LEGACY ; opacity blendingmode (in the gradient)
            200.0 100.0 ; X Y
            123.0 100.0 ; angle width
            FALSE ) ; MERGE
       ; (script-fu-layerfx-gradient-overlay
       ;         theNewImage theGlassLayer 0 0 "AG_rainbow3" 0 0 TRUE 30 SCREEN-MODE
       ;         0.0 0.0 90 100 FALSE)
        
        ;;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE
        ;;       theNewImage theDupGlassLayer "AG_rainbow3" 0 0 TRUE 30 SCREEN-MODE
        ;;        0.0 0.0 90 100 FALSE)
        
        ; ***************************   FINAL OUTPUT ***************************************************	
        (set! theDisplay (car (gimp-display-new theNewImage)))
        
        (if (= doFlatten TRUE)
            (gimp-image-flatten theNewImage)
        )
        (gimp-selection-none theNewImage)
        ;Ensure the updated image is displayed now
        (gimp-displays-flush)
        (gimp-message "Good finish")
        (gimp-image-undo-group-end theNewImage)
    )
)

(script-fu-register "script-fu-stained-glass-txt"
            "<Toolbox>/Script-Fu/Logos/Stained Glass Text"
            "Write a text with a stained glass appearance \n file:DIEGO_stained0glass-text_02.scm"
            "Diego Nassetti"
            "Diego Nassetti"
            "26/05/2014"
            ""
            SF-TEXT         "Text"                  "VITROa"
            SF-ADJUSTMENT   "Font size in pixels"   '(350 15 350 1 1 0 1)
            SF-FONT         "Font"                  "Sans Bold"
            SF-COLOR        "Stroke color"          '(43 37 29)
            SF-PATTERN      "Background Pattern"    "Bricks" ;select a Pattern for the background
            SF-PATTERN      "Glass Pattern"         "Craters" ;select a Pattern for the selected areas
            SF-GRADIENT     "Stained Gradient"      "Horizon 1"
            SF-TOGGLE       "Flatten Image"         FALSE 
)

; end of file