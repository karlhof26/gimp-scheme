; FU_effects_reflection.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 04/09/2020 on GIMP-2.10.20 
;
; 02/14/2014 - convert to RGB if needed
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;==============================================================
; Original information 
; 
; Copyright (C) 2005-2007 Otavio Correa Cordeiro (otavio gmail com)
; Create a reflection effect like Apple iWeb does..
;
; modified by Paul Sherman to work in GIMP 2.4.2 on 11/30/2007
;
; modified by David Cummins and Paul Sherman Dec 2007, tested on GIMP-2.4.3
; * added user settings to control the height of the generated reflection,
;   the gradient mask starting point (presented as Fade Rate %), and options
;   to keep the generated reflection as a separate layer, or on a single layer
;   either transparent or flattened.
; * added code to "play nice" with the current GIMP environment: colors and
;   gradient prior selections are preserved, as well as the original layer
;   name (handled using careful stack order).
;   generated layers are constrained around the original layer.
; * simplified some of the internal logic so repeated calculations are done
;   only once, reformatted, organised the code, added comments, etc.
; * flattened on start (to avoid errors)
; * undo ability functional
;
; Wed Oct  1 22:14:40 EDT 2008
; Modified to remove deprecated procedures as listed:
;     gimp-flip  ==>  gimp-drawable-transform-flip-simple
; added extra parameters
; also flushed before undo-end
;==============================================================


(define (draw-line layer x-from y-from x-to y-to) 
    (let* (
            (points (cons-array 4 'double))
        )
        (aset points 0 x-from)
        (aset points 1 y-from)
        (aset points 2 x-to  )
        (aset points 3 y-to  )
        
        (gimp-pencil layer 4 points)
    )
)

(define (fu-gimp-reflection
        theImage
        theLayer
        userHeight
        userFade
        new_layer
        transparentBG
        make_line
        )
    
    (let* (
                (originalWidth  (car (gimp-image-width theImage)))
                (originalHeight (car (gimp-image-height theImage)))
                (old-bg '(0 0 0))
                (old-fg '(255 200 200))
                (old-grad)
                (fadeStart 0)
                (fadeColor 0) 
                (reflectionScale 0) 
                (stackPos 1)
                (newWidth 1) 
                (newHeight 1)
                (gradX 1)
                (gradY1 1)
                (gradY2 1)
                (new-layer 1)
                (new-mask 0)
                (bg-layer 0)
            )
            
        (set! originalWidth (car (gimp-image-width theImage)))
        (set! originalHeight (car (gimp-image-height theImage)))
        
        (gimp-image-undo-group-start theImage)
        
        (if (not (= RGB (car (gimp-image-base-type theImage))))
                (gimp-image-convert-rgb theImage))
        
        ;    (gimp-selection-all theImage)
        (gimp-selection-none theImage)
        (set! theLayer (car(gimp-image-merge-visible-layers theImage 0)))
        
        
        (if (= make_line TRUE)
            (begin
                (gimp-context-set-background '(255 255 255))
                (gimp-context-set-foreground '(000 000 000))
                (gimp-context-set-brush "Circle (03)"    )   
                (draw-line theLayer 0 originalHeight originalWidth originalHeight)
            )
        )
        
        ;preserve original settings
        (set! old-bg   (car (gimp-context-get-background)))
        (set! old-fg   (car (gimp-context-get-foreground)))
        (set! old-grad (car (gimp-context-get-gradient)))
        
        ;calculate color for start of gradient fade
        (set! fadeStart (* (- 100 userFade) 2.55))
        (set! fadeColor (list fadeStart fadeStart fadeStart))
        
        (set! reflectionScale (/ userHeight 100))
        (set! stackPos (car(gimp-image-get-item-position theImage theLayer)))
        (set! newWidth originalWidth)
        (set! newHeight (* originalHeight (+ reflectionScale 1)))
        (set! gradX (/ originalWidth 2))
        (set! gradY1 (* originalHeight reflectionScale))
        (set! gradY2 (* originalHeight (* reflectionScale -1)))
        
        (gimp-image-resize theImage originalWidth newHeight 0 0)
        (set! new-layer (car (gimp-layer-copy theLayer 1)))
        (gimp-image-insert-layer theImage new-layer 0 (+ stackPos 0))
        (gimp-item-set-name new-layer "Reflection")
        (gimp-layer-set-offsets new-layer 0 originalHeight)
        (gimp-item-transform-flip-simple new-layer ORIENTATION-VERTICAL TRUE 0.0)
        
        
        (set! new-mask (car (gimp-layer-create-mask new-layer 0)))
        (gimp-layer-add-mask new-layer new-mask)
        
        (gimp-context-set-foreground fadeColor)
        
        (gimp-edit-blend new-mask BLEND-FG-TRANSPARENT LAYER-MODE-NORMAL
            GRADIENT-LINEAR 100 0 REPEAT-NONE
            FALSE
            FALSE 0 0 TRUE
            gradX gradY1 gradX gradY2)
        
        
        (if (= new_layer FALSE)
            (begin
                (if (= transparentBG TRUE)
                    (begin ;# NO separate layer, transparent ##############
                        (gimp-image-merge-visible-layers theImage 1)
                    )
                    (begin ;# NO separate layer, NOT transparent ##########
                        (gimp-image-flatten theImage)
                    )
                )
                ; final crop not needed for new_layer FALSE
            )
            (begin
                (if (= transparentBG TRUE)
                    (begin ;# separate layer, transparent ###############
                        (gimp-image-set-active-layer theImage new-layer)
                    )
                    (begin ;# separate layer, NOT transparent ###########
                        (set! bg-layer (car(gimp-layer-new theImage originalWidth newHeight 0 "Reflection BG" 100 0)))
                        (gimp-image-insert-layer theImage bg-layer 0 (+ stackPos 2))
                        (gimp-selection-all theImage)
                        (gimp-edit-bucket-fill bg-layer 1 0 100 255 0 1 1)
                        (gimp-selection-none theImage)
                        (gimp-image-set-active-layer theImage new-layer)
                    )
                )
                ; the Reflection layer still overflows the image here
                (gimp-image-crop theImage originalWidth newHeight 0 0)
            )
        )
        
        ;restore original settings
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-background old-bg)
        (gimp-context-set-gradient old-grad)
        (gimp-displays-flush)
        (gimp-image-undo-group-end theImage)
    )
)

(script-fu-register "fu-gimp-reflection"
    "<Image>/Script-Fu/Effects/Reflection"
    "Reflection -- extends lower section of an image as a reflection of the original image. \nfile:FU_effects_reflection.scm"
    "Original author Otavio Cordeiro, later edited by David Cummins,Paul Sherman and Karl Hofmeyr"
    "Paul Sherman <psherman2001@gmail.com>"
    "Last updated 26/01/2018 - tested on GIMP-2.10.22"
    "*"
    SF-IMAGE        "Image"                                 0
    SF-DRAWABLE     "Drawable"                              0
    SF-ADJUSTMENT   "Reflection Height (% of original)"     '( 40 10 99 1 20 0 0)
    SF-ADJUSTMENT   "Fade Rate (%)"                         '(60 0 100 10 20 0 0)
    SF-TOGGLE       "Keep Reflection as a separate Layer"   FALSE
    SF-TOGGLE       "Transparent Background"                FALSE
    SF-TOGGLE       "Include a Separator Line?"             TRUE
)

; end of script