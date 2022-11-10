; FU_edges_picture-frame.scm
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 03/09/2020 on GIMP-2.10.20
;
; 02/14/2014 - convert to RGB if needed 
; =======================================================================
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
; In ADDITION:
; The Crown-Moulding-01.ggr (and 10 others)
; should be placed in teh GIMP gradients folder.
; (while they can be copied there individually,
; I simply drop the whole folder into /gradients.)
;
; Windows Vista/7/8
; 	C:\Program Files\GIMP 2\share\gimp\2.0\gradients\Pictureframe
; 	or
; 	C:\Users\YOUR-NAME\.gimp-2.8\gradients\Pictureframe
; 		
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\gradients\Pictureframe
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\gradients\Pictureframe
;	
;	Linux
;	/home/yourname/.gimp-2.8/gradients/Pictureframe
;	or
;	Linux - system-wide
;	/usr/share/gimp/2.0/gradients/Pictureframe
; ========================================================================
; Original script information:
;
; Gradient Frame rel 0.02
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
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; ------------
;| Change Log |
; ------------ 
; Rel 0.01 - Initial Release
; Rel 0.02 - Added an optional Vignette and changed the tint layer position to give a better color 
; ========================================================================
(define (script-fu-picture-frame image drawable
        xsize
        ysize
        gradient
        reverse
        tint
        tint-color
        use-pattern
        pattern
        lomo
        lomo-size
        lomo-opacity
        keep-selection-in
        conserve
    )
                        
    (gimp-message "line95")
    (gimp-context-push)
    (gimp-image-undo-group-start image)
    (if (not (= RGB (car (gimp-image-base-type image))))
        (gimp-image-convert-rgb image)
    )
            
    (let* (
            (image-layer (car (gimp-image-get-active-layer image)))
            (old-width (car (gimp-drawable-width image-layer)))
            (old-height (car (gimp-drawable-height image-layer)))
            (width (+ old-width (* xsize 2)))
            (height (+ old-height (* ysize 2)))
            (alpha (car (gimp-drawable-has-alpha image-layer)))
            (typeA (car (gimp-drawable-type-with-alpha image-layer)))
            (sel (car (gimp-selection-is-empty image)))
            (layer-name (car (gimp-drawable-get-name image-layer)))
            (keep-selection keep-selection-in)
            (original-selection-channel 0)
            (selection-channel 0)
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (border-layer 0)
            (tint-layer 0)
            (bump-layer 0)
            (*newpoint-top* (cons-array 10 'double))
            (*newpoint-bot* (cons-array 10 'double))
            (*newpoint-left* (cons-array 10 'double))
            (*newpoint-right* (cons-array 10 'double))
        )   
        ;(gimp-message "line124")
        (gimp-context-set-default-colors)
        
        ;;;;create original-selection-channel if a selection exists (gimp-selection-load original-selection-channel)
        (if (= sel FALSE)
            (begin
                (gimp-selection-save image)
                (set! original-selection-channel (car (gimp-image-get-active-drawable image)))
                (gimp-channel-set-opacity original-selection-channel 100)
                (gimp-drawable-set-name original-selection-channel "original-selection-channel")
                (gimp-image-set-active-layer image image-layer)	
                (gimp-selection-none image)
            )
        )
        
        (if (= alpha FALSE)
            (begin
                (gimp-layer-add-alpha image-layer)
            )
        )
        ;(gimp-message "line144")
        ;;;;create the Image selection  
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 10 10)
        
        ;(gimp-rect-select image
        ;                (car (gimp-drawable-offsets image-layer))
        ;                (cadr (gimp-drawable-offsets image-layer))
        ;                old-width
        ;                old-height
        ;                CHANNEL-OP-REPLACE
        ;                FALSE
        ;                10)    
        (gimp-image-select-rectangle image
                        CHANNEL-OP-REPLACE
                        (car (gimp-drawable-offsets image-layer))
                        (cadr (gimp-drawable-offsets image-layer))
                        old-width
                        old-height)
        
        ;;;;create selection-channel (gimp-selection-load selection-channel)
        (gimp-selection-save image)
        (set! selection-channel (car (gimp-image-get-active-drawable image)))	
        (gimp-channel-set-opacity selection-channel 100)
        (gimp-drawable-set-name selection-channel "selection-channel")
        (gimp-image-set-active-layer image image-layer)
        (gimp-selection-none image)
        
        ;;;;begin the script
        ;(gimp-message "line173")
        ;;;;prepare for the border
        (if (= lomo TRUE)
            (begin
                ;(gimp-message "Lomo True")
                (the-picture-frame-K-vignette image image-layer lomo-size lomo-opacity FALSE FALSE)
            )
            (begin
                ;(gimp-message "Lomo False")
            )
        )
        ;(gimp-message "line184")
        (gimp-image-resize image width height xsize ysize)
        
        (set! border-layer (car (gimp-layer-new image width height typeA "Border" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image border-layer 0 -1)
        
        (set! tint-layer (car (gimp-layer-new image width height typeA "Tint" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image tint-layer 0 1)
        (if (= tint TRUE)
            (begin
                (gimp-context-set-background tint-color)
                (gimp-selection-load selection-channel)
                (gimp-selection-invert image)
                (gimp-layer-set-mode border-layer LAYER-MODE-GRAIN-MERGE)
                (gimp-edit-fill tint-layer FILL-BACKGROUND)
            )
        )
        (gimp-image-set-active-layer image border-layer)
        ;(gimp-message "line202")
        (set! bump-layer (car (gimp-layer-new image width height typeA "Grain" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image bump-layer 0 -1)
        (gimp-drawable-set-visible bump-layer FALSE)
        (if (= use-pattern TRUE)
            (begin
                (gimp-context-set-pattern pattern)
                (gimp-edit-fill bump-layer FILL-PATTERN)
                (gimp-drawable-desaturate bump-layer DESATURATE-LUMINANCE)
            )
        )
        (gimp-selection-none image)
        
        (aset *newpoint-top* 0 0)    ; set the top arrays
        (aset *newpoint-top* 1 0)
        (aset *newpoint-top* 2 xsize)
        (aset *newpoint-top* 3 ysize)
        (aset *newpoint-top* 4 (+ xsize old-width))
        (aset *newpoint-top* 5 ysize)
        (aset *newpoint-top* 6 width)
        (aset *newpoint-top* 7 0)
        (aset *newpoint-top* 8 0)
        (aset *newpoint-top* 9 0)
        
        (aset *newpoint-bot* 0 0)    ; set the bottom arrays
        (aset *newpoint-bot* 1 height)
        (aset *newpoint-bot* 2 xsize)
        (aset *newpoint-bot* 3 (+ ysize old-height))
        (aset *newpoint-bot* 4 (+ xsize old-width))
        (aset *newpoint-bot* 5 (+ ysize old-height))
        (aset *newpoint-bot* 6 width)
        (aset *newpoint-bot* 7 height)
        (aset *newpoint-bot* 8 0)
        (aset *newpoint-bot* 9 height)
        
        (aset *newpoint-left* 0 0)    ; set the left arrays
        (aset *newpoint-left* 1 0)
        (aset *newpoint-left* 2 xsize)
        (aset *newpoint-left* 3 ysize)
        (aset *newpoint-left* 4 xsize)
        (aset *newpoint-left* 5 (+ ysize old-height))
        (aset *newpoint-left* 6 0)
        (aset *newpoint-left* 7 height)
        (aset *newpoint-left* 8 0)
        (aset *newpoint-left* 9 0)
        
        (aset *newpoint-right* 0 width)    ; set the right arrays
        (aset *newpoint-right* 1 0)
        (aset *newpoint-right* 2 (+ xsize old-width))
        (aset *newpoint-right* 3 ysize)
        (aset *newpoint-right* 4 (+ xsize old-width))
        (aset *newpoint-right* 5 (+ ysize old-height))
        (aset *newpoint-right* 6 width)
        (aset *newpoint-right* 7 height)
        (aset *newpoint-right* 8 width)
        (aset *newpoint-right* 9 0)
        
        (gimp-context-set-gradient gradient)
        ;;;;create the top of border
        (gimp-free-select image 10 *newpoint-top* 2 TRUE FALSE 15)
        ;(gimp-message "line262")
        
        (gimp-edit-blend border-layer BLEND-CUSTOM LAYER-MODE-NORMAL  GRADIENT-LINEAR 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE (/ width 2) 0 (/ width 2) ysize)
        
        ;;;;create the bottom of border
        (gimp-free-select image 10 *newpoint-bot* 2 TRUE FALSE 15)
        (gimp-edit-blend border-layer BLEND-CUSTOM LAYER-MODE-NORMAL  GRADIENT-LINEAR 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE (/ width 2) height (/ width 2) (+ old-height ysize))
        
        ;;;;create the left of border
        (gimp-free-select image 10 *newpoint-left* 2 TRUE FALSE 15)
        (gimp-edit-blend border-layer BLEND-CUSTOM LAYER-MODE-NORMAL  GRADIENT-LINEAR 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE 0 (/ height 2) xsize (/ height 2))
        
        ;;;;create the right of border
        (gimp-free-select image 10 *newpoint-right* 2 TRUE FALSE 15)
        (gimp-edit-blend border-layer BLEND-CUSTOM LAYER-MODE-NORMAL  GRADIENT-LINEAR 100 0 REPEAT-NONE reverse FALSE 3 0.2 TRUE width (/ height 2) (+ old-width xsize) (/ height 2))
        (gimp-selection-none image)
        
        ;(gimp-message "line279")
        
        (gimp-image-set-active-layer image border-layer)
        (if (= tint TRUE)
            (begin
                (plug-in-sample-colorize 1 image border-layer tint-layer TRUE FALSE FALSE TRUE 0 255 1.00 0 255)
            )
        )
        (if (= use-pattern TRUE)
            (begin
                (plug-in-bump-map 1 image border-layer bump-layer 135 45 3 0 0 0 0 TRUE FALSE 1) ; was LINEAR
            )
        )
        
        ;;;;finish the script
        (if (= conserve FALSE)
            (begin  
                (set! border-layer (car (gimp-image-merge-down image border-layer EXPAND-AS-NECESSARY)))
                (set! image-layer (car (gimp-image-merge-down image border-layer EXPAND-AS-NECESSARY)))
                (gimp-image-remove-layer image bump-layer)
                (gimp-drawable-set-name image-layer layer-name) 
            )
        )
        (if (and (= sel TRUE)(= keep-selection TRUE))
            (begin
                (gimp-selection-load selection-channel)
            )
        )
        (if (and (= sel FALSE) (= keep-selection TRUE))
            (begin
                (gimp-selection-load original-selection-channel)
            )
        )
        (gimp-image-remove-channel image selection-channel)
        (if (= sel FALSE) (gimp-image-remove-channel image original-selection-channel))
        (if (and (= conserve FALSE) (= alpha FALSE))
            (gimp-layer-flatten image-layer)
        )
        
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
        (gc) ; memory clenup; an array as used;
    )
)

(script-fu-register "script-fu-picture-frame"             
    "Picture Frame"
    "Creates a Moulded Picture Frame using Gradients with options for Tint Colors and a grain from patterns. \nfile:FU_edges_picture-frame.scm"
    "Graechan"
    "Graechan - http://gimpchat.com"
    "NOV 2012"
    "*"
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-ADJUSTMENT  "Border Width"           '(50 1 250 1 10 0 1)
    SF-ADJUSTMENT  "Border Height"          '(50 1 250 1 10 0 1)
    SF-GRADIENT     "Border Gradient"       "Crown molding"
    SF-TOGGLE       "Reverse Gradient"      FALSE
    SF-TOGGLE       "Use Color Tint"        TRUE
    SF-COLOR        "Tint Color"            '(169 134 41)
    SF-TOGGLE       "Use Grain Pattern"     FALSE
    SF-PATTERN      "Grain Pattern"         "Crack"
    SF-TOGGLE       "Apply Vignette"        TRUE
    SF-ADJUSTMENT   "Vignette Size"         '(100 0 200 1 10 0 0)
    SF-ADJUSTMENT   "Vignette Opacity"      '(80 0 100 1 10 0 0)
    SF-TOGGLE       "Keep selection"        FALSE
    SF-TOGGLE       "Keep the Layers"       FALSE
)

(script-fu-menu-register "script-fu-picture-frame" "<Image>/Script-Fu/Edges/")

(define (the-picture-frame-K-vignette image drawable
        size
        opacity
        keep-selection
        conserve)       
        
    (let* (
            (image-layer (car (gimp-image-get-active-layer image)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (alpha (car (gimp-drawable-has-alpha image-layer)))
            (layer-name (car (gimp-drawable-get-name image-layer)))
            (name-string (string-append (car (gimp-drawable-get-name image-layer)) "-Vignette"))
            (sel (car (gimp-selection-is-empty image)))
            (original-selection-channel 0)
            (vignette-layer 0)
            (copy-layer 0)
            (vig-x width)
            (vig-y height)
            (feather 120)
        )
        
        ;(gimp-message "line373 vignette")
        (gimp-context-push)
        (gimp-image-undo-group-start image)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        (if (= alpha FALSE)
            (begin
                (gimp-layer-add-alpha image-layer)
            )
        )
        
        
        ;;;;save the original selection
        (if (= sel FALSE)
            (begin    
                (gimp-selection-save image)
                (set! original-selection-channel (car (gimp-image-get-active-drawable image)))
                (gimp-channel-set-opacity original-selection-channel 100)
                (gimp-drawable-set-name original-selection-channel "Original selection")
                (gimp-selection-none image)
                (gimp-image-set-active-layer image image-layer)
            )
        )
        
        ;(gimp-message "line395 vignette")
        
        ;;;;apply the vignette
        ;(gimp-message (number->string vig-x))
        
        (if (< width 640)
            (begin
                ;(gimp-message "small-ish so make bigger")
                (set! vig-x 640)
            )
        )
        (if (< height 520)
            (begin
                (set! vig-y 520)
            )
        )
        (if (< width 640) (set! feather (/ width 5.33)))
        (if (< height 520) (set! feather (/ height 4.33)))
        
        (set! vignette-layer (car (gimp-layer-new image vig-x vig-y  RGBA-IMAGE "Vignette" 100 LAYER-MODE-HARDLIGHT )))
        (gimp-image-insert-layer image vignette-layer 0 -1)
        
        ;(gimp-message "line400 ellipse select")
        (gimp-context-set-antialias TRUE)
        (gimp-context-set-feather FALSE)
        ;(gimp-ellipse-select image 0 0 vig-x vig-y 0 TRUE FALSE 10)
        (gimp-image-select-ellipse image CHANNEL-OP-ADD 0 0 vig-x vig-y)
        
        (gimp-selection-shrink image size)
        (gimp-selection-feather image feather)
        (gimp-selection-invert image)
        (gimp-image-set-active-layer image vignette-layer)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        ;(gimp-image-resize image vig-x vig-y 0 0)
        ;(gimp-edit-blend vignette-layer FG-BG-RGB-MODE NORMAL-MODE GRADIENT-RADIAL 100 0 REPEAT-NONE TRUE FALSE 3 0.2 TRUE (/ vig-x 2) (/ vig-y 2) vig-x vig-y)		
        (gimp-context-set-gradient "FG to Transparent")
        (gimp-edit-blend vignette-layer BLEND-CUSTOM LAYER-MODE-NORMAL   GRADIENT-RADIAL 100 0 REPEAT-NONE TRUE FALSE 3 0.2 TRUE (/ vig-x 2) (/ vig-y 2) vig-x vig-y)
        (gimp-selection-none image)
        (gimp-layer-scale vignette-layer width height FALSE)
        (set! copy-layer (car (gimp-layer-copy vignette-layer TRUE)))
        (gimp-image-insert-layer image copy-layer 0 -1)
        (set! vignette-layer (car (gimp-image-merge-down image copy-layer EXPAND-AS-NECESSARY)))
        (gimp-drawable-set-name vignette-layer "Vignette")
        (gimp-layer-set-opacity vignette-layer opacity)
        
        ;(gimp-message "line431")
        
        (if (= conserve FALSE)
            (begin
                (set! vignette-layer (car (gimp-image-merge-down image vignette-layer EXPAND-AS-NECESSARY)))
                (gimp-item-set-name vignette-layer name-string)
            )
        )
        ;(gimp-drawable-set-name image-layer layer-name)
        (if (and (= conserve FALSE) (= alpha FALSE))
            (gimp-layer-flatten vignette-layer)
        )
        (if (and (= keep-selection TRUE) (= sel FALSE))
            (gimp-selection-load original-selection-channel)
        )
        (if (= sel FALSE)
            (gimp-image-remove-channel image original-selection-channel)
        )
        
        ;(gimp-message "line462 vignette return")
        (gimp-displays-flush)
        (gimp-image-undo-group-end image)
        (gimp-context-pop)
    )
) 
 
; end of script
; end of script