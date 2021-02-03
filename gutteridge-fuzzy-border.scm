;;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Fuzzy border script  for GIMP 2.4
; Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England.
; Updated by Karlhof26 on 26/05/2020 for Gimp 2.10.18
;
; Tags: border, fuzzy
;
; Author statement:
; Do a cool fade to a given colour at the border of an image (optional shadow)
; Will make image RGB if it isn't already.
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Define the function:

(define (script-fu-fuzzy-border-a inImage
                                inLayer
                                inColor
                                inSize
                                inBlur
                                inGranu
                                inShadow
                                inShadWeight
                                inCopy
                                inFlatten
        )
    
  (let* (
            (theWidth (car (gimp-image-width inImage)))
            (theHeight (car (gimp-image-height inImage)))
            (theImage 0)
            (theLayer 0)
        )
        
        
        (gimp-selection-all inImage)
        (set! theImage (if (= inCopy TRUE)
                     (car (gimp-image-duplicate inImage))
                     inImage
                   )
        )
        (gimp-image-undo-group-start theImage)
        
        (if (> (car (gimp-drawable-type inLayer)) 1)
            (gimp-image-convert-rgb theImage)
        )
        
        (set! theLayer (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGBA-IMAGE
                                        "layer 1"
                                        100
                                        LAYER-MODE-NORMAL)))
        
        (gimp-image-insert-layer theImage theLayer 0 0)
        
        
        (gimp-edit-clear theLayer)
        (chris-color-edge theImage theLayer inColor inSize)
        
        (gimp-layer-scale theLayer
                      (/ theWidth inGranu)
                      (/ theHeight inGranu)
                      TRUE)
        
        (plug-in-spread TRUE
                    theImage
                    theLayer
                    (/ inSize inGranu)
                    (/ inSize inGranu))
        (chris-color-edge theImage theLayer inColor 1)
        (gimp-layer-scale theLayer theWidth theHeight TRUE)
        
        (gimp-selection-layer-alpha theLayer)
        (gimp-selection-invert theImage)
        (gimp-edit-clear theLayer)
        (gimp-selection-invert theImage)
        (gimp-edit-clear theLayer)
        (gimp-context-set-background inColor)
        (gimp-edit-fill theLayer FILL-BACKGROUND)
        (gimp-selection-none inImage)
        (chris-color-edge theImage theLayer inColor 1)
        
        (if (= inBlur TRUE)
            (plug-in-gauss-rle TRUE theImage theLayer inSize TRUE TRUE)
        )
        (if (= inShadow TRUE)
            (begin
                (gimp-selection-none inImage)
                (gimp-image-add-layer theImage
                                (car (gimp-layer-copy theLayer FALSE)) 0)
                (gimp-layer-scale theLayer
                            (- theWidth inSize) (- theHeight inSize) TRUE)
                (gimp-drawable-desaturate theLayer DESATURATE-LUMINOSITY)
                (gimp-brightness-contrast theLayer 127 127)
                (gimp-invert theLayer)
                (gimp-layer-resize theLayer
                             theWidth
                             theHeight
                             (/ inSize 2)
                             (/ inSize 2))
                (plug-in-gauss-rle TRUE
                             theImage
                             theLayer
                             (/ inSize 2)
                             TRUE
                             TRUE)
                (gimp-layer-set-opacity theLayer inShadWeight)
            )
        )
        (if (= inFlatten TRUE)
            (gimp-image-flatten theImage)
        )
        (if (= inCopy TRUE)
            (begin
                (gimp-image-clean-all theImage)
                (gimp-display-new theImage)
            )
        )
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    )
)

(define (chris-color-edge inImage inLayer inColor inSize)
    (gimp-selection-all inImage)
    (gimp-selection-shrink inImage inSize)
    (gimp-selection-invert inImage)
    (gimp-context-set-background inColor)
    (gimp-edit-fill inLayer BACKGROUND-FILL)
    (gimp-selection-none inImage)
)

(script-fu-register "script-fu-fuzzy-border-a"
    "Fuzzy Border..."
    "Add a jagged, fuzzy border to an image. Do a cool fade to a given colour at the border of an image (optional shadow).\n
Will make image RGB if it isn't already. \nfile:gutteridge-fuzzy-border.scm"
    "Chris Gutteridge"
    "1998, Chris Gutteridge / ECS dept, University of Southampton, England."
    "3rd April 1998"
    "RGB* GRAY*"
    SF-IMAGE        "The image"               0
    SF-DRAWABLE     "The layer"               0
    SF-COLOR        "Color"                  "white"
    SF-ADJUSTMENT   "Border size"            '(16 1 300 1 10 0 1)
    SF-TOGGLE       "Blur border"            TRUE
    SF-ADJUSTMENT   "Granularity (1 is Low)" '(4 1 16 0.25 5 2 0)
    SF-TOGGLE       "Add shadow"             FALSE
    SF-ADJUSTMENT   "Shadow weight (%)"      '(100 0 100 1 10 0 0)
    SF-TOGGLE       "Work on copy"           TRUE
    SF-TOGGLE       "Flatten image"          FALSE
)

(script-fu-menu-register "script-fu-fuzzy-border-a"
                         "<Toolbox>/Script-Fu/Effects")

;end of script