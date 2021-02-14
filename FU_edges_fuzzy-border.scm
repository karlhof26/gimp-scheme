; FU_edges_fuzzy-border.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10

; Do a cool fade to a given colour at the border of an image 
; (optional shadow.) Will make image RGB if it isn't already.
; 02/14/2014 - convert to RGB if needed
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8/10
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   C:\Users\YOUR-NAME\.gimp-2.10\scripts
;   C:\Users\YOUR-NAME\AppData\Roaming\GIMP\2.10\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   C:\Users\YOUR-NAME\.gimp-2.10\scripts
;   C:\Users\YOUR-NAME\AppData\Roaming\GIMP\2.10\scripts
;    
;   Linux
;   /home/yourname/.gimp-2.8/scripts  
;   or
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
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
; Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England
;
; tweaked for GIMP-2.4.x by Paul Sherman 10/24/2007
; later moved menu location
; edited again for GIMP 2.6 - 11/20/2008
; edited again for Gimp 2.10.18 - 05/23/2020
;==============================================================
(define (chris-color-edge inImage inLayer inColor inSize)
    (gimp-selection-all inImage)
    (gimp-selection-shrink inImage inSize)
    (gimp-selection-invert inImage)
    (gimp-context-set-background inColor)
    (gimp-edit-fill inLayer FILL-BACKGROUND)
    (gimp-selection-none inImage)
)

(define (FU-edges-fuzzy-border 
        inImage
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
        
    (gimp-image-undo-group-start inImage)
    (if (not (= RGB (car (gimp-image-base-type inImage))))
            (gimp-image-convert-rgb inImage)
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
        ;(gimp-message "line 111")
        
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
        
        (gimp-image-select-item theImage CHANNEL-OP-REPLACE theLayer)
        (gimp-selection-invert theImage)
        (gimp-edit-clear theLayer)
        (gimp-selection-invert theImage)
        (gimp-edit-clear theLayer)
        (gimp-context-set-background inColor)
        (gimp-edit-fill theLayer FILL-BACKGROUND)
        (gimp-selection-none inImage)
        (chris-color-edge theImage theLayer inColor 1)
        ;(gimp-message "line 138")
        
        (if (= inBlur TRUE)
            (plug-in-gauss-rle TRUE theImage theLayer inSize TRUE TRUE)
        )
        (if (= inShadow TRUE)
            (begin
                ;(gimp-message "line 145")
                (gimp-selection-none inImage)
                (gimp-image-insert-layer theImage
                                (car (gimp-layer-copy theLayer FALSE)) 0 0)
                (gimp-layer-scale theLayer
                            (- theWidth inSize) (- theHeight inSize) TRUE)
                (gimp-drawable-desaturate theLayer DESATURATE-LIGHTNESS)
                ;(gimp-brightness-contrast theLayer 127 127)
                (gimp-drawable-brightness-contrast theLayer 0.5 0.5)
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
        
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush)
        ;(gimp-message "Good finish OK")
    )
)



(script-fu-register "FU-edges-fuzzy-border"
    "Fuzzy Border"
    "Add a jagged, fuzzy border to an image. \n file:FU_edges_fuzzy-border.scm"
    "Chris Gutteridge"
    "1998, Chris Gutteridge / ECS dept, University of Southampton, England."
    "3rd April 1998"
    "*"
    SF-IMAGE        "The image"               0
    SF-DRAWABLE     "The layer"               0
    SF-COLOR        "Color"                     "white"
    SF-ADJUSTMENT   "Border size"               '(16 1 300 1 10 0 1)
    SF-TOGGLE       "Blur border"               TRUE
    SF-ADJUSTMENT   "Granularity (1 is Low)"    '(4.0 1.0 16.0 0.25 2.5 2 0)
    SF-TOGGLE       "Add shadow"                FALSE
    SF-ADJUSTMENT   "Shadow weight (%)"         '(100 0 100 1 10 0 0)
    SF-TOGGLE       "Work on copy"              TRUE
    SF-TOGGLE       "Flatten image"             FALSE
)

(script-fu-menu-register "FU-edges-fuzzy-border" "<Toolbox>/Script-Fu/Edges")

; end of script