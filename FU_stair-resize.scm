; FU_stair-resize.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10 
;
; 02/15/2014 - added a final size correction, accommodated indexed images,
; cleaned code (pulling unused variables)
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
; Stair re-size is a script for The GIMP
; Resizes the image up or down in multiple steps instead of one.
; The script is located in menu 
; "<Image> / Script-Fu / Misc / Stair Interpolation..."
; Last changed: 13 August 2007
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;==============================================================


(define (step-size stepNumber wantedSize currentSize)
    
    (let* (
            (stepSize (/ (- wantedSize currentSize) stepNumber))
        )
        
        ;Return stepSize
        stepSize
    )
)


(define (FU-stair-resize
        theImage
        theLayer
        targetSide
        targetValue
        stepsWanted
    )
    
    (let* (
            
            ;Read the image width and height
            (imageWidth (car (gimp-image-width theImage)))
            (imageHeight (car (gimp-image-height theImage)))
            (sizeList)
            (realWidth)
            (realHeight)
            (nextWidth)
            (nextHeight)
            (stepsX)
            (stepsY)
        )
        
        ;Start an undo group so the process can be undone with one undo
        (gimp-image-undo-group-start theImage)
        ; convert indexed to RGB, if needed
        (define indexed (car (gimp-drawable-is-indexed theLayer)))
        (if (= indexed TRUE)(gimp-image-convert-rgb theImage))
        
        ;Select none
        (gimp-selection-none theImage)
        
        ;Calculate the required step size
        (if (= targetSide 0)
            ;True width is the target
            (begin
                (set! stepsX (step-size stepsWanted targetValue imageWidth))
                (set! realWidth (+ (* stepsX stepsWanted) imageWidth))
                (set! realHeight (/ (* imageHeight  realWidth) imageWidth))
                (set! stepsY (step-size stepsWanted realHeight imageHeight))
            )
            
            ;False the height is the target
            (begin
                (set! stepsY (step-size stepsWanted targetValue imageHeight))
                (set! realHeight (+ (* stepsY stepsWanted) imageHeight))
                (set! realWidth (/ (* imageWidth  realHeight) imageHeight))
                (set! stepsX (step-size stepsWanted realWidth imageWidth))
            )
        )
        
        ;Set the first resize values
        (set! nextWidth (+ imageWidth stepsX))
        (set! nextHeight (+ imageHeight stepsY))
        
        ;Change the image size by a step at a time
        (while (> stepsWanted 0)
            (gimp-image-scale theImage nextWidth nextHeight)
            (set! stepsWanted (- stepsWanted 1))
            (set! nextWidth (+ nextWidth stepsX))
            (set! nextHeight (+ nextHeight stepsY))
        )
        
        ;Final size correction
        (if (= targetSide 0)
            (gimp-image-scale theImage targetValue realHeight)
        )
        (if (= targetSide 1)
            (gimp-image-scale theImage realWidth targetValue)
        )	
        
        ;Finish the undo group for the process
        (gimp-image-undo-group-end theImage)
        
        ;Ensure the updated image is displayed now
        (gimp-displays-flush)
    )
)

(script-fu-register "FU-stair-resize"
    "<Image>/Script-Fu/Photo/Enhancement/Step Resize"
    "Resizes the image to desired size using small steps.\n\nResizing in small increments can often produce clearer results... most noticeably in less jagged-looking edges. \nfile:FU_stair-resize.scm"
    "Harry Phillips"
    "Harry Phillips"
    "13 August 2007"
    "*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Drawable"          0
    SF-OPTION       "Target side"       '("Width" "Height")
    SF-VALUE        "Target value"      "1024"
    SF-ADJUSTMENT   "Numbers of steps"  '(10 2 20 1 1 0 0)
)           

; end of script