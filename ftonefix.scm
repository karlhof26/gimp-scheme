; GPL v3
;----------------------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or 
; modify it under the terms of the GNU General Public License   
; as published by the Free Software Foundation; either version 3
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-tone-fix theImage theLayer lowInt intInterval highInt)
    
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)
    
    ;Auto level base image
    (gimp-levels-stretch theLayer)
    
    (let*
        (
            (originalImage (car (gimp-layer-copy theLayer 0)))
            (layerCopy)
            (layerCopyMask)
            (layerInt)
            (merged)
        )
        
        ;start with lowest intensity
        (set! layerInt (/ lowInt 100))
        ;(gimp-levels theLayer 0 0 255 layerInt 0 255)
        (gimp-drawable-levels theLayer 0 0.0 1.0 0 layerInt 0.0 1.0 0)
        
        (set! lowInt (+ lowInt intInterval))
        (while (< lowInt highInt)
            ;process layer
            (set! layerCopy (car (gimp-layer-copy originalImage 0)))
            (gimp-image-insert-layer theImage layerCopy 0 -1)
            (gimp-drawable-levels-stretch layerCopy)
            (set! layerInt (/ lowInt 100))
            (gimp-drawable-levels layerCopy HISTOGRAM-VALUE 0.0 1.0 TRUE layerInt 0.0 1.0 TRUE)
            (set! layerCopyMask (car (gimp-layer-create-mask layerCopy ADD-MASK-COPY)))
            (gimp-layer-add-mask layerCopy layerCopyMask)
            (gimp-drawable-invert layerCopyMask FALSE)
            (set! lowInt (+ lowInt intInterval))
            (set! merged (car (gimp-image-merge-down theImage layerCopy 0)))
            (gimp-image-flatten theImage)
        )
        
        ;Finish the undo group for the process
        (gimp-image-undo-group-end theImage)
        
        ;Ensure the updated image is displayed now
        (gimp-displays-flush)
        
    )
)

(script-fu-register "script-fu-tone-fix"
            "Tone Fix..."
            "Performs pseudo tone fixing. \nfile:ftonefix.scm"
            "Fidelito Fernandez"
            ""
            "20080129"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-ADJUSTMENT    "Lowest Intensity"     '(20 10 100 1 10 1 0)
            SF-ADJUSTMENT    "Intervals"            '(10 5 30 1 10 1 0)
            SF-ADJUSTMENT    "Highest Intensity"    '(170 100 300 1 10 1 0))

(script-fu-menu-register "script-fu-tone-fix" "<Image>/Script-Fu/Enhance")

; end of script