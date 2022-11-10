; FU_edges_art-border.scm
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 02/09/2020 on GIMP-2.10.22 
; 
; Last update reintroduced a modified drop-shadow effect 
; to give slight "dark glow" to inner border. This is optional. 
; Also pulled the flatten image on start, so you can retain an 
; alpha layer image if present.
; Dropped remnant selection when finished,
; changed menu location, added version for reference.
; convert to RGB if needed.
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
; Do a cool fade to a given colour at the border of an image (optional shadow)
; Will make image RGB if it isn't already.
;
; Chris Gutteridge (cjg@ecs.soton.ac.uk)
; At ECS Dept, University of Southampton, England.
;
; Modified for use in GIMP-2.4+ by Paul Sherman
; distributed by gimphelp.org
; improved UNDO while testing for GIMP-2.6
; last modified 10/02/2008
;==============================================================


(define (FU-art-border
                inImage
                inLayer
                inSize
                inMiniSize
                inCopy
                inDesaturate
                inShadow
                inInvert
                inBlur
    )
    (let* (
            (dummy 0)
            (theImage (if (= inCopy TRUE)
                (car (gimp-image-duplicate inImage))
                        inImage))
            (dropamount 2)
          )
        (gimp-image-undo-group-start inImage)
        (if (= inCopy FALSE) 
            (begin
                (gimp-image-undo-group-start inImage)
                (if (not (= RGB (car (gimp-image-base-type inImage))))
                    (gimp-image-convert-rgb inImage))		 
            )
        )
        
        ;(define theImage (if (= inCopy TRUE)
        ;        (car (gimp-image-duplicate inImage))
        ;                inImage))
        (define theWidth (car (gimp-image-width inImage)))
        (define theHeight (car (gimp-image-height inImage)))
        (define theLayer (car (gimp-image-get-active-layer theImage)))
        
        ;(gimp-message "start ok")
        (if (not (= RGB (car (gimp-image-base-type theImage))))
            (gimp-image-convert-rgb theImage)
        )
        (gimp-selection-all theImage)
        (gimp-selection-shrink theImage inSize)
        (gimp-selection-invert theImage)
        
        (if  (= inDesaturate TRUE)
            (begin
                (gimp-drawable-desaturate theLayer DESATURATE-LIGHTNESS)
            )
        )
        
        (if  (= inInvert TRUE)
            (begin
                (gimp-drawable-invert theLayer FALSE)
            )
        )
        
        (if  (= inBlur TRUE)
            (begin
                (plug-in-blur 1 theImage theLayer)
            )
        )
        ;(gimp-message "middle ok")
        (gimp-selection-invert theImage)
        (if (> inMiniSize 0)
            (begin
                (gimp-selection-border theImage inMiniSize) ; was 1
                
                (gimp-edit-fill theLayer 0)
            )
        )
        (if  (= inShadow TRUE)
            (begin
                ;(gimp-message "inshadow True")
                (set! dropamount (round (* inSize 0.5)))
                (if (> dropamount 15)
                    (begin
                        ;(gimp-message "dropamount 15")
                        (set! dropamount 15)
                    )
                )
                ;(gimp-message (number->string dropamount))
                ;(gimp-selection-invert theImage)
                
                
                (if (> inMiniSize 0)
                    (begin
                        (gimp-selection-border theImage inMiniSize) ; was 1
                        ;(gimp-message "start drop-shad")
                        (script-fu-drop-shadow theImage theLayer 6 6 dropamount '(0 0 0) 70 TRUE)
                        ;(script-fu-drop-shadow theImage theLayer 0 0 8 '(0 0 0) 70 TRUE)
                        ;(gimp-message "end drop-shad")
                    )
                    (begin
                        (gimp-selection-border theImage 2)
                        (script-fu-drop-shadow theImage theLayer 6 6 dropamount '(0 0 0) 70 TRUE)
                    )
                )
                
                
            )
        )
        
        (if  (= inCopy TRUE)
            (begin
                (gimp-image-clean-all theImage)
                (gimp-selection-none theImage)
                (gimp-display-new theImage)
            )
            ()
        )
        (gimp-selection-none inImage)
        (gimp-displays-flush)
        (if (= inCopy FALSE) 
            (begin
                (gimp-selection-none inImage)
                (gimp-image-undo-group-end inImage)
            )
        )
        ;(gimp-message "Good finish OK")
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush) 
    )
)

(script-fu-register "FU-art-border"
    "<Toolbox>/Script-Fu/Edges/Art Border"
    "Add art border to the image. Uses the foreground color for the thin mini border. \nfile:FU_edges_art-border.scm"
    "Antony Dovgal"
    "2006, Antony Dovgal. This script is based on 'fuzzy border' by Chris Gutteridge"
    "21 April 2006"
    "*"
    SF-IMAGE       "The image"                      0
    SF-DRAWABLE    "The layer"                      0
    SF-ADJUSTMENT  "Border size"                    '(30 1 300 1 10 0 1)
    SF-ADJUSTMENT  "Mini Border size"               '(3 0 30 1 10 0 1)
    SF-TOGGLE      "Work on copy"                   FALSE
    SF-TOGGLE      "Desaturate border"              TRUE
    SF-TOGGLE      "Lightly Shadow Inner Border"    FALSE
    SF-TOGGLE      "Invert border colors"           FALSE
    SF-TOGGLE      "Blur border"                    FALSE
)

; end of script