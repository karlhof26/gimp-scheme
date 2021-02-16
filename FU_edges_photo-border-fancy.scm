; FU_edges_photo-border-fancy.scm 
; version 2.8 [gimphelp.org] 
; 2nd last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
;
; recent updates:
; convert to RGB if needed, option to merge layer upon script completion
; last modified/tested by karlhof26
; 05/29/2020 on GIMP-2.10.18
;;==============================================================
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
; NOT AVAILABLE
;--------------------------------------------------------------------
(define (FU-photo-border 
        image 
        photo-layer 
        border 
        lower 
        shadow  
        border-color
        border-shading-feather 
        border-shading-transparency 
        border-shading-color
        text-string 
        text-font 
        text-height
        inFlatten
        )
    (let* (    
            (w (car (gimp-image-width image)))
            (h (car (gimp-image-height image)))
            (th (/ (* h text-height) 100))
            (bw (/ (* w border) 100))
            (lh (/ (* w lower) 100))
            (sw (/ (* w shadow) 100))
            (bsw (/ (* w border) 400))
            (bsf (/ (* w border-shading-feather) 100))
            (with-border-width (+ bw (+ w bw)))
            (with-border-height (+ bw (+ h (+ bw lh))))
            
            (text-layer #f)
            (border-shading-layer #f)
            (border-layer 0)
          )
        ;   (gimp-context-push)
        (gimp-image-undo-group-start image)
        (if (not (= RGB (car (gimp-image-base-type image))))
            (gimp-image-convert-rgb image)
        )
        (gimp-layer-add-alpha photo-layer)
        ;make room for border
        (gimp-image-resize image with-border-width with-border-height bw bw)

        (set! border-layer (car 
            (gimp-layer-new image with-border-width with-border-height 0 "Photo-Border" 100 LAYER-MODE-NORMAL)
                ))
        (gimp-image-insert-layer image border-layer 0 1)
        (gimp-context-set-foreground border-color)
        (gimp-drawable-fill border-layer FILL-FOREGROUND) 
        
        (unless (= border-shading-feather 0)
            (set! border-shading-layer (car
                (gimp-layer-new image with-border-width with-border-height 0 "Photo-Border-Shadow"
                    border-shading-transparency LAYER-MODE-NORMAL)
                )
             )
            (gimp-image-insert-layer image border-shading-layer 0 -1)
            (gimp-context-set-foreground border-shading-color)
            (gimp-drawable-fill border-shading-layer 0)
            (gimp-image-set-active-layer image border-shading-layer)
            (gimp-image-select-rectangle image CHANNEL-OP-REPLACE bsw bsw
                (- with-border-width (* 2 bsw)) (- with-border-height (* 2 bsw)))
            (gimp-edit-clear border-shading-layer)
            (gimp-selection-none image)
            (gimp-image-set-active-layer image photo-layer)
        )
        
        (unless (= lh 0)
            ; Polaroid style has shadow around frame (not in the same plane)
            (script-fu-drop-shadow image photo-layer 0 0 (/ bw 5) '(0 0 0) 80 1)
        )
        (unless (= sw 0)
            (script-fu-drop-shadow image border-layer (/ sw 4) (/ sw 4) sw '(0 0 0) 80 1)
        )
        
        (unless (= (string-length text-string) 0)
            (set! text-layer 
                (gimp-text-fontname image border-layer (* bw 1.2) (+ bw h (/ (- (+ bw lh) th) 2)) text-string 0 TRUE th PIXELS text-font) ; (/ (- (+ bw lh) th) 1.5)
            )
        )
        (if (= inFlatten TRUE)(gimp-image-flatten image))
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
    )
)
  
(script-fu-register 
    "FU-photo-border"
    "<Toolbox>/Script-Fu/Decor/Photo Border Fancy"
    "Add white border to photo.\n
    The extra lower border is for a 'Polaroid' effect,  
        in which case a caption may also be added.\n
        A drop shadow further adds the effect
        of a photo placed on a background.
        \nfile:FU_edges_photo-border-fancy.scm
    "
    "Egil Kvaleberg"
    "copyright 2009, Egil Kvaleberg"
    "4 May, 2009"
    "*"
    SF-IMAGE        "Image"                                     0
    SF-DRAWABLE     "Layer"                                     0
    SF-ADJUSTMENT   "Border (%)"                                '(7.0 0 20 0.1 1.0 1 0)
    SF-ADJUSTMENT   "Extra Lower Border (0 for none) (%)"       '(21.0 0 100 0.1 1.0 1 0)
    SF-ADJUSTMENT   "Drop Shadow (0 to disable) (%)"            '(3.0 0 50 0.1 1.0 1 0)
    SF-COLOR        "Border Color"                              '(250 250 240)
    SF-ADJUSTMENT   "Border Shading Feather (0 for none) (%)"   '(10.0 0 50 0.1 1.0 1 0)
    SF-ADJUSTMENT   "Border Shading Transparency (%)"           '(12.0 0 100 0.5 5.0 1 0)
    SF-COLOR        "Border Shading Color"                      '(0 0 0)
    SF-STRING       "Caption Text (needs lower border)"         ""
    SF-FONT         "Caption Font" (if (= 0 (car (gimp-fonts-get-list "Comic"))) "Serif Bold" (caadr (gimp-fonts-get-list "Comic")))
    SF-ADJUSTMENT   "Caption Height (%)"                        '(7.0 0 50 0.1 1.0 1 0)
    SF-TOGGLE       "Flatten image when complete?"              FALSE
)

; end of script