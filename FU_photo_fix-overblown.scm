; FU_photo_fix-overblown.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/12/2014 on GIMP-2.8.10
;
; No editing except for menu entry involved - 
; Very handy for adjusting and overexposed photo
; without resorting to curves and other tweaking
;
; 10/15/2010 - restricted to RGB to eliminate errors on indexed and gray
; 02/12/2014 - added strength option as well as flatten.
; 02/14/2014 - convert to RGB if needed
; 10/20/2020 - updated to work with GIMP-2.10.20
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
; FixOverblown is a script for Gimp
; This script helps fix overblown areas of an image.
; The script is located in "<Image> / Script-Fu / Photo / Fix Overblown"
; Last changed: 2009 June 18
; Copyright (C) 2009 Jonathan Denning <jon@gfxcoder.us>
;==============================================================


(define (FU-FixOverblown 
        inImage 
        inLayer 
        strength 
        flatten
    )
    
    (gimp-image-undo-group-start inImage)
    (if (not (= RGB (car (gimp-image-base-type inImage))))
            (gimp-image-convert-rgb inImage))
            
    (let*
        (
            (overlayLayer (car (gimp-layer-copy inLayer FALSE)))
            (mask 0)
        )
        (gimp-image-insert-layer inImage overlayLayer 0 -1)
        (gimp-layer-set-mode overlayLayer LAYER-MODE-OVERLAY)
        (gimp-item-set-name overlayLayer "Fix Overblown")
        (set! mask (car (gimp-layer-create-mask overlayLayer ADD-MASK-COPY)))
        (gimp-layer-add-mask overlayLayer mask)
        (plug-in-vinvert RUN-NONINTERACTIVE inImage overlayLayer)
        
        (cond ((= strength 1)
                ;(gimp-curves-spline mask 0 6 #(0 0 128 0 255 255))
                (gimp-drawable-curves-spline mask 0 6 #(0.0 0.0 0.501 0.0 1.0 1.0))
            )
            ((= strength 2)
                ;(gimp-curves-spline mask 0 8 #(0 0 55 27 176 197 255 255))
                (gimp-drawable-curves-spline mask 0 8 #(0.0 0.0 0.215 0.105 0.690 0.772 1.000 1.000))
            )
            ((= strength 3)
                ;(gimp-curves-spline mask 0 16 #(0 0 63 73 95 125 127 31 156 188 191 151 223 227 255 255))
                (gimp-drawable-curves-spline mask 0 16 #(0.0 0.0 0.247 0.282 0.372 0.490 0.498 0.121 0.611 0.737 0.749 0.592 0.874 0.890 1.000 1.000))
            )
        )
        
        (if (= flatten TRUE)(gimp-image-flatten inImage))
        (gimp-image-undo-group-end inImage)
        (gimp-displays-flush)
        (list overlayLayer)
    )
)

(script-fu-register "FU-FixOverblown"
    "<Image>/Script-Fu/Photo/Enhancement/Fix Overblown"
    "Helps fix overblown areas. \nfile:FU_photo_fix-overblown.scm"
    "Jon Denning <jon@gfxcoder.us>"
    "Jon Denning"
    "2009-06-18"
    "*"
    SF-IMAGE        "Image"                 0
    SF-DRAWABLE     "Layer"                 0
    SF-ADJUSTMENT   "Adjustment Strength"   '(1 1 3 1 1 0 0)
    SF-TOGGLE       "Flatten image"         FALSE
)

;end of script