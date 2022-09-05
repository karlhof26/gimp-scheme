; FU_sketch_quick-sketch.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
;
;  Version 1.0a - 10/14/2008
;		  Modified by Paul Sherman 
;		  in accordence with post by char101 on
;		  http://registry.gimp.org/node/5921
;		  Also changed menu location.
; 10/15/2010 - bumped INDEXED* to prevent errors
; 02/15/2014 - accommodated indexed images properly
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
;	
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
; Quick sketch is a script for The GIMP
; Quick sketch turns a photo into what looks like a artists sketch
; The script is located in "<Image> / Script-Fu / Artistic / Quick sketch..."
; Last changed: 14 June 2008
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;==============================================================


(define (script-fu-karl-quick-sketch  
        theImage
        theLayer
        blurAmount
        )
        
        
        
    ;Initiate some variables
    (let* (
            (layerCopy 0)
            (layerGrey (car (gimp-drawable-is-gray theLayer)))
            (indexed 0)
        )
        
        (gimp-image-undo-group-start theImage)
        (set! indexed (car (gimp-drawable-is-indexed theLayer)))
        (if (= indexed TRUE)(gimp-image-convert-rgb theImage))
        
        ;Rename the layer
        (gimp-item-set-name theLayer "Original")
        
        ;Select none
        (gimp-selection-none theImage)

        ;Change the layer Greyscale if it isn't already
        (if (= layerGrey 0)
            (gimp-drawable-desaturate theLayer DESATURATE-LUMINOSITY)
        )
        (set! layerCopy (car (gimp-layer-copy theLayer 1)))
        
        ;Copy the layer
        (gimp-image-insert-layer theImage layerCopy 0 0)
        
        ;Rename the layer
        (gimp-item-set-name layerCopy "Dodge layer") 
        
        ;Invert the layer
        (gimp-invert layerCopy)
        
        ;Change the layers mode
        (gimp-layer-set-mode layerCopy 16)
        
        ;Blur the dodge layer
        (plug-in-gauss 1 theImage layerCopy blurAmount blurAmount 0)
        
        
        
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-karl-quick-sketch"
    "Sketch Quick sketch"
    "Create a sketch from a photo.\nfile:FU_sketchquick-sketch_02.scm"
    "Harry Phillips"
    "Harry Phillips"
    "Sep. 9 2007"
    "*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Drawable"          0
    SF-ADJUSTMENT   "Blur factor"      '(30 5 200 1 1 1 1)
)

(script-fu-menu-register "script-fu-karl-quick-sketch" "<Image>/Script-Fu/Artist")

;end of script