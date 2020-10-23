; GPLv3
; 04/09/2020 to work on GIMP-2.10.20
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


(define (script-fu-crayon-pixel-art theImage drawable dithertype scale_factor scale_back interpolation chosen_palette autolevels)
    
    (let* (
            ; calls to PDB functions always return a list. We have
            ; to pick the first element with "car" explicitely, even
            ; when the function called returns just one value.
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (new_width (* width scale_factor))
            (new_height (* height scale_factor))
            (InputImageMode 0);mode of the input image. needed so we can prevent errors when getting an RGB image as input.
        )
        
        
        ;create undo group
        (gimp-image-undo-group-start theImage)
        
        ; (gimp-image-scale-full theImage (* (gimp-image-width theImage) scale_factor) (* (gimp-image-height theImage) scale_factor) 0);can't do this :(
        
        (set! InputImageMode ( car (gimp-image-base-type theImage )))
        (if (not (= InputImageMode 0))
            (gimp-convert-rgb theImage)
        );convert to RGB if the image isn't already an RGB image
        
        ; check if the image is going to be scaled before scaling.
        (if (not (= new_width 0))
            (gimp-image-scale theImage new_width new_height)
        )
        
        ;apply autoleveling if desired
        ;this is pretty useless if the image is a gif as typically pure white and pure black are two of the colors in the palette so this does nothing in those cases.
        (if (= autolevels TRUE)
            (gimp-drawable-levels-stretch drawable)
        );FML I kept getting an error here bc it can run on an IMAGE yet it really wants a DRAWABLE;theImage))
        
        ;apply dither
        (if (= dithertype 1)
            (gimp-drawable-set-name drawable "FloydSteinbergDither")
        )
        (if (= dithertype 2)
            (gimp-drawable-set-name drawable "FloydSteinbergLowBleedDither")
        )
        (if (= dithertype 3)
            (gimp-drawable-set-name drawable "FixedDither")
        )
        (gimp-image-convert-indexed theImage dithertype 4 0 FALSE FALSE chosen_palette)
        
        (if (= scale_back TRUE)
            (gimp-image-scale theImage width height)
        )
        ;create undo group
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    ) ; end let
) ;; end of define  

;I was receiving a "eval unbound variable error" because I had mistyped my variable
;I was receiving an "illegal token" error because I had deleted the "let" statement and didn't realize that it had a matching parens way at the end of the script before we register the script ...this also might manifest as a "mismatched or unmatched parens" error

(script-fu-register "script-fu-crayon-pixel-art"
    "Crayon Pixel Art"
    "Processes an image creating a pixelized image given a palette. Creates pixilized image. \nfile:CrayonPixelArt_02.scm"
    "InkyDinky"
    "InkyDinky"
    "2011 11 28"
    "*"
    SF-IMAGE        "Image"     0
    SF-DRAWABLE     "Drawable"  0
    ;SF-ENUM "Dither" '("DitherType" "GIMP_FS_DITHER") google gimp enum types
    SF-OPTION       "Dither"            '("None" "Floyd-Steinberg" "Floyd-Steinberg Low Bleed" "Fixed")
    SF-ADJUSTMENT   "Scale Factor"      '(1 1 5 1 2 0 SF-SLIDER)
    SF-TOGGLE       "Scale back to orig size"       TRUE
    SF-OPTION       "Interpolation"     '("None" "Linear" "Cubic" "Sinc (Lanczos3)");I wish I knew how to use the enum types but not worth the hassle when can use options and works right away. http://developer.gimp.org/api/2.0/libgimpbase/libgimpbase-gimpbaseenums.html
    SF-PALETTE      "Palette"            "Ega"
    SF-TOGGLE       "Auto-level image?"  TRUE
)

(script-fu-menu-register "script-fu-crayon-pixel-art" "<Image>/Script-Fu/Photo/Graphic/")

; end of file