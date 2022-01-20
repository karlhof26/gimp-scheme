; GPLv3
; 02/15/2014 - work with non-rgb, merge option and install info added
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

(define (script-fu-crayon-pixel-art-all-dither theImage drawable scale_factor interpolation chosen_palette autolevels keepscaled)
    ;NOTES:
    ;for some reason it doesn't work on PNGs. currently you need to save as a gif to get it to work.
    ;if it throws an error on a jpg bc it wasn't in indexed mode you may need to close gimp and reopen to clean up any variables that will prevent the successful execution of the script on subsequent attempts
    (let* (
            ; calls to PDB functions always return a list. We have
            ; to pick the first element with "car" explicitely, even
            ; when the function called returns just one value.
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (new_width (* width scale_factor ))
            (new_height (* height scale_factor))
            
            (FSDither 0); (gimp-image-new new_width new_height 0));0 for rgb
            (FSLBDither 0); (gimp-image-new new_width new_height 0));0 for rgb
            (FixedDither 0); (gimp-image-new new_width new_height 0));0 for rgb
            (NoDither 0)
            (PaletteGen 0)
            (adrawable 0);
            (InputImageMode 0);mode of the input image. needed so we can prevent errors when getting an RGB image as input.
            (scale_back TRUE)
          )
        
        
        ;create undo group
        (gimp-image-undo-group-start theImage)
        
        
        
        (set! InputImageMode ( car (gimp-image-base-type theImage )))
        (if (not (= InputImageMode 0))
            (gimp-convert-rgb theImage)
        );convert to RGB if the image isn't all ready an RGB image
        
        ;apply autoleveling if desired
        ;this is pretty useless if the image is a gif as typically pure white and pure black are two of the colors in the palette so this does nothing in those cases.
        (if (= autolevels TRUE)
            (begin
                (set! adrawable ( car (gimp-image-get-active-drawable theImage)))
                (gimp-drawable-levels-stretch adrawable) ;;
            )
            (begin ; else
                (set! adrawable ( car (gimp-image-get-active-drawable theImage)))
            )
        )
        
        ;Scale the Image to blow up the gif so we can see it. this helps to allow for a more pleasing picture to dither. if the image is too small the dithering is awful
        (gimp-image-scale theImage new_width new_height);
        
        ;Create NO DITHER image
        (gimp-edit-named-copy-visible theImage "ImgVisible")
        (set! NoDither ( car (gimp-edit-named-paste-as-new "ImgVisible")))
        ;(gimp-display-new NoDither)
        (gimp-image-convert-indexed NoDither 0 4 0 FALSE FALSE chosen_palette )
        (set! adrawable ( car (gimp-image-get-active-drawable NoDither)))
        (gimp-drawable-set-name adrawable "No Dither")
        (if (= keepscaled FALSE)
            (gimp-image-scale NoDither width height)
        )
        (gimp-display-new NoDither)
        
        ;Create FS DITHER image
        (gimp-edit-named-copy-visible theImage "ImgVisible")
        (set! FSDither ( car (gimp-edit-named-paste-as-new "ImgVisible")))
        ;(gimp-display-new FSDither)
        (gimp-image-convert-indexed FSDither 1 4 0 FALSE FALSE chosen_palette )
        (set! adrawable ( car (gimp-image-get-active-drawable FSDither)))
        (gimp-drawable-set-name adrawable "FS Dither")
        (if (= keepscaled FALSE)
            (gimp-image-scale FSDither width height)
        )
        (gimp-display-new FSDither)
        
        ;Create FS Low Bleed DITHER image
        (gimp-edit-named-copy-visible theImage "ImgVisible")
        (set! FSLBDither ( car (gimp-edit-named-paste-as-new "ImgVisible")))
        ;(gimp-display-new FSLBDither)
        (gimp-image-convert-indexed FSLBDither 2 4 0 FALSE FALSE chosen_palette )
        (set! adrawable ( car (gimp-image-get-active-drawable FSLBDither)))
        (gimp-drawable-set-name adrawable "FSLB Dither")
        (if (= keepscaled FALSE)
            (gimp-image-scale FSLBDither width height)
        )
        (gimp-display-new FSLBDither)
        
        ;Create FIXED DITHER image
        (gimp-edit-named-copy-visible theImage "ImgVisible")
        (set! FixedDither ( car (gimp-edit-named-paste-as-new "ImgVisible")))
        ;(gimp-display-new FixedDither)
        (gimp-image-convert-indexed FixedDither 3 4 0 FALSE FALSE chosen_palette )
        (set! adrawable ( car (gimp-image-get-active-drawable FixedDither)))
        (gimp-drawable-set-name adrawable "Fixed Dither")
        (if (= keepscaled FALSE)
            (begin
                (gimp-image-scale FixedDither width height)
            )
        )
        (gimp-display-new FixedDither)
        
        ;Create Generated Palette image
        (gimp-edit-named-copy-visible theImage "ImgVisible")
        (set! PaletteGen ( car (gimp-edit-named-paste-as-new "ImgVisible")))
        ;(gimp-display-new FixedDither)
        (gimp-image-convert-indexed PaletteGen 0 0 8 TRUE FALSE "GeneratedPaletteCrayonPixelArt" )
        (set! adrawable ( car (gimp-image-get-active-drawable PaletteGen)))
        (gimp-drawable-set-name adrawable "PaletteGen")
        (if (= keepscaled FALSE)
            (begin
                (gimp-image-scale PaletteGen width height)
            )
        )
        (gimp-display-new PaletteGen)
        
        
        ; empty the buffer
        (gimp-buffer-delete "ImgVisible")      
        (gc)
        
        ;create undo group
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush)
    )

)

(script-fu-register "script-fu-crayon-pixel-art-all-dither"
    "Caryon-Pixel-Art-All-Dither"
    "Processes an indexed color JPG/GIF (doesnt work on pngs) image creating a pixelized image given a palette. \n Creates 5 images so results can be compared. Final version generates a palette. \nfile:CrayonPixelArtAllDitherTypes_02.scm"
    "InkyDinky"
    "InkyDinky"
    "2011 11 28"
    "*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    ;SF-ENUM "Dither" '("DitherType" "GIMP_FS_DITHER") google gimp enum types
    ; SF-OPTION "Dither" '("None" "Floyd-Steinberg" "Floyd-Steinberg Low Bleed" "Fixed")
    SF-ADJUSTMENT   "Scale Factor"        '(2 0 10 1 2 0 SF-SLIDER )
    SF-OPTION       "Interpolation"       '("None" "Linear" "Cubic" "Sinc (Lanczos3)");I wish I knew how to use the enum types but not worth the hassle when can use options and works right away. http://developer.gimp.org/api/2.0/libgimpbase/libgimpbase-gimpbaseenums.html
    SF-PALETTE      "Palette"             "Ega"
    SF-TOGGLE       "Auto-level image?"   TRUE
    SF-TOGGLE       "Keep scaled up versions" FALSE
)

(script-fu-menu-register "script-fu-crayon-pixel-art-all-dither" "<Image>/Script-Fu/Photo/Graphic/")

; end of file