; FU_artist_water-paint-effect.scm 
; version 2.8 [gimphelp.org]
; modified/tested by Paul Sherman
; 02/15/2014 on GIMP-2.8.10
; last modified/tested by karlhof26
; 01/03/2020 on GIMP-2.10.18
;
; 02/15/2014 - work with non-rgb, merge option and install info added
;==============================================================
;
; Installation: 
; This script should be placed in the user or system-wide script folder.
;
;	Windows 10
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.10\scripts
; 
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
;   - Changelog -
; version 0.1  2001/04/15 iccii <iccii@hotmail.com>
;     - Initial relased
; version 0.1a 2001/07/20 iccii <iccii@hotmail.com>
;     - more simple
; version 0.1b Receved as completely broken, doing just gausian blur. Fixed to 
; do something that may have been the authors intent. 
;==============================================================

(define (FU-artist-water-paint-effect
    inImage
    inDrawable
    inEffect
    inMerge
    )
    
  (let*  (  
            (width (car (gimp-drawable-width inDrawable)))
            (height (car (gimp-drawable-height inDrawable)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-selection (car (gimp-selection-save inImage)))
            (theNewlayer 0)   
            (origlayer 0)   
            (nlayer 0)   
            (indexed 0)            ; was (car )
            (img 0)
            )
    
    (gimp-image-undo-group-start inImage) 
    
    (set! indexed (car (gimp-drawable-is-indexed inDrawable)))
    ;;(define indexed (car (gimp-drawable-is-indexed inDrawable))) 
    (if (= indexed TRUE)
        (begin
            (gimp-image-convert-rgb inImage)
        )
    )
    
    (set! theNewlayer (car (gimp-layer-copy inDrawable FALSE)))
    (set! origlayer (car (gimp-layer-copy inDrawable FALSE)))
    (gimp-image-insert-layer inImage theNewlayer 0 -1)
    (gimp-layer-set-mode theNewlayer LAYER-MODE-VIVID-LIGHT) 
    
    (plug-in-gauss-iir2 1 inImage inDrawable inEffect inEffect)
    ;;(gimp-image-insert-layer inImage theNewlayer 0 -1)
    (plug-in-sobel 1 inImage theNewlayer TRUE TRUE TRUE) ; was laplace
    (gimp-image-set-active-layer inImage theNewlayer)
    (gimp-drawable-levels-stretch theNewlayer)
    (plug-in-vinvert 1 inImage theNewlayer)
    ;(gimp-layer-set-mode theNewlayer LAYER-MODE-DIFFERENCE) 
    ;;(gimp-image-merge-down inImage theNewlayer EXPAND-AS-NECESSARY)
    
    (set! nlayer (car (gimp-image-get-active-layer inImage)))
    (gimp-image-insert-layer inImage origlayer 0 -1)
    (gimp-image-lower-item-to-bottom inImage origlayer)
    (gimp-image-set-active-layer inImage nlayer)
    (gimp-item-set-name nlayer "Watercolor Layer")
    
    (if (= inMerge TRUE)
        (gimp-image-merge-visible-layers inImage EXPAND-AS-NECESSARY)
    )
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
  )
)

(script-fu-register "FU-artist-water-paint-effect"
    "<Toolbox>/Script-Fu/Artist/WaterColor"
    "Draw with water paint effect \n file:FU-artist_water-paint-effect.scm"
    "Iccii <iccii@hotmail.com> & karlhof26"
    "Iccii"
    "March 2020"
    "*"
    SF-IMAGE        "Image"                         0
    SF-DRAWABLE     "Drawable"                      0
    SF-ADJUSTMENT   "Effect Size (pixels)"          '(5 0 32 1 1 0 0)
    SF-TOGGLE       "Merge layers when complete?"   FALSE
)

; end of script