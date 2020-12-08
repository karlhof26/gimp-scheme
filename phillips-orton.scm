; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Orton Effect script  for GIMP 2.4
; Copyright (C) 2007 Harry Phillips <script-fu@tux.com.au>
;
; Tags: photo, orton, effect
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;  Version 1.3 (5th August 2007)
;    - Added GPL3 licence
;    - Menu location at the top of the script
;    - Removed the "script-fu-menu-register" section
;
;  Version 1.2
;    - Made the script compatible with GIMP 2.3
;
;  Version 1.3 
;    - Made the script compatible with GIMP 2.10.22
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-orton-effect     theImage
                theLayer
                blurAmount
                20on
                opacityLevel
    )
    
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)
    
    ;Select none 
    (gimp-selection-none theImage)
    
    ;Initiate some variables
    (let* (
            (sharp (car (gimp-layer-copy theLayer 0)))
            (sharp-screen (car (gimp-layer-copy theLayer 0)))
            (imageWidth 0)
            (imageHeight 0)
            (merged 0)
            (sharpCopy 0)
        )
         
        ;Read the image width and height
        (set! imageWidth (car (gimp-image-width theImage)))
        (set! imageHeight (car (gimp-image-height theImage)))
        
        
        ;Add the first layer to the image
        (gimp-image-insert-layer theImage sharp 0 0)
        
        ;Rename the layer
        (gimp-drawable-set-name sharp "Sharp")
        
        ;Add the second layer to the image
        (gimp-image-insert-layer theImage sharp-screen 0 0)
        
        ;Change the mode of the second layer
        (gimp-layer-set-mode sharp-screen LAYER-MODE-SCREEN-LEGACY)
        
        ;Merge the top layer down and keep track of the newly merged layer
        (set! merged (car (gimp-image-merge-down theImage sharp-screen 0)))
        
        ;Copy the merged layer
        (set! sharpCopy (car (gimp-layer-copy merged 0)))
        
        ;Add the copy of the merged layer
        (gimp-image-insert-layer theImage sharpCopy 0 0)
        
        ;Rename the copied layer
        (gimp-drawable-set-name sharpCopy "Out of focus")
        
        
        ;Setblur factor to 20% of the longest side
        (if (= 20on TRUE)
            (begin
                (if (> imageHeight imageWidth)
                    (set! blurAmount (/ imageWidth 5))
                    (set! blurAmount (/ imageHeight 5))
                )
            )
        )
        
        ;Apply the blur with the supplied blur amount
        (plug-in-gauss 1 theImage sharpCopy blurAmount blurAmount 0)
        
        ;Change the mode of the sharpCopy
        (gimp-layer-set-mode sharpCopy 3)
        
        ;Change the sharpCopy layers opacity 
        (gimp-layer-set-opacity sharpCopy opacityLevel)
        
        ;Finish the undo group for the process
        (gimp-image-undo-group-end theImage)
        
        ;Ensure the updated image is displayed now 
        (gimp-displays-flush) 
        
        
    )
)


(script-fu-register "script-fu-orton-effect"
    "<Image>/Script-Fu/Effects/Orton effect..."
    "Gives the Orton effect on a photo. \nfile:phillips-orton.scm"
    "Harry Phillips"
    "Harry Phillips"
    "Feb. 07 2006"
    "*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-ADJUSTMENT   "Blur"         '(100 5 10000 1 10 0 1)
    SF-TOGGLE       "Use 20% of the shortest side for Blur"       FALSE
    SF-ADJUSTMENT   "Opacity"      '(90 0 100 1 10 1 0)
)

;end of script