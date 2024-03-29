;;;;;;;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Vintage Film Effect script for GIMP 2.4
; Original author: Alexia Death
; 
; Tags: photo, vintage
;
; Author statement:
;
; Based on paint net tutorial by fallout75.
;(http://www.flickr.com/photos/fallout75/)
; This represents my first attempt at gimp scripting and my first ever contact
; with Scheme language. If you feel its not as good as it can be,
; feel free to improve it.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-vintage-effect   inImage
                                    inLayer
                                    inCopy
                                    inFlatten
        )
    
  (let (
        
            (theWidth (car (gimp-image-width inImage)))
            (theHeight (car (gimp-image-height inImage)))
            (theImage 0)
            (base 0)
            (sepia 0)
            (magenta 0)
            (floating-sel 0)
            (control_pts_r (cons-array 10 'byte))
            (control_pts_g (cons-array 8 'byte))
            (control_pts_b (cons-array 4 'byte))
            
        )
        
    (set! theImage  (if (= inCopy TRUE)
                        (car (gimp-image-duplicate inImage))
                        inImage
                    )
    )
    
    (if (= inCopy FALSE)
        (begin
            (gimp-image-undo-group-start theImage)
        )
    )
    
    (if (> (car (gimp-drawable-type inLayer)) 1)
        (gimp-image-convert-rgb theImage)
    )
    
    ; flattning the image at hand into a copy
    (gimp-edit-copy-visible theImage)
    
    ; Making base layer
    (set! base (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGBA-IMAGE
                                        "base"
                                        100
                                        LAYER-MODE-NORMAL)))
    
    (gimp-image-insert-layer theImage base 0 -1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste base TRUE)))
    
    (gimp-drawable-hue-saturation base HUE-RANGE-ALL 0.0 0.0 15.0 0.0) ; was 0 0 15
    (gimp-drawable-brightness-contrast base 0.0 0.083) ; was 0 20
    
    ;(set! control_pts_r #(0 0 88 47 170 188 221 249 255 255))
    (set! control_pts_r #(0.0 0.0 0.345 0.184 0.666 0.737 0.866 0.976 1.0 1.0))
    ;(set! control_pts_g #(0 0 65 57 184 208 255 255))
    (set! control_pts_g #(0.0 0.0 0.254 0.223 0.721 0.815 1.0 1.0))
    ;(set! control_pts_b #(0 29 255 226))
    (set! control_pts_b #(0.0 0.113 1.000 0.886))
    (gimp-drawable-curves-spline base HISTOGRAM-RED 10 control_pts_r)
    (gimp-drawable-curves-spline base HISTOGRAM-GREEN 8 control_pts_g)
    (gimp-drawable-curves-spline base HISTOGRAM-BLUE 4 control_pts_b)
    
    ; making sepia layer
    (set! sepia (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGBA-IMAGE
                                        "sepia"
                                        100
                                        LAYER-MODE-NORMAL)))
    
    (gimp-image-insert-layer theImage sepia 0 -1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste sepia TRUE)))
    
    (gimp-drawable-colorize-hsl sepia 25 25 30)
    (gimp-drawable-brightness-contrast sepia 0.156 0.117) ; was 40 30
    (gimp-layer-set-opacity sepia 50.0)
    
    ; making magenta layer
    (set! magenta (car (gimp-layer-new theImage
                                        theWidth
                                        theHeight
                                        RGBA-IMAGE
                                        "magenta"
                                        100
                                        LAYER-MODE-SCREEN)))
    
    (gimp-image-insert-layer theImage magenta 0 -1)
    
    (gimp-context-push)
    (gimp-context-set-foreground '(255 0 220))
    
    (gimp-drawable-fill magenta FILL-FOREGROUND)
    
    (gimp-layer-set-opacity magenta 6)
    
    (gimp-context-pop)
    
    (if (= inFlatten TRUE)
        (gimp-image-flatten theImage)
    )
    
    (if (= inCopy TRUE)
        (begin
            (gimp-image-clean-all theImage)
            (gimp-display-new theImage)
        )
    )
    (if (= inCopy FALSE)
        (begin
            (gimp-image-undo-group-end theImage)
        )
    )
    
    (gimp-displays-flush)
    (gc) ; arrays were used - memory cleanup
  )
)


(script-fu-register "script-fu-vintage-effect"
    "Vintage Film effect..."
    "Give that vintage look to a photo. \n file:death-vintage-effect.scm"
    "Alexia Death"
    "2007, Alexia Death"
    "3rd October 2007"
    "RGB* GRAY*"
    SF-IMAGE      "The image"               0
    SF-DRAWABLE   "The layer"               0
    SF-TOGGLE     "Work on copy"            FALSE
    SF-TOGGLE     "Flatten image"         FALSE
)

(script-fu-menu-register "script-fu-vintage-effect"
                         "<Toolbox>/Script-Fu/Effects")

; end of script