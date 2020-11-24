; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Bercovich LOMO script  for GIMP 2.4
; Copyright (C)March 2003 under GPL by Avi Bercovich <avi@sillypages.org>
;
; Tags: photo, lomo
;
; Author statement:
;
; A Script-Fu script to turn a GIMP-image into a 'LOMO' snap.
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
;  23/11/2020 - Updated for Gimp-2.10.22
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-lomo-berco image drawable fringeWidth fringeBoost)
    
    (let*
     (
        (imgWidth (car (gimp-image-width image)))
        (imgHeight (car (gimp-image-height image)))
        
        (midpoint_X (/ imgWidth 2))
        (midpoint_Y (/ imgHeight 2))
        
        (l_whiteFlare (car (gimp-layer-new image imgWidth imgHeight RGBA-IMAGE "White flare" 100 0 )))
        (l_fringe (car (gimp-layer-new image imgWidth imgHeight RGBA-IMAGE "frame" 100 LAYER-MODE-NORMAL )))
     )
        
        
        (gimp-image-undo-group-start image)
        
        ; adjust colours
        (gimp-color-balance drawable TRANSFER-SHADOWS TRUE 30 0 0)
        ;(gimp-brightness-contrast drawable 0 30)
        (gimp-drawable-brightness-contrast drawable 0.0 0.1176)
        
        ; draw whiteFlare layer
        (gimp-drawable-fill l_whiteFlare FILL-TRANSPARENT)
        (gimp-image-insert-layer image l_whiteFlare 0 0)
        (gimp-context-set-foreground '(255 255 255) )
        (gimp-edit-blend l_whiteFlare 2 LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE FALSE 0 0 FALSE midpoint_X midpoint_Y imgWidth imgHeight)
        (gimp-layer-set-mode l_whiteFlare LAYER-MODE-OVERLAY)
        (gimp-layer-set-opacity l_whiteFlare 80)
        
        ; draw fringe
        (gimp-drawable-fill l_fringe FILL-TRANSPARENT)
        (gimp-image-add-layer image l_fringe 0)
        (gimp-rect-select image (+ 0 fringeWidth) (+ 0 fringeWidth) (- imgWidth (* 2 fringeWidth)) (- imgHeight (* 2 fringeWidth)) CHANNEL-OP-ADD TRUE (abs (/ imgWidth 10)))
        (gimp-selection-invert image)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-bucket-fill l_fringe BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        (gimp-selection-none image)
        (gimp-layer-set-mode l_fringe LAYER-MODE-OVERLAY)
        (gimp-image-insert-layer image (car (gimp-layer-copy l_fringe FALSE)) 0 0)
        
        (if (= fringeBoost TRUE)
            (gimp-image-insert-layer image (car (gimp-layer-copy l_fringe FALSE)) 0 0)
        )
        
        ; clean up.
        (gimp-image-undo-group-end image)
        
        ; flush display to the user 
        (gimp-displays-flush)
        
    )
)

(script-fu-register "script-fu-lomo-berco"
            "<Toolbox>/Script-Fu/Photo/Effects/Bercovich Lomo..."
            "Turn an image into a 'LOMO' snap. \nwww.lomography.com for those that are not sure what lomo's are.\nfile:bercovich-lomo.scm\nEnjoy!"
            "Avi Bercovich <avi@sillypages.org>"
            "Avi Bercovich"
            "April 2003"
            "RGB* GRAY* INDEXED*"
            SF-IMAGE "Image" 0
            SF-DRAWABLE "Drawable" 0
            SF-ADJUSTMENT "Fringe offset" '(4 1 10 1 10 0 1)
            SF-TOGGLE "Boost fringe?" FALSE
)

; end of script