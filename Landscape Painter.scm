;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;               Landscape Painter              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A GIMP script-fu to generate a painted look to a
; landscape photo.  Sometimes provides an interesting
; effect on portraits and animal shots.
;
; Creates a top layer set to Darken Only mode and
; then blurs it.  Varying the blur radius will change
; the effect, as will applying a Levels or Curves adjustment
; to the Darken Only layer.  Just play with it and see 
; what you get!
;
; Tested on 2.10.20
; 4/12/2006 and 11/9/2020
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;

 
(define (script-fu-Land_paint  img drawable blur-rad merge-flag )
    (let* (
            (darken-layer 0)
        )
        ; Start an undo group.  Everything between the start and the end
        ; will be carried out if an undo command is issued.
        
        (gimp-image-undo-group-start img)
        
        ;; CREATE THE DARKEN ONLY LAYER ;;
        ; Create a new layer
        (set! darken-layer (car (gimp-layer-copy drawable 0)))
        
        ; Give it a name
        (gimp-drawable-set-name darken-layer "Darken Only layer")
        
        ; Add the new layer to the image
        (gimp-image-insert-layer img darken-layer 0 0)
        ; Set opacity to 100%
        (gimp-layer-set-opacity darken-layer 100)
        (gimp-layer-set-mode darken-layer LAYER-MODE-DARKEN-ONLY)
        ;
        ;
        ; Blur the layer
        (if (> blur-rad 0)
            (plug-in-gauss-iir 1 img darken-layer blur-rad 1 1 )
            ()
        )
        ;
        ;
        ; NOW MERGE EVERYTHING DOWN IF DESIRED
        (if (equal? merge-flag TRUE)
            (set! merged-layer (car(gimp-image-merge-down img darken-layer 1 )))
            ()
        )
        
        (if (equal? merge-flag TRUE)
            (gimp-drawable-set-name merged-layer "Result of Landscape Painter")
            ()
        )
        
        ; Complete the undo group 
        (gimp-image-undo-group-end img)
        ; Flush the display
        (gimp-displays-flush)
    )
)
 
 
(script-fu-register "script-fu-Land_paint"
        "<Toolbox>/Script-Fu/Effects/Landscape Painter DarkenLayer"
        "Add Darken Only layer and blur it \nfile:Landscape Painter.scm"
        "Script by Mark Lowry"
        "Technique by Mark Lowry"
        "2006"
        "RGB*, GRAY*"
        SF-IMAGE "Image"                0
        SF-DRAWABLE "Current Layer"     0
        SF-VALUE "Blur radius?"         "15"
        SF-TOGGLE "Merge Layers?"       FALSE
)

;end of script