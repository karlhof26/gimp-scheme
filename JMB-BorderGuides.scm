; Border Guides
; This script creates guides along the border of the canvas, offset by the 
; user specified amount.

;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Border Guides script  for GIMP 2.10.18
;
; Tags: guides
;
; Author statement:
;
; Creates Border Guides offset from edges
;
; --------------------------------------------------------------------
; Distributed by ...
; --------------------------------------------------------------------
;   - Changelog -
; Updated to work with Gimp2.4 (11-2007)
; http://www.gimpscripts.com
;
; Updated to work with Gimp2.10.18 (05-2020)
;
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

(define (script-fu-border-guides image x y)
    
    (let*
        (
            (height (car (gimp-image-height image)))
            (width (car (gimp-image-width image)))
        )
        
        (gimp-context-push) ; Initiates the temporary state.
        
        (if (or (< x 0) (< y 0))
            (gimp-message "X and Y must be less than the canvas mid-point") ; TRUE
            (if (and (< x (/ width 2)) (< y (/ height 2)))
                (add-guides image x y height width) ; TRUE
                (gimp-message "X and Y must be less than the canvas mid-point") ; FALSE
            ); Close If
        ); Close If
        
        ; Deactivates the temporary state and resets the previous user defaults.
        (gimp-context-pop) 
        (gimp-displays-flush)
    )
)

; add-guides
; Adds the guides to the image
(define (add-guides image x y height width)
    (gimp-image-add-hguide image y)
    (gimp-image-add-hguide image (- height y))
    
    (gimp-image-add-vguide image x)
    (gimp-image-add-vguide image (- width x))
)

; Finally register our script with script-fu. 
(script-fu-register
    "script-fu-border-guides"                   ;func name
    "Border Guides"                             ;menu label
    ;description
    "Create guides offset from the edge of the canvas. \nfile:JMB-Border-Guides.scm" 
    "Jeffrey Boulais"                           ;author
    "copyright 2009, Jeffrey Boulais"           ;copyright notice
    "October 13, 2009"                          ;date created
    "RGB* GRAY* INDEXED*"                       ;image type that the script works on
    SF-IMAGE "Image"            0
    SF-VALUE "X Offset (px)"    "0"
    SF-VALUE "Y Offset (px)"    "0"
)

(script-fu-menu-register "script-fu-border-guides" "<Toolbox>/Script-Fu/Setup/Guides")

;end of script