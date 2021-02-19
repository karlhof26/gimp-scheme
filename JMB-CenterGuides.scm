; Center Guides
; This function puts a horizontal and vertical guide at the center of the 
; canvas.
;
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Center Guides script  for GIMP 2.10.18
;
; Tags: center guides centre
;
; Author statement:
;
; Creates Center Guides crossing in middle of the image
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
(define (script-fu-center-guides image)

   (let*
      (
            (height (car (gimp-image-height image)))
            (width (car (gimp-image-width image)))
            (ypos (/ height 2))
            (xpos (/ width 2))
      )
        
        (gimp-context-push) ; Initiates the temporary state.
        
        (gimp-image-add-vguide image xpos)
        (gimp-image-add-hguide image ypos)
        
        ; Cleanup
        (gimp-context-pop) ; Deactivates the temporary state and resets the previous user defaults.
        (gimp-displays-flush)
   )
)


; Finally register our script with script-fu.
(script-fu-register
    "script-fu-center-guides"                   ;func name
    "Center Guides"                             ;menu label
    ;description
    "Create guides at the center of the canvas. \nfile:JMB-Center-Guides.scm" 
    "Jeffrey Boulais"                           ;author
    "copyright 2009, Jeffrey Boulais"           ;copyright notice
    "October 13, 2009"                          ;date created
    "RGB* GRAY* INDEXED*"                       ;image type that the script works on
    SF-IMAGE    "Image"     0
)

(script-fu-menu-register "script-fu-center-guides" "<Toolbox>/Script-Fu/Setup/Guides")

;end of script