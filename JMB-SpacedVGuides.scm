; Spaced V-Guides
; This function creates a series of v-guides in the image at the user specified
; spacing
;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Spaced V Guides script  for GIMP 2.10.18
;
; Tags: spaced horizontal guides 
;
; Author statement:
;
; Creates Spaced Vertical Guides
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



(define (script-fu-spaced-vguides image spacing origin)

   (let*
      (
            (x 0) ; will be computed v-guide position.
            (width (car (gimp-image-width image)))
            (center (/ width 2))
      )
        
        (gimp-context-push) ; Initiates the temporary state.
        
        ; Calculate starting point
        (set! x (calc-offset origin center width spacing))
        
        ; Iterate and create h-guides until y > height
        (while (< x width)
            (gimp-image-add-vguide image x)
            (set! x (+ x spacing))
        )
        
        ; DEBUG
        ; (gimp-message 
        ;   (string-append "Origin: " (number->string origin) " \nSpacing: " (number->string spacing)
        ;   "\nCenter: " (number->string center) "\nMod: " y
        ;   )
        ;)
        ; DEBUG
        
        ; Deactivates the temporary state and resets the previous user defaults.
        (gimp-context-pop) 
        (gimp-displays-flush)
   ) ; close let*
) ; close define


(define (calc-offset origin center width spacing)
   (cond 
        ((<= origin 0)  spacing) ; Left 
        ((<= origin 1)  (modulo center spacing)) ; Center
        ((<= origin 2)  (modulo width spacing)) ; Right
   )
)


; Finally register our script with script-fu.
(script-fu-register
   "script-fu-spaced-vguides"                   ;func name
   "Spaced V-Guides"                            ;menu label
   ;description
   "Create a series of vertical guides at even spacing. \nfile:JBM-SpacedVGuides.scm"
   "Jeffrey Boulais"                           ;author
   "copyright 2009, Jeffrey Boulais"           ;copyright notice
   "February 2008"                             ;date created
   "RGB* GRAY* INDEXED*"                       ;image type that the script works on
   SF-IMAGE     "Image"                         0
   SF-VALUE     "Vertical Guide Spacing (px)"       "100"
   SF-OPTION    "Start from"                        '("Left" "Center" "Right")
   
)

(script-fu-menu-register "script-fu-spaced-vguides" "<Toolbox>/Script-Fu/Setup/Guides")

;end of script