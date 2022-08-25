; 
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Vivid saturation script  for GIMP 2.4
; Original author: Olli Salonen <olli@cabbala.net>
;
; Tags: photo, color, saturation
;
; Author statement:
;
; script-fu-vivid-saturation - A script that will saturate the image with
; vivid saturated colors. Other implementations of this script are called
; "Digital Velvia".
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; 1.00 Jan 15, 2004- initial release
; 1.10 - Fixed for 2.4 by Alexia Death
; 1.11 - Updated for Gimp 2.10.18 by karlhof26
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


(define (script-fu-vivid-saturation inImg inDrw inAmount)
  
  (let* (
            
            (amount (/ inAmount 100))
        )
        
    (gimp-image-undo-group-start inImg)
    
    (plug-in-colors-channel-mixer TRUE inImg inDrw FALSE (+ 1 amount amount) (- amount) (- amount) (- amount) (+ 1 amount amount) (- amount) (- amount) (- amount) (+ 1 amount amount))
    
    (define (spline)
      (let* ((a (cons-array 8 'byte)))
        (set-pt a 0 0.000 0.000)
        (set-pt a 1 0.246 0.235) ; 63 60
        (set-pt a 2 0.749 0.7607) ; 191 194
        (set-pt a 3 1.000 1.000) ; 255 255
        a)
    )
    (define (set-pt a index x y)
        (prog1
        (aset a (* index 2) x)
        (aset a (+ (* index 2) 1) y)
        )
    )
    (gimp-drawable-curves-spline inDrw 0 8 (spline))
    
    (gimp-image-undo-group-end inImg)
    (gimp-displays-flush)
    (gc) ; array was used so garbage cleanup
  )
)


(script-fu-register "script-fu-vivid-saturation"
            "<Toolbox>/Script-Fu/Photo/Enhancement/_Vivid Saturation"
            "Vivid saturation of image. Gives similar effect as many Digital Velvia filters do for the Other Product. \nfile:salonen-vivid-saturation.scm"
            "Olli Salonen <olli@cabbala.net>"
            "Olli Salonen"
            "Jan 15, 2004"
            ""
            SF-IMAGE              "Image"                0
            SF-DRAWABLE           "Drawable"             0
            SF-ADJUSTMENT         "Amount"              '(10 0 50 1 1 0 0)
)

; end of script