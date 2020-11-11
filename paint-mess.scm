;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; Paint Mess script for Gimp 2.10.20
; Copyright SteveMi 2015
;
; Tags: photo, paint mess
;
; Author statement:
;
; This script creates a Paint mess creation. Uses an image and the background colour. 
;
; --------------------------------------------------------------------
; Distributed by karlhof26 on GitHub
; --------------------------------------------------------------------
;   - Changelog -
; Changelog:
;
;  Version 1.1
;    - Made the script compatible with GIMP 2.10.22
;
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;

(define (script-fu-xl5-mk1 image layer amount swaps color polarcoords)
    
    (let* (
            (lay-width (car (gimp-drawable-width layer)))
            (lay-height (car (gimp-drawable-height layer)))
            (X-axis 0)
            (Y-axis (* lay-height 2))
            (variable 0)
            (swap1 0)
            (swap2 0)
            (temp 0)
            (counter 0)
            (flag (make-vector (+ lay-width 2) 'double)) ;24 'double)))
            
            
            (srand (car (gettimeofday)))
            (swapcount swaps)
          )
        ;(gimp-message "started OK")
        ;(gimp-message "number checks")
        ;(gimp-message (number->string (vector-length flag)))
        (if (> (+ lay-height lay-width) 3000)
            (gimp-message "Warning: Script may fail on very large images due to running out of memory. If it fails consider resizing to less than 3000 for width+height.")
        )
        (gimp-progress-init "Painting part I" -1)
        (while (< counter lay-width)
            
            (vector-set! flag (+ counter 0) 0)
            (vector-set! flag (+ counter 1) 1)
            (vector-set! flag (+ counter 2) 2)
            
            (set! counter (+ counter 3))
        )
         ;(gimp-message "line 84")
        
        (while (> swapcount 0)
            ;(gimp-message "line 87")
            ;(set! swap1 (rand (vector-length flag)))
            (set! swap1 (rand lay-width))
            ;(gimp-message (number->string swap1))
            (set! swap2 (rand (vector-length flag)))
            (if (= swap1 swap2)
                (begin
                    ;(gimp-message "equal XXXXXXXXXXXXXXX")
                    (set! swap2 (rand (vector-length flag)))
                )
            )
            (if (> swap2 (+ lay-width 1))
                (begin
                    ;(gimp-message "swap 2 gr laywidth XXXXXXXXXXXXXXX")
                    (set! swap2 (rand lay-width))
                )
            )
            ;(gimp-message "line 104")
            (set! temp (vector-ref flag swap1))
            ;(gimp-message (number->string temp))
            (vector-set! flag swap1 (vector-ref flag swap2))
            ;(gimp-message "line 108")
            (vector-set! flag swap2 temp)
            (set! swapcount (- swapcount 1))
            ;(gimp-message (string-append "--------->" (number->string swapcount)))
            (gimp-progress-update (- 1 (/ swapcount swaps)))
        )
        
        ;(gimp-message "line 115")
        (gimp-image-undo-group-start image)
        
        (gimp-context-set-background color)
        (gimp-edit-copy layer)
        (gimp-image-scale image lay-width (* lay-height 3))
        (gimp-drawable-fill layer 1)
        (gimp-floating-sel-anchor (car (gimp-edit-paste layer FALSE)))
        
        ;(gimp-message "line 124")
        (gimp-progress-pulse)
        (gimp-progress-init "Painting part II" -1)
        (while (< X-axis lay-width)
            
            (set! variable (vector-ref flag X-axis))
            
            (case variable
                ((0)
                    (set! Y-axis (- Y-axis amount)))
                ((1)
                    (set! Y-axis (+ Y-axis amount)))
            )
            
            ;(gimp-rect-select image X-axis lay-height 1 lay-height 0 FALSE 0)
            (gimp-image-select-rectangle image CHANNEL-OP-ADD X-axis lay-height 1 lay-height)
             
            (gimp-drawable-transform-scale layer X-axis (- Y-axis lay-height) (+ X-axis 1) Y-axis 0 3 TRUE 3 0)
            
            (set! X-axis (+ X-axis 1))
            (gimp-progress-update (/ X-axis lay-width))
        )
        (gimp-progress-pulse)
        
        (gimp-floating-sel-anchor (car (gimp-image-get-active-drawable image)))
        (gimp-layer-add-alpha layer)
        ;(gimp-by-color-select-full layer color 0 0 FALSE FALSE 0 0 TRUE FALSE 0)
        (gimp-image-select-color image CHANNEL-OP-ADD layer color)
        (gimp-edit-clear layer)
        (gimp-selection-none image)
        (plug-in-autocrop FALSE image layer)
        
        (gimp-image-undo-group-end image)
        
        (gimp-progress-pulse)
        
        (if (= polarcoords TRUE)
            (begin
                (plug-in-polar-coords FALSE image layer 100 0 FALSE TRUE TRUE)
                (plug-in-autocrop FALSE image layer)
            )
        )
        (gimp-message "Good finish OK")
        (gimp-image-flatten image)
        (gc) ; garbage collect as an array was used 
    )
           
    (gimp-displays-flush)
    
)


(script-fu-register
    "script-fu-xl5-mk1"
    "<Toolbox>/Script-Fu/Patterns/Paint mess"
    "Paint mess creation. Uses an image and the background colour. Slow on large images. \nfile:paint-mess.scm"
    "SteveMi"
    "Unrestricted"
    "2015"
    "*"
    SF-IMAGE        "Image"             0
    SF-DRAWABLE     "Layer"             0
    SF-ADJUSTMENT   "Soft edge ------> Spiked edge"     '(20 0 30 1 4 0 0)
    SF-ADJUSTMENT   "Segmental ----------> Chaotic"     '(900 0 9000 100 1 0 0)
    SF-COLOR        "Set background colour"             '(0 0 0)
    SF-TOGGLE     "Open Distorts > Polar Coordinates"   TRUE
)

;end of script