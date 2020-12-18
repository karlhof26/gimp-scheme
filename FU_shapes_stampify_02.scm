; FU_shapes_stampify.scm 
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/04/2014 on GIMP-2.8.10 
; 16/12/2020 on GIMP-2.10.22
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
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	
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
; 
; adapted for GIMP-2 by Eddy Verlinden
; filter found at: http://users.telenet.be/ev1/gimpphotoeffects_en.html
;==============================================================


(define (fu-stampify img drawable paper hole diameter gap marg)
    (let* (
            (owidth (car (gimp-image-width img)))
            (oheight (car (gimp-image-height img)))
            (nw (+ diameter (+ owidth (* 2 marg))))
            (nh (+ diameter (+ oheight (* 2 marg))))
            (img2 (car (gimp-image-new nw nh RGB)))
            (layer1 (car (gimp-layer-new img2 nw nh 0 "Layer1" 100 LAYER-MODE-NORMAL)))
            (nholes 0)
            (pos 0)
            (i 0)
            (dist 0)
            (floating-sel 0)
          )
        (gimp-image-undo-group-start img)
        (gimp-image-undo-group-start img2)
        (gimp-image-insert-layer img2 layer1 0 0)
        ;;(gimp-image-add-layer img layer1 -1)
        (gimp-image-set-active-layer img2 layer1)
        (gimp-context-set-background paper)
        (gimp-drawable-fill layer1 1)
        (gimp-selection-none img2)
        
        ; calculate number of horisontal holes
        (set! nholes (/ (+ nw gap) (+ diameter gap)))
        (set! pos 0)
        (set! i 0)
        
        ; loop horisontally
        (while (< i nholes)
            (gimp-image-select-ellipse img2 CHANNEL-OP-ADD pos 0 diameter diameter)
            (set! pos (+ pos diameter gap))
            (set! i (+ i 1))
        )
        
        ; calculate number of vertical holes
        (set! nholes (/ (+ nh gap) (+ diameter gap)))
        (set! pos 0)
        (set! i 0)
        
        ; loop vertically
        (while (< i nholes)
            (gimp-image-select-ellipse img2 CHANNEL-OP-ADD 0 pos diameter diameter)
            (set! pos (+ pos diameter gap))
            (set! i (+ i 1))
        )
        
        ; and fill the holes with a colour
        (gimp-context-set-background hole)
        (gimp-edit-fill layer1 1)
        (gimp-selection-none img2)
        
        ; and here comes the clever part:
        ; offset horis and vert holes by half the diameter
        (set! dist (* -1 (/ diameter 2)))
        (gimp-drawable-offset layer1 1 0 dist dist)

        ; insert old image into a new layer in img2
        (gimp-selection-all img)
        (gimp-edit-copy drawable)
        (set! floating-sel (car (gimp-edit-paste layer1 0)))
        (gimp-floating-sel-anchor floating-sel)
        
        (gimp-layer-new-from-visible img2 img "finLayer")
        ; and return command to The Gimp
        (gimp-image-clean-all img2)
        (gimp-display-new img2)
        (gimp-image-undo-group-end img2)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(script-fu-register "fu-stampify"
    "Postage Stampify"
    "Will make an image look like a postage stamp. \nfile:FU_shapes_stampify_02.scm"
    "Claes G Lindblad <claesg@algonet.se>"
    "Claes G Lindblad <claesg@algonet.se>"
    "990330"
    "*"
    SF-IMAGE "Input Image"              0
    SF-DRAWABLE "Input Drawable"        0
    SF-COLOR "Paper colour"                             '(242 242 242)
    SF-COLOR "Hole colour"                              '(153 153 153)
    SF-VALUE "Diameter of perforation"                  "20"
    SF-VALUE "Gap between perforations"                 "10"
    SF-VALUE "Marginal between art and perforations"    "7"
)

(script-fu-menu-register "fu-stampify" "<Toolbox>/Script-Fu/Shapes")

;end of script