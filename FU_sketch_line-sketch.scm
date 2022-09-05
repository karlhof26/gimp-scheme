; FU_sketch_line-sketch.scm  
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/04/2014 on GIMP-2.8.10
;
; 11/22/2007 - modified by Paul Sherman for GIMP 2.4
; 05/22/2020 - modified for Gimp 2.10.18
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
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;
;   Windows Vista/7/8)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   
;   Windows XP
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;   Linux
;   /home/yourname/.gimp-2.8/scripts  
;   
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
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
; Line Sketch script by kward1979uk
;==============================================================


(define (set-pt a index x y)
    (begin
        (aset a (* index 2) x)
        (aset a (+ (* index 2) 1) y)
    )
)
  
(define (spline-sketch)
    (let* (
            (a (cons-array 18 'double)) ; was make-vector
          )
        (set-pt a 0 0.0 0.0)
        (set-pt a 1 0.745098 0.001)      ; 0.745098 190 0
        (set-pt a 2 0.749019 0.005)      ; 0.749019 191 1
        (set-pt a 3 0.832 0.832)    ; 0.823529 210
        (set-pt a 4 0.862 0.862)    ; 0.862745 220
        (set-pt a 5 0.9019 0.9019)    ; 0.901960 230
        (set-pt a 6 0.9411 0.9411)    ; 0.941176 240
        (set-pt a 7 0.9039 0.9039)    ; 0.903921 250
        (set-pt a 8 1.0000 1.0000)    ; 0.999989 255
        
        a
    )
)


(define (FU-line-sketch inimage indraw bg-colour)
    (let* (
            (theImage 0)
            (theDraw 0)
            (height 1)
            (width 0)
            (highpass 0)
            (imagemask 0)
            (copy-paste 0)
            (background 0)
          )
        (set! theImage inimage)
        (set! theDraw indraw)
        (gimp-image-undo-group-start theImage)
        
        (set! height (car (gimp-drawable-height theDraw)))
        (set! width (car (gimp-drawable-width theDraw)))
        (plug-in-edge 1 theImage theDraw 2 1 0)
        (gimp-drawable-equalize theDraw 0)
        (gimp-drawable-desaturate theDraw DESATURATE-LIGHTNESS)
        (set! highpass (car (gimp-layer-copy theDraw 1)))
        (gimp-image-insert-layer theImage highpass 0 1)
        (gimp-drawable-curves-spline highpass 0 18 (spline-sketch))
        (gimp-drawable-invert theDraw FALSE)
        (gimp-layer-add-alpha theDraw)
        (set! imagemask (car (gimp-layer-create-mask theDraw 0)))
        (gimp-layer-add-mask theDraw imagemask)
        (gimp-edit-copy highpass)
        (set! copy-paste (car (gimp-edit-paste imagemask 0)))
        (gimp-floating-sel-anchor copy-paste)
        (gimp-context-set-background bg-colour)
        (set! background (car (gimp-layer-new theImage width height 1 "background" 100 0)))
        (gimp-drawable-fill background FILL-BACKGROUND)
        (gimp-image-insert-layer theImage background 0 0)
        (gimp-image-remove-layer theImage highpass)
        (gimp-image-lower-item theImage background)
        
        (gimp-image-undo-group-end theImage)
        (gimp-displays-flush) 
        (gc) ; garbage collection because an array was used
    )
)

(script-fu-register "FU-line-sketch"
    "<Toolbox>/Script-Fu/Artist/Line Sketch"
    "Turns a image into a sketch. If the image is a alpha layer it will be flattened first, background color is selectable. \nfile:FU_sketch_line-sketch.scm"
    "Karl Ward"
    "Karl Ward"
    "Feb 2006"
    "RGB*"
    SF-IMAGE        "SF-IMAGE"              0
    SF-DRAWABLE     "SF-DRAWABLE"           0
    SF-COLOR        "Background Colour"     '(255 255 255)
)

; end of script