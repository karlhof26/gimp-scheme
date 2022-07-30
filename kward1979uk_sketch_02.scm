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

(define (set-pt arr i x y) 
    (aset arr (* i 2) x)
    (aset arr (+ (* i 2) 1) y)
)

(define (spline-sketch)
  (let* (
            (a (cons-array 18 'byte))
        )
        (set-pt a 0 0 0)
        (set-pt a 1 190 0)
        (set-pt a 2 191 1)
        (set-pt a 3 210 210)
        (set-pt a 4 220 220)
        (set-pt a 5 230 230)
        (set-pt a 6 240 240)
        (set-pt a 7 250 250)
        (set-pt a 8 255 255)
    
    a
  )
)

(define (kward1979uk-sketch inimage indraw bg-colour)
  (gimp-layer-flatten indraw)   ; NO transparency
  (let* (
            
            (theImage inimage)
            (theDraw indraw)
            (a)
        )
        (gimp-image-undo-group-start theImage)
        (let* (
                    (height (car (gimp-drawable-height theDraw)))
                    (width (car (gimp-drawable-width theDraw)))
            )
            (plug-in-edge 1 theImage theDraw 2 1 0)
            (gimp-equalize theDraw 0)
            (gimp-drawable-desaturate theDraw DESATURATE-LUMINANCE)
            (let* (
                (highpass (car (gimp-layer-copy theDraw 1)))
                )
                (gimp-image-insert-layer theImage highpass 0 1)
                (gimp-curves-spline highpass 0 18 (spline-sketch))
                (gimp-drawable-invert theDraw FALSE)
                (gimp-layer-add-alpha theDraw)
                (let* (
                        (imagemask (car (gimp-layer-create-mask theDraw 0)))
                    )
                    (gimp-layer-add-mask theDraw imagemask)
                    (gimp-edit-copy highpass)
                    (let*(
                            (copy-paste (car (gimp-edit-paste imagemask 0)))
                        )
                        (gimp-floating-sel-anchor copy-paste)
                        (gimp-context-set-background bg-colour)
                        (let* (
                                (background (car (gimp-layer-new theImage width height 1 "background" 100 0)))
                            )
                            (gimp-drawable-fill background 1)
                            (gimp-image-insert-layer theImage background 0 0)
                            (gimp-image-remove-layer theImage highpass)
                            (gimp-image-lower-layer theImage background)
                            
                            (gimp-image-undo-group-end theImage)
                            (gimp-displays-flush)
                        )
                    )
                )
            )
        )
    )
)

(script-fu-register "kward1979uk-sketch"
    "<Toolbox>/Script-Fu/Artist/Sketch..."
    "Turns a image into a sketch if the image is a alpha layer it must be flatterned first. \nfile:kaward1979uk_sketch_02.scm"
    "Karl Ward"
    "Karl Ward"
    "Feb 2006"
    "RGB*"
    SF-IMAGE        "SF-IMAGE"          0
    SF-DRAWABLE     "SF-DRAWABLE"       0
    SF-COLOR        "Background Colour" '(255 255 255)
    
)
				
