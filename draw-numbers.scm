;;; Gilles Maire 
;;; GPL v3
;;; gilles@gillesmaire.com
;;; http://www.gillesmaire.com
;;; Draws circles with incremental mnumber inside
;;; version 1.1
; The GIMP -- an image manipulation program 
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
;


(define (draw-numbers image layer n1 n2 colb colf font diam)
    (let* (
            (i 0) 
            (j 0)
            (letter "x")
            (x 0) 
            (fontsize ( * diam 0.6))
            (y (/ fontsize  3.5))
            (layer 0)
            ;(len car (string-length (number->string n2)))
            (len (string-length (number->string n2))) 
            (select-layer 0)
            (layername "Layer") 
            (y0 0) 
            (H (- (car (gimp-image-height image)) diam))
          )
        (gimp-message "started ok")
        (do ((i n1 (+ i 1)))
            ((> i n2))
                (if (= (string-length (number->string i))  2)  (set! x (/ fontsize 4.2)) (set! x (/ fontsize 2.0)))
                (set! y0 (+ (+ y0 diam) diam))
                (if (> y0  H)
                    (begin
                        (set! y0 0)
                        ;(set! x (+ x diam))
                    )
                )
                (set! j (- i 1))
                (set! letter (number->string i))
                (set! layername ( string-append "Layer_" letter))
                (set! layer (car (gimp-layer-new image diam diam RGBA-IMAGE layername 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image layer 0 j)
                (gimp-image-select-ellipse image CHANNEL-OP-ADD 0 0 diam diam)
                (gimp-context-set-foreground colb)
                (gimp-edit-bucket-fill layer BUCKET-FILL-FG  LAYER-MODE-NORMAL  100  100  0  10 10)
                (gimp-context-set-foreground colf)
                (set! select-layer (car (gimp-text-fontname image layer x y letter 0 TRUE fontsize PIXELS font)))
                (gimp-floating-sel-anchor select-layer)   
                (gimp-layer-translate layer 0 y0)
        )
        (gimp-displays-flush)
    )
)

(define (merge-and-export image drawable)
 (let* (
            (layer (car(gimp-image-merge-visible-layers image CLIP-TO-IMAGE)))(filename "/tmp/save.png")
       )
        (set! filename (car(gimp-image-get-filename image)))
        (gimp-file-save RUN-NONINTERACTIVE image layer filename filename)
        (gimp-quit TRUE)
 )
)
   
(script-fu-register "draw-numbers"
                    "<Toolbox>/Script-Fu/Decor/DrawNumber/Draw Numbers"
                    "Make circle numbers. Higher numbered layers are under the first set. \n file:draw-numbers.scm"
                    "Gilles Maire "
                    "GPL3 2015"
                    "2015/12/09"
                    ""
                    SF-IMAGE "image" 0
                    SF-DRAWABLE "drawable" 0
                    SF-ADJUSTMENT "Minimum" '(1 1 99 1 10 0 SF-SPINNER)
                    SF-ADJUSTMENT "Maximum" '(5 1 99 1 10 0 SF-S)
                    SF-COLOR "Background" '(100 100 100 ) 
                    SF-COLOR "Foreground" '(255 255 255 ) 
                    SF-FONT "Font" "Sans"
                    SF-ADJUSTMENT "Size" '(25 10 80 1 10 0 SF-SPINNER)
                    
)

(script-fu-register "merge-and-export"
                    "<Toolbox>/Script-Fu/Decor/DrawNumber/Draw Numbers helper merge and quit"
                    "Merge and quit. Merge numbers layers and then quit Gimp. \nfile:draw-numbers.scm"
                    "Gilles Maire "
                    "GPL3 2015"
                    "2016/12/09"
                    ""
                    SF-IMAGE "image" 0
                    SF-DRAWABLE "drawable" 0
)

