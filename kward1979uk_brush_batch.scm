; GPLv3 
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
; Copyright (C) 2006 Karl Ward
;
; 07/10/2020  Updated to Gimp-2.10.20
;
;
(define (brush-batch load opt name filename spacing location)
 
 (let* (
        (a 0)
        (drawable 0)
        (selection-bounds 0)
        (sx1 0)
        (sy1 0)
        (sx2 0)
        (sy2 0)
        (swidth 0)
        (sheight 0)
        (newimage 0)
        (newlayer 0)
        (active 0)
        (filename2 0)
        (s 0)
        (filelist 0)
    )
    
    (set! a
        (cond 
            (( equal? opt 0 ) ".jpg" )
            (( equal? opt 1 ) ".bmp" )
            (( equal? opt 2 ) ".xcf" )
            (( equal? opt 3 ) ".png" )
            (( equal? opt 4 ) ".gif" )
        )
    )
    (let* (
            (num-files (car (file-glob (string-append load "\\*" a)  1)))
            (filelist (cadr (file-glob (string-append load "\\*" a)  1)))
            (s 1)
          )
        (while (> num-files 0) ; was while filelist
            (let* (
                    
                    (loadfile (car filelist))
                    (image (car (gimp-file-load RUN-NONINTERACTIVE loadfile loadfile)))
                  )
                
                
                (gimp-image-flatten image)
                (set! drawable (gimp-image-get-active-drawable image))
                (if (= 1 (car (gimp-selection-is-empty image)))
                    (gimp-selection-all image)
                )
                (gimp-displays-flush)
                (gimp-edit-copy (car drawable) )
                (set! selection-bounds (gimp-selection-bounds image))
                (set! sx1 (cadr selection-bounds))
                (set! sy1 (caddr selection-bounds))
                (set! sx2 (cadr (cddr selection-bounds)))
                (set! sy2 (caddr (cddr selection-bounds)))
                (gimp-image-delete image)
                (set! swidth  (- sx2 sx1))
                (set! sheight (- sy2 sy1))
                (set! newimage (gimp-image-new swidth sheight 0))
                (gimp-message "line 81")
                (set! newlayer (gimp-layer-new (car newimage) swidth sheight 1 "newlayer" 100 0))
                (gimp-image-insert-layer (car newimage) (car newlayer) 0 0)
                (gimp-drawable-fill (car newlayer) 3)
                (gimp-edit-paste (car newlayer) 0 )
                (gimp-image-flatten (car newimage))
                (set! active (gimp-image-get-active-drawable (car newimage)))
                (gimp-drawable-desaturate (car active) DESATURATE-LUMINANCE)
                (gimp-image-convert-grayscale (car newimage))
                (gimp-displays-flush)
                (gimp-message "line 90")
                (gimp-selection-all (car newimage))
                (set! filename2 (string-append location "/" filename (string-append (number->string s))".gbr"))
                (file-gbr-save 1 (car newimage) (car active) filename2 (string-append name (number->string s)) spacing (string-append name (number->string s)))
            
            )
            (set! s (+ s 1))
            (gimp-image-delete (car newimage))
            (set! filelist (cdr filelist))
            (set! num-files (- num-files 1))
            (gimp-message "line 103")
        )
    )
    (gimp-message "Good finish OK")
 )
)

(script-fu-register "brush-batch"
            "<Image>/Script-Fu2/Misc/Brush-batch..."
            "Turns a folder of files into brush's works with jpg, bmp, xcf, png and gif. \nfile:kward1979uk_brush_batch.scm"
            "Karl Ward"
            "Karl Ward"
            "April 2006"
            ""
            
            SF-DIRNAME    "Load from" ""
            SF-OPTION     "File Type"'("jpg" "bmp""xcf""png""gif")
            
            SF-STRING     "Brush Name" "name"
            SF-STRING     "File Name" "filename"
            SF-ADJUSTMENT "spacing"         '(25 0 1000 1 1 1 0)
            SF-DIRNAME    "SAVE TO FOLDER" ""
)
           
           
           
;end of script