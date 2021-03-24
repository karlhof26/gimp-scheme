;   GPLv3  
;   Copyright (C) 2006 Tom Lechner <tom@tomlechner.com>
;   
;   This program is free software; you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation; either version 3 of the License, or
;   (at your option) any later version.
;   
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;   
;   You should have received a copy of the GNU General Public License
;   along with this program; if not, write to the Free Software
;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;

(define (script-fu-gradient-fill-horiz image drawable)
  (let* (
            (bounds (cdr (gimp-drawable-mask-bounds drawable)))
            (left  (car bounds))
            (top (cadr bounds))
            (right (caddr bounds))
        )
        (gimp-image-undo-group-start image)
        (gimp-edit-blend drawable BLEND-CUSTOM LAYER-MODE-NORMAL
                     GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE
                     FALSE 0 0 TRUE
                     left top right top)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gc) ; garbage collect
  )
)

(define (script-fu-gradient-fill-vert image drawable)
  (let* (
            (bounds (cdr (gimp-drawable-mask-bounds drawable)))
            (left  (car bounds))
            (top (cadr bounds))
            (bottom (car (cdddr bounds)))
      )
        (gimp-image-undo-group-start image)
        (gimp-edit-blend drawable BLEND-CUSTOM LAYER-MODE-NORMAL
                     GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE
                     FALSE 0 0 TRUE
                     left top left bottom)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gc) ; garbage collect
  )
)


(script-fu-register "script-fu-gradient-fill-horiz"
    "<Image>/Script-Fu2/Select/Gradient Fill Horizontal"
    "Fill selection with gradient from left to right. \nfile:Gradient fill selection.scm"
    "Saul Goode"
    "Saul Goode"
    "1/16/2007"
    ""
    SF-IMAGE    "Image"    0
    SF-DRAWABLE "Drawable" 0
)

(script-fu-register "script-fu-gradient-fill-vert"
    "<Image>/Script-Fu2/Select/Gradient Fill Vertical"
    "Fill selection with gradient from top to bottom. \nfile:Gradient fill selection.scm"
    "Saul Goode"
    "Saul Goode"
    "1/16/2007"
    ""
    SF-IMAGE    "Image"    0
    SF-DRAWABLE "Drawable" 0
)

; end of script 