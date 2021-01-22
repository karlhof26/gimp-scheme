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
;
;

(define (script-fu-golden-guides image drawable)
    (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        )

    (gimp-image-add-hguide image (/ (* height 1000) 1618))
    (gimp-image-add-hguide image (/ (* height 382) 1000))
    (gimp-image-add-vguide image (/ (* width 1000) 1618))
    (gimp-image-add-vguide image (/ (* width 382) 1000))

    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-golden-guides"
  "Golden Rule Guides"
  "Adds Golden Ratio guides \nfile:golden-rule.scm"
  "Karl Hofmeyr"
  "Karl Hofmeyr, 2021"
  "January 2021"
  ""
  SF-IMAGE      "Input Image"      0
  SF-DRAWABLE   "Input Drawable"   0
)

(script-fu-menu-register "script-fu-golden-guides"
                         "<Image>/Image/Guides")
