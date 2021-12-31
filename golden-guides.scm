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

(define (script-fu-golden-guides-kh image drawable)
    (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        )
    (gimp-image-add-hguide image 0)
    (gimp-image-add-hguide image height)
    (gimp-image-add-vguide image 0)
    (gimp-image-add-vguide image width)
    
    ;add rule of thirds guides
    (gimp-image-add-hguide image (- height 30))
    (gimp-image-add-hguide image (round (* height 0.2800)))
    (gimp-image-add-hguide image (round (* height 0.6012)))
    (gimp-image-add-vguide image (* width 0.3333))
    (gimp-image-add-vguide image (* width 0.6666))
    
    ;add golden rule guides
    (gimp-image-add-hguide image (/ (* height 1000) 1618))
    (gimp-image-add-hguide image (/ (* height 382) 1000))
    (gimp-image-add-vguide image (/ (* width 1000) 1618))
    (gimp-image-add-vguide image (/ (* width 382) 1000))

    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-golden-guides-kh"
  "Golden Ratio plus Rule of Thirds Guides VerKH"
  "Adds Golden Ratio guides. Includes rule of thirds guides too. \nfile:golden-guides.scm"
  "Karl Hofmeyr"
  "Karl Hofmeyr, 2021"
  "January 2021"
  ""
  SF-IMAGE      "Input Image"      0
  SF-DRAWABLE   "Input Drawable"   0
)

(script-fu-menu-register "script-fu-golden-guides-kh"
                         "<Toolbox>/Script-Fu/Setup/Guides")
