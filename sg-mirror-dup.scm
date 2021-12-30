;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

; Revised September 2010 to work with GIMP 2.4 and later
; Revised Jan 2021 for GIMP-2.10.22 by karlhof26
;

(define (script-fu-sg-mirror-dup image layer iterations horizontal vertical workcopy)
  (let* ((work-image 0)
         (new-layer 0)
         (orig-width 0)
         (orig-height 0)
         )
    (if (= workcopy TRUE)
      (begin
        (set! work-image (car (gimp-image-duplicate image)))
        (gimp-image-undo-disable work-image)
        (gimp-display-new work-image)
        )
      (begin
        (set! work-image image)
        (gimp-image-undo-group-start work-image)
        )
      )
    (gimp-selection-none work-image)
    (while (> iterations 0)
      (set! layer (car (gimp-image-get-active-layer work-image)))
      (if (> (car (gimp-image-get-layers work-image)) 1)
        (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
        )
      (if (= horizontal TRUE)
        (begin
          (set! new-layer (car (gimp-layer-copy layer 1)))
          (gimp-image-insert-layer work-image new-layer 0 -1)
          (set! orig-width (car (gimp-drawable-width new-layer)))
          (set! orig-height (car (gimp-drawable-height new-layer)))
          (gimp-layer-resize
              new-layer
              (* 2 orig-width)
                  orig-height
                  0
                  0
                  )
          (set! new-layer (car (gimp-item-transform-flip-simple new-layer
              ORIENTATION-HORIZONTAL
              TRUE
              orig-width
              ;0
              ))
            )
          (gimp-image-resize-to-layers work-image)
          (if (> (car (gimp-image-get-layers work-image)) 1)
            (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
            )
          )
        )
      (if (= vertical TRUE)
        (begin
          (set! new-layer (car (gimp-layer-copy layer 1)))
          (gimp-image-insert-layer work-image new-layer 0 -1)
          (set! orig-width (car (gimp-drawable-width new-layer)))
          (set! orig-height (car (gimp-drawable-height new-layer)))
          (gimp-layer-resize
              new-layer
              orig-width
              (* 2 orig-height)
              0
              0
              )
          (set! new-layer (car (gimp-item-transform-flip-simple new-layer
              ORIENTATION-VERTICAL
              TRUE
              orig-height
              ))
          )
          (gimp-image-resize-to-layers work-image)
        )
      )
      (set! iterations (- iterations 1))
      )
    (gimp-selection-none work-image)
    (if (> (car (gimp-image-get-layers work-image)) 1)
      (set! layer (car (gimp-image-merge-visible-layers work-image EXPAND-AS-NECESSARY)))
      )
    (gimp-displays-flush)
    (if (= workcopy TRUE)
      (begin
        (gimp-image-undo-enable work-image)
        (gimp-image-clean-all work-image)
        )
      (gimp-image-undo-group-end image)
      )
    )
  )

(script-fu-register "script-fu-sg-mirror-dup"
    "Mirror Duplicates and Reflections..."
    "Duplicates the image with mirror images. \nfile:sg-mirror-dup.scm"
    "Saul Goode"
    "Saul Goode"
    "4/17/2006"
    "*"
    SF-IMAGE    "Image"    0
    SF-DRAWABLE "Drawable" 0
    SF-ADJUSTMENT "Iterations (Image doubles each time)" '( 1 0 10 1 1 0 1 )
    SF-TOGGLE "Horizontal direction" TRUE
    SF-TOGGLE "Vertical direction" FALSE
    SF-TOGGLE "Work on copy" TRUE
)

(script-fu-menu-register "script-fu-sg-mirror-dup"
  "<Toolbox>/Script-Fu/Map/"
)
