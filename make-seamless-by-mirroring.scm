; make-seamless-by-mirroring.scm
; version 1.0 
;
; This is Script-Fu program written for GIMP 2.8.
; Updated for GIMP-2.10.20
;
; It creates a tileable image in perhaps the simplest way possible: by mirroring
; the image in both the x and y direction, leaving an image that is twice the
; original width and height.
;
; Preview X tiles:
; Preview Y tiles:
; If either (or both) of these are greater than '1', it will create a preview
; tiled image. It will also create an additional undo point to easily go back
; to the untiled version without re-running the script.
;
;
; Copyright 2015 John Tasto
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; 

(define (script-fu-make-seamless-by-mirroring image
    layerBase
    tilesX
    tilesY)
    (let* (
            (layerBasePosition (car (gimp-image-get-item-position image layerBase)))
            (width (car (gimp-drawable-width layerBase)))
            (height (car (gimp-drawable-height layerBase)))
            (layerOriginal (car (gimp-layer-new-from-drawable layerBase image)))
            (layerFloating 0)
            (offsetX 0)
            (offsetY 0)
          )
        (set! offsetX (/ width 2))
        (set! offsetY (/ height 2))
        (gimp-context-push)
        (gimp-context-set-defaults)
        (gimp-image-undo-group-start image)
        (gimp-image-insert-layer image layerOriginal 0 layerBasePosition)
        (gimp-image-resize image (* width 2) (* height 2) 0 0)
        (gimp-layer-resize layerBase (* width 2) (* height 2) 0 0)
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-selection-none image)
        (gimp-edit-copy layerOriginal)
        (set! layerFloating (car (gimp-edit-paste layerBase FALSE)))
        (gimp-item-transform-flip-simple layerFloating ORIENTATION-HORIZONTAL TRUE 0)
        (gimp-layer-translate layerFloating (ceiling (+ 0 offsetX))
            (ceiling (- 0 offsetY)))
        (gimp-selection-none image)
        (gimp-edit-copy layerOriginal)
        (set! layerFloating (car (gimp-edit-paste layerBase FALSE)))
        (gimp-item-transform-flip-simple layerFloating ORIENTATION-VERTICAL TRUE 0)
        (gimp-layer-translate layerFloating (ceiling (- 0 offsetX))
            (ceiling (+ 0 offsetY)))
        (gimp-selection-none image)
        (gimp-edit-copy layerOriginal)
        (set! layerFloating (car (gimp-edit-paste layerBase FALSE)))
        (gimp-item-transform-flip-simple layerFloating ORIENTATION-HORIZONTAL TRUE 0)
        (gimp-item-transform-flip-simple layerFloating ORIENTATION-VERTICAL TRUE 0)
        (gimp-layer-translate layerFloating (ceiling (+ 0 offsetX))
            (ceiling (+ 0 offsetY)))
        (gimp-floating-sel-anchor layerFloating)
        (gimp-selection-none image)
        (gimp-image-remove-layer image layerOriginal)
        (if (or (not (= tilesX 1)) (not (= tilesY 1)))
            (begin
                (gimp-image-undo-group-end image)
                (gimp-image-undo-group-start image)
                (script-fu-tile-random-rotation image layerBase tilesX tilesY FALSE FALSE)
            )
        )
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gimp-context-pop)
    )
)

(script-fu-register "script-fu-make-seamless-by-mirroring"
    "Make seamless by _mirroring..."
    "Make image seamless by mirroring it horizontally and vertically. \nfile:make-seamless-by-mirroring.scm"
    "John Tasto <john@tasto.net>"
    "John Tasto"
    "2015/09/03"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE        "Image" 0
    SF-DRAWABLE     "Drawable" 0
    SF-ADJUSTMENT   "Preview X tiles" '(1 1 256 1 1 0 SF-SPINNER)
    SF-ADJUSTMENT   "Preview Y tiles" '(1 1 256 1 1 0 SF-SPINNER)
)

(script-fu-menu-register "script-fu-make-seamless-by-mirroring"
    "<Image>/Script-Fu2/Pattern")

;end of script