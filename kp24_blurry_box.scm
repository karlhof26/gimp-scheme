;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis 
;
; Blurry box script  for GIMP 2.10.18
;
;
; Tags: blurry, box, shadow
;
; Author statement:
;
; 
;   - Changelog -
; Updated to work with Gimp2.10.18 (05-2020)
;
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define selfmenuroot "<Image>/Script-Fu/Light and Shadow/Shadow")     ; Change these to suit yourself         
(define selfmenuentry "Xach Blurry thing...")

(define selfauthor "Kevin Payne")
(define selfcopyright "Copyright (C) 2011 Kevin Payne paynekj@hotmail.com")

; Copyright (C) 2007 Kevin Payne paynekj@hotmail.com
;
; Version 0.1 2.12.2011 First version

(define selffileversion "02.12.2011 Version 0.1 - Just for a laugh")

(define selffilename "\n- file:kp24_blurry_box.scm")

(define selftipstrip (string-append "Test" "\n\n\nRegisters in menu " selfmenuroot  "/" selfmenuentry "\n\nScript File name - " selffilename))


(define (script-fu-kp24_blurry_box image drawable hl-offset-x hl-offset-y hl-color hl-opacity-comp ds-color ds-opacity ds-blur ds-offset-x ds-offset-y keep-selection blur_rad)
  (let* ( 
          (blurry_layer 0) (saved-selection 0) (layer-mask 0)
        )
        (gimp-context-push)
 
        (gimp-image-undo-group-start image)

        (if (= (car (gimp-selection-bounds image)) FALSE) (error "No selection found"))

        ; Need to save the selection so that we can blur the whole layer
        (set! saved-selection (car (gimp-selection-save image)))
        (gimp-selection-none image)                                                   ; loose the selection
        (set! blurry_layer (car (gimp-layer-copy drawable TRUE)))                     ; create the layer to blur
        (gimp-image-add-layer image blurry_layer -1)                                  ; add the new layer to the image
        (plug-in-gauss RUN-NONINTERACTIVE image blurry_layer blur_rad blur_rad 0)     ; blur the new layer
        (set! layer-mask (car (gimp-layer-create-mask blurry_layer ADD-MASK-BLACK)))  ; create a black layer-mask
        (gimp-layer-add-mask blurry_layer layer-mask)                                 ; add the layer mask to the layer
        (gimp-selection-load saved-selection)                                         ; restore the selection
        (gimp-context-set-foreground '(255 255 255))                                  ; set the foreground colour to white
        (gimp-edit-bucket-fill layer-mask BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE 0 0) ; fill the selection on layer mask
        
        ; Do the Xach effect on the selection from the original layer
        (script-fu-xach-effect image drawable hl-offset-x hl-offset-y hl-color hl-opacity-comp ds-color ds-opacity ds-blur ds-offset-x ds-offset-y keep-selection)

        (gimp-image-undo-group-end image)
        (gimp-displays-flush)

        (gimp-context-pop)
  )
)

(script-fu-register "script-fu-kp24_blurry_box"
                    selfmenuentry
                    selftipstrip
                    selfcopyright
                    selfauthor
                    selffileversion
                    ""
                    SF-IMAGE "Input Image" 0
                    SF-DRAWABLE "Input Drawable" 0
    SF-ADJUSTMENT     "Highlight X offset"      '(-1 -100 100 1 10 0 1)
    SF-ADJUSTMENT     "Highlight Y offset"      '(-1 -100 100 1 10 0 1)
    SF-COLOR          "Highlight color"         "white"
    SF-ADJUSTMENT     "Highlight opacity"       '(66 0 255 1 10 0 0)
    SF-COLOR          "Drop shadow color"       "black"
    SF-ADJUSTMENT     "Drop shadow opacity"     '(100 0 100 1 10 0 0)
    SF-ADJUSTMENT     "Drop shadow blur radius" '(12 0 255 1 10 0 1)
    SF-ADJUSTMENT     "Drop shadow X offset"    '(5 0 255 1 10 0 1)
    SF-ADJUSTMENT     "Drop shadow Y offset"    '(5 0 255 1 10 0 1)
    SF-TOGGLE         "Keep selection"          TRUE
    SF-ADJUSTMENT     "Blur Radius"    '(5 0 255 1 10 0 1)
)

(script-fu-menu-register "script-fu-kp24_blurry_box" "<Toolbox>/Script-Fu/Light and Shadow/Shadow")

; end of script