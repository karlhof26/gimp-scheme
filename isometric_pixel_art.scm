;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Isometric Pixel Art script  for GIMP 2.10.24
; Original author: Bartłomiej Wójtowicz
;
; Tags: isometric,
; 
; Author statement:
;
; A GIMP script-fu to create Isometric pixel tiles or boxes
;
; --------------------------------------------------------------------
; Distributed by Karlhof26
; --------------------------------------------------------------------
;   - Changelog -
; Created on 4/15/2006 for 2.2.8
; Revised on 11/01/2022 to GIMP-2.10.24
;
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





(define (pa-a4double a b c d)
  (let* (
            (points (cons-array 4 'double))
    )
    (aset points 0 a)
    (aset points 1 b)
    (aset points 2 c)
    (aset points 3 d)
  )
)

(define (pa-color color)
    (gimp-context-set-foreground color)
)

(define (pa-fill-by-fuzzy-select image layer x y)
    ;(gimp-fuzzy-select layer x y 0 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (gimp-image-select-contiguous-color image CHANNEL-OP-REPLACE layer x y)
    
    (gimp-edit-bucket-fill-full layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 FALSE TRUE SELECT-CRITERION-COMPOSITE x y)
)

(define (pa-add-layer img name w h)
  (let* (
      (layer
        (car
          (gimp-layer-new img w h RGBA-IMAGE name 100 LAYER-MODE-NORMAL)
        )
      )
    )
    (gimp-image-insert-layer img layer 0 -1)
    (gimp-edit-clear layer)
    
    layer
  )
)

(define (pa-put-segment layer x y)
    (pa-draw-line layer x y (+ x 1) y)
)

(define (pa-make-power-of-two x)
  (if
    (= 0 (modulo x 2))
    x
    (+ x 1)
  )
)

(define (pa-pixel-mode)
    (gimp-context-set-brush "Pixel (1x1 square)")
)

(define (pa-draw-line layer x1 y1 x2 y2)
    (gimp-pencil layer 4 (pa-a4double x1 y1 x2 y2))
)

(define (pa-draw-line-down layer x1 y1 length)
  (let* (
      (x 0)
      (y 0)
    )
    
    (while (< x length)
        (pa-put-segment layer (+ x x1) (+ y y1))
        
        (set! x (+ x 2))
        (set! y (+ y 1))
    )
  )
)

(define (pa-draw-line-up layer x1 y1 length)
  (let* (
            (x 0)
            (y 0)
    )
    
    (while (< x length)
        (pa-put-segment layer (+ x x1) (+ y y1))
        
        (set! x (+ x 2))
        (set! y (- y 1))
    )
  )
)

(define (pa-draw-line-v layer x y h)
  (pa-draw-line layer x y x (- y (- h 1)))
)

; TODO refactor it
(define (pa-box img width height depth color-main color-shading color-lighting color-dark-border color-light-border)
  (let* (
          (w (pa-make-power-of-two width))
          (h (pa-make-power-of-two height))
          (d (pa-make-power-of-two depth))
          (w-half (/ w 2))
          (h-half (/ h 2))
          (new-layer-width (+ (+ w h) -2))
          (new-layer-height (+ (+ w-half h-half) -1))
          (x 0)
          (y 0)
          (ymax (- new-layer-height 1))
          (xmax (- new-layer-width 1))
          (layer 0)
        )
        
    ; apply depth if needed
    (if (> depth 1)
        (begin
            (set! new-layer-height (+ new-layer-height depth -1))
            (set! ymax (- new-layer-height 1))
        )
        (begin
            (set! depth 0)
            ;
            (set! ymax (- new-layer-height 1))
        )
    )
    
    (gimp-context-push)
    (gimp-image-undo-disable img)
    
    (set! layer (pa-add-layer img "box" new-layer-width new-layer-height))
        
    (pa-pixel-mode)
    
    (set! y (+ (- ymax w-half) 1))
    ;(gimp-message (number->string y))
    ;(gimp-message (number->string new-layer-height))
    ;(gimp-message (number->string depth))
    ; upper rectangle
    ;
    ;  bottom left
    (pa-color color-light-border)
    (pa-draw-line-down layer 0 (- h-half 1) w)
    ;
    ; bottom right
    (pa-color color-light-border)
    (pa-draw-line-up layer (- w 2) (+ (+ w-half h-half) -2) h)
    ;
    ; top right
    (pa-color color-dark-border)
    (pa-draw-line-down layer (- h 2) 0 w)
    ;
    ; top left
    (pa-color color-dark-border)
    (pa-draw-line-up layer 0 (- h-half 1) h)
    ;
    ; lower rectangle with vertical lines of box for Z-axis
    (if (> depth 1)
        (begin
            ; left vertical
            (pa-color color-dark-border)
            (pa-draw-line-v layer 0 (+ (+ h-half depth) -2) depth)
            ;
            ; middle vertical
            (pa-color color-light-border)
            (pa-draw-line-v layer (- w 1) (+ (+ (+ (+ w-half h-half) -2) depth) -1) depth)
            ;
            ; right vertical
            (pa-color color-dark-border)
            (pa-draw-line-v layer (+ (+ (+ w h) -1) -2) (+ (+ w-half -1) (+ depth -1)) depth)
            ;
            ; bottom left
            (pa-color color-dark-border)
            (pa-draw-line-down layer 0 (+ (- h-half 1) (+ depth -1)) w)
            ;
            ; bottom right
            (pa-color color-dark-border)
            (pa-draw-line-up layer (- w 2) (+ (+ (+ (+ w-half h-half) -2) depth) -1) h)
        )
    )
    ;
    ; fill with colors
    
    (if (> depth 1)
        (begin
            ; top wall fill
            (pa-color color-lighting)
            (pa-fill-by-fuzzy-select img layer 2 (- h-half 1))
            ;
            ; left wall fill
            (pa-color color-main)
            (pa-fill-by-fuzzy-select img layer 2 (+ (+ (- h-half 1) depth) -1))
            ; 
            ; right wall fill
            (pa-color color-shading)
            (pa-fill-by-fuzzy-select img layer (+ (+ w -2 ) 2) (+ (+ w-half h-half) -2))
        )
        (begin
            ; tile fill
            (pa-color color-main)
            (pa-fill-by-fuzzy-select img layer 2 (- h-half 1))
        )
    )
    
    (gimp-context-pop)
    (gimp-image-undo-enable img)
    
    (gimp-displays-flush)
  )
)

; ----- scripts registering -----

(script-fu-register "pa-box"
    "<Image>/File/Create/Isometric Pixel Art/Box OR Tile..."
    "Isometric Pixel Art :: Box OR Tile \nfile:isometric_pixel_art.scm"
    "Bartłomiej Wójtowicz"
    ":)"
    "2013"
    ""
    SF-IMAGE "Image" 0
    SF-VALUE "Width (X axis)"                           "50"
    SF-VALUE "Height (Y axis)"                          "50"
    SF-VALUE "Depth (Z axis, 0 or 1 for flat tile)"     "30"
    SF-COLOR "Main"                 '(82 176 255)
    SF-COLOR "Shading"              '(23 124 210)
    SF-COLOR "Lighting"             '(159 215 255)
    SF-COLOR "Dark edge border"     '(0 0 0)
    SF-COLOR "Light edge border"    '(255 255 255)
)
