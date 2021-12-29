;*************************************************************************************** 
; The GIMP -- an image manipulation program 
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Patchwork effect script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>  
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/12
;     - Initial relase
; version 0.2  by Iccii 2001/07/14
;     - Change to better algorithm
; version 0.3  by Iccii 2001/07/16
;     - Add the round option to create round tile
; version 0.4  by Iccii 2001/07/19
;     - Add Tile Type options to select tile type
; version 0.5  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added Angle option
; version 0.5a by Iccii 2001/10/02 <iccii@hotmail.com>
;     - Fixed bug in keeping transparent area
; version 0.5b by Iccii 2001/10/02 <iccii@hotmail.com>
;     - Fixed bug (get error when drawable doesn't have alpha channel)
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

	;; パッチワークスクリプト
(define (script-fu-patchwork-ga
            img         ;
            drawable    ;
            type        ;
            size       ; 
            depth       ;
            angle       ;
            level       ;
    )
    
   (gimp-image-undo-group-start img)
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-palette-get-foreground)))
            (old-selection (car (gimp-selection-save img)))
            (tmp-layer1 (car (gimp-layer-copy drawable TRUE)))
            (tmp-layer2 (car (gimp-layer-copy drawable TRUE)))
            (final-layer (car (gimp-layer-copy drawable TRUE)))
            (depth-color (list depth depth depth))
            (radius (- (/ size 2) 1))
            (blur   (cond ((= type 0) 1) ((= type 1) 0) ((= type 2) 0) ((= type 3) 0)))
            (hwidth (cond ((= type 0) 1) ((= type 1) 0) ((= type 2) 2) ((= type 3) 1)))
            (vwidth (cond ((= type 0) 1) ((= type 1) 2) ((= type 2) 0) ((= type 3) 1)))
        ) ; end variable definition
    
    ;;
; *** The next 2 lines were moved here by EV
    (gimp-image-insert-layer img tmp-layer1 0 -1)
    (gimp-image-insert-layer img tmp-layer2 0 -1)
    (gimp-drawable-desaturate tmp-layer2 DESATURATE-LIGHTNESS)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE)
        (begin
          (gimp-selection-invert img)
          (gimp-edit-clear tmp-layer2)
          (gimp-selection-invert img)
        )
    )
; *** The next 2 lines were moved upwards
;    (gimp-image-add-layer img tmp-layer1 -1)
;    (gimp-image-add-layer img tmp-layer2 -1)
    
    ;;
    (plug-in-noisify 1 img tmp-layer2 FALSE 1.0 1.0 1.0 0)
    (plug-in-pixelize 1 img tmp-layer1 size)
    (plug-in-pixelize 1 img tmp-layer2 size)
    (gimp-drawable-levels tmp-layer2 HISTOGRAM-VALUE (/ (+ 32 (* level 2)) 255) (/ (- 223 (* level 2)) 255) TRUE 1.0 0.0 1.0 TRUE)
    (plug-in-grid 1 img tmp-layer2 hwidth size 0 depth-color 255
                            vwidth size 0 depth-color 255
                            0      0    0 '(0 0 0)    255)
    ;; 
    (if (= type 3)
        (begin
            (let* ((selection-channel (car (gimp-selection-save img))))
                (gimp-context-set-foreground depth-color)
                (gimp-by-color-select tmp-layer2 depth-color 0 REPLACE FALSE 0 0 FALSE)
                (gimp-selection-grow img radius)
                (gimp-selection-shrink img radius)
                (gimp-edit-fill tmp-layer2 FILL-FOREGROUND)
                (gimp-selection-load selection-channel)
                (gimp-image-remove-channel img selection-channel)
                (gimp-image-set-active-layer img tmp-layer2)          ;; why do I need this line?
                (gimp-context-set-foreground old-fg)
            )
        )
    )
    (plug-in-gauss-iir2 1 img tmp-layer2 (+ blur hwidth) (+ blur vwidth))
    (plug-in-bump-map 1 img tmp-layer2 tmp-layer2 angle
                      30 (+ 4 level) 0 0 0 0 TRUE FALSE 1)
    
    ;;
    (gimp-layer-set-mode tmp-layer2 LAYER-MODE-OVERLAY)
    (gimp-layer-set-opacity tmp-layer2 (+ level 84))
    (set! final-layer (car (gimp-image-merge-down img tmp-layer2 EXPAND-AS-NECESSARY)))
    (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
        (gimp-selection-layer-alpha drawable))
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE)
        (begin
          (gimp-selection-invert img)
          (gimp-edit-clear final-layer)))
    (gimp-selection-load old-selection)
    (gimp-edit-copy final-layer)
    (gimp-image-remove-layer img final-layer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste drawable 0)))
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    ;;
    (gimp-context-set-foreground old-fg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  ) ; end of let*
) ; end of define

(script-fu-register
    "script-fu-patchwork-ga"
    "<Toolbox>/Script-Fu/Decor/Texture/Patchwork..."
    "Creates patchwork image, which simulates Photoshop's Patchwork filter. \n photoeffects_246_02_part3.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Oct"
    "RGB*"
    SF-IMAGE       "Image"          0
    SF-DRAWABLE    "Drawable"       0
    SF-OPTION      "Tile Type"      '("Normal" "Horizontal" "Vertical" "Round")
    SF-ADJUSTMENT  _"Block Size"    '(10 2 127 1 10 0 1)
    SF-ADJUSTMENT  _"Depth"         '(127 0 255 1 10 0 1)
    SF-ADJUSTMENT  _"Angle"         '(135 0 360 1 10 0 0)
    SF-ADJUSTMENT  _"Level"         '(8 0 16 1 2 0 0)
)

;*************************************************************************************** 
; Stained glass script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; You'll find that this script isn't "real" staind glass effect
; Plese tell me how to create if you know more realistic effect
; This script is only applying the mosac plugin ;-(
; --> Eddy Verlinden : tile spacing bigger and light-direction set to 270 + added copy-layer3 in screenmode
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/21
;     - Initial relase
; this version 9 april 2006
; --------------------------------------------------------------------



;;
(define (script-fu-stained-glass-ga
            img         ;; image
            drawable    ;; drawable
            tile-size   ;; size of the tiles
    )
    (gimp-image-undo-group-start img)
; one variable added in the next line; and some other parameters are changed
; (plug-in-mosaic 1 img drawable tile-size 0 1.0 0.65 90.0 0.25 TRUE TRUE 1 0 0)
;  (plug-in-mosaic 1 img drawable tile-size 0 2.5 0.65 0 270.0 0.25 TRUE TRUE 1 0 0)
   (plug-in-mosaic 1 img drawable tile-size tile-size 2.5 0.65 0 270.0 0.25 TRUE TRUE 1 0 0)
  (let* (
            (copy-layer1 (car (gimp-layer-copy drawable 1)))
            (copy-layer2 (car (gimp-layer-copy drawable 1)))
            (copy-layer3 (car (gimp-layer-copy drawable 1)))
        )
        
    (gimp-image-insert-layer img copy-layer1 0 -1)
    (gimp-image-insert-layer img copy-layer2 0 -1)
    (gimp-layer-set-mode copy-layer1 LAYER-MODE-OVERLAY)
    (gimp-layer-set-mode copy-layer2 LAYER-MODE-OVERLAY)
    (gimp-layer-set-opacity copy-layer1 100)
    (gimp-layer-set-opacity copy-layer2 100)
    (gimp-image-merge-down img
      (car (gimp-image-merge-down img copy-layer2 EXPAND-AS-NECESSARY))
                               EXPAND-AS-NECESSARY)
    
    
    (gimp-image-add-layer img copy-layer3 -1)
    (gimp-layer-set-mode copy-layer3 LAYER-MODE-SCREEN-LEGACY)
    (gimp-layer-set-opacity copy-layer3 100)
    (gimp-image-merge-down img copy-layer3 EXPAND-AS-NECESSARY)
  )
    
    
    
    ;;finish and display
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
)

(script-fu-register "script-fu-stained-glass-ga"
    "<Toolbox>/Script-Fu/Decor/Texture/Stained Glass..."
    "Create stained glass image. \n file:ev_icci_photoeffects_246_02_part3.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Jul"
    "RGB*"
    SF-IMAGE      "Image"         0
    SF-DRAWABLE   "Drawable"      0
    SF-ADJUSTMENT "Cell Size (pixels)"   '(18 5 100 1 10 0 1)
)

;end of script  