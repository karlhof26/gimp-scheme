;*************************************************************************************** 
; Texturizer script  for GIMP 2.10.14  
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
;
; ; This program is free software; you can redistribute it and/or modify
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
;
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/09/26 <iccii@hotmail.com>
;     - Initial relase
;     - There are three texture type -- Sand, Paper, Cloud
; version 0.2  by Iccii 2001/09/27 <iccii@hotmail.com>
;     - Create the texture image as new window image instead of
;       creating layer in base image
;     - Added Depth option
; version 0.2a by Iccii 2001/09/30 <iccii@hotmail.com>
;     - Added Pattern option in texture type
; version 0.3  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added Angle option
; version 0.4 by karlhof26 2020/02/16
;     - Changed modes etc for later gimp versions
; --------------------------------------------------------------------
; 


	;; テクスチャスクリプト
(define (script-fu-texturizer-ga
            img         ;; 対象画像
            drawable    ;; 対象ドロアブル (レイヤー)
            pattern     ;; パターン
            bg-type     ;; 背景のテクスチャの種類
            angle       ;; バンプの角度
            elevation   ;; 彫りの深さ
            direction   ;; テクスチャ伸張の方向
            invertY     ;; 凹凸を反転させるかどうか
            show?       ;; テクスチャ画像を表示するかどうか
    )
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-palette-get-foreground)))
            (old-pattern (car (gimp-context-get-pattern))) 
            (tmp-image (car (gimp-image-new width height GRAY)))
            (tmp-layer (car (gimp-layer-new tmp-image width height 2 "Texture" 100 LAYER-MODE-NORMAL)))  ; was 'GRAY-Image "Texture" 100 NORMAL'
            (overlayerkh 0)
        ) ; end variable definition
    
    ;; 本処理
    (gimp-image-undo-group-start img)
    (gimp-image-undo-disable tmp-image)
   ; (if (eqv? (car (gimp-drawable-is-layer-mask drawable)) TRUE)
   ;     (set! layer (car (gimp-image-get-active-layer img drawable))))
    (gimp-drawable-fill tmp-layer FILL-WHITE)
    (gimp-image-insert-layer tmp-image tmp-layer 0 0)
    
    (cond
      ((eqv? bg-type 0)
         (gimp-context-set-pattern pattern)
         (gimp-edit-bucket-fill tmp-layer BUCKET-FILL-PATTERN LAYER-MODE-NORMAL 100.0 0.0 FALSE 0 0)
      )
      ((eqv? bg-type 1)
         (begin
            (gimp-message "sand...")
            ;;(plug-in-noisify 1 img tmp-layer TRUE 0.9 1.0 1.0 0.0) ; was FALSE 1.0 1.0 1.0 0
            ;;(plug-in-rgb-noise 1 img tmp-layer FALSE FALSE 0.9 0.8 0.99 0.0) ; was FALSE 1.0 1.0 1.0 0
            (plug-in-solid-noise 1 img tmp-layer FALSE FALSE (rand 65532) 10 9 23) ; was 10 9 26
            (plug-in-rgb-noise 1 img tmp-layer FALSE FALSE 0.9 0.8 0.99 0.0) ; was FALSE 1.0 1.0 1.0 0
            (gimp-drawable-brightness-contrast tmp-layer 0.0 0.25)
         )
      )
      ((eqv? bg-type 2)
         (gimp-message "Paper...")
         (plug-in-solid-noise 1 img tmp-layer FALSE FALSE (rand 65535) 15 16 16)
         ; last parameter added by EV (for GIMP 2)
         (plug-in-edge 1 img tmp-layer 4 1 5)
         (gimp-drawable-brightness-contrast tmp-layer 0.0 -0.25)
      )
      ((eqv? bg-type 3)
         ;(plug-in-plasma 1 img tmp-layer (rand 65535) 4.0)
         (plug-in-solid-noise 1 img tmp-layer FALSE FALSE (rand 65535) 4 4 4)
         (plug-in-gauss-iir2 1 img tmp-layer 1 1)
         (gimp-drawable-brightness-contrast tmp-layer 0.0 0.241)
      )
    ) ; end of cond
    
    (plug-in-bump-map 1 img drawable tmp-layer angle (+ 35 elevation)
                      1 0 0 0 0 TRUE invertY 0)
        
        
        
    ; 後処理
     (cond
    ; If Drawable is Layer
       ((eqv? (car (gimp-drawable-is-layer drawable)) TRUE)
         (gimp-image-set-active-layer img drawable))
    ; If Drawable is Layer mask
       ((eqv? (car (gimp-drawable-is-layer-mask drawable)) TRUE)
         (gimp-image-set-active-layer img layer))
    ; If Drawable is Channel
       ((eqv? (car (gimp-drawable-is-channel drawable)) TRUE)
         (gimp-image-set-active-channel img drawable))
     ) ; end of cond
    
    ; karlhof26 new section
    ;(gimp-message "entering new section")
    ;(gimp-display-new tmp-image)
    (set! overlayerkh (car (gimp-layer-new-from-visible tmp-image img "Texturizer overlayer kh")))
    (gimp-image-insert-layer img overlayerkh 0 -1)
    (gimp-layer-set-mode (car (gimp-image-get-active-layer img)) LAYER-MODE-MULTIPLY)
    (gimp-displays-flush)
    
    (gimp-layer-set-mode tmp-layer LAYER-MODE-MULTIPLY)
    
    ;(gimp-message "success")
    
    (gimp-context-set-foreground old-fg)
    (gimp-context-set-pattern old-pattern)
    ;;(gimp-image-clean-all tmp-image)
    (gimp-image-undo-enable tmp-image)
    (if (eqv? show? TRUE)
        (gimp-display-new tmp-image)
        (begin
            ;(gimp-image-delete tmp-image)
        )
    )
    
    (gimp-message "Good finish OK")
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    
    (gc) ; memory garbage cleaner
  )
)

(script-fu-register
   "script-fu-texturizer-ga"
   "Texturizer..."
   "Creates textured canvas image, which simulates Photoshop's Texturizer filter \nFile:photoeffects_246_02_part4.scm"
   "Iccii <iccii@hotmail.com>"
   "Iccii"
   "2001, Oct"
   "RGB* GRAY*"
   SF-IMAGE      "Image"            0
   SF-DRAWABLE   "Drawable"         0
   SF-PATTERN    "Use Pattern"      "Pine"
   SF-OPTION     "Texture Type"     '("Pattern" "Sand" "Paper" "Cloud")
   SF-ADJUSTMENT "Angle"            '(135 0 360 1 10 0 0)
   SF-ADJUSTMENT "Depth"            '(0 -5 5 1 1 0 1)
   SF-OPTION     "Stretch Direction" '("None" "Horizontal" "Vertical")
   SF-TOGGLE     "Invert"           FALSE
   SF-TOGGLE     "Show Texture"     TRUE
)

(script-fu-menu-register "script-fu-texturizer-ga" "<Toolbox>/Script-Fu/Decor/Texture/")

;*************************************************************************************** 
; Water paint effect script  for GIMP 1.2
;   <Image>/Filters/Alchemy/Water Paint Effect...
;
; --------------------------------------------------------------------
;   - Changelog -
; version 0.1  2001/04/15 iccii <iccii@hotmail.com>
;     - Initial relased
; version 0.1a 2001/07/20 iccii <iccii@hotmail.com>
;     - more simple
;
; --------------------------------------------------------------------
; 

(define (script-fu-water-paint-effect-ga
            inImage
            inDrawable
            inEffect
    )
    
    
  (let* (
        (currentselection (car (gimp-selection-save inImage))) 
        (theNewlayer (car (gimp-layer-copy inDrawable TRUE)))
        (tmp-layer 0)
        (finalLayer 0)
      )
      
    (gimp-image-undo-group-start inImage)   
    
    ;(gimp-selection-none inImage)
    (set! tmp-layer (car (gimp-layer-copy inDrawable TRUE)))
    (gimp-image-insert-layer inImage tmp-layer 0 -1)
    
    
    (plug-in-gauss-iir2 1 inImage inDrawable inEffect inEffect)
    (set! theNewlayer (car (gimp-layer-copy inDrawable TRUE)))
    (gimp-image-insert-layer inImage theNewlayer 0 -1)
    
    (plug-in-unsharp-mask 1 inImage theNewlayer 5.0 10.0 0) ; was 5.0 10.0 0
    ;(plug-in-laplace 1 inImage theNewlayer)
    (plug-in-neon 1 inImage theNewlayer 6.2 0.25)
    (gimp-layer-set-mode theNewlayer LAYER-MODE-SUBTRACT-LEGACY)
    
    (gimp-message "new items actioned")
    (gimp-layer-set-name theNewlayer "newLayer")
    (gimp-layer-set-name tmp-layer "tempLayer")
    (gimp-drawable-levels tmp-layer HISTOGRAM-VALUE 0.3 1.0 TRUE 1.0 0.0 1.0 TRUE)
    (gimp-drawable-brightness-contrast tmp-layer 0.31 0.45)
    
    (gimp-layer-set-mode tmp-layer LAYER-MODE-SUBTRACT)
    
    (gimp-selection-load currentselection)
    (if (equal? (car (gimp-selection-is-empty inImage)) FALSE) 
        (begin
            (gimp-selection-invert inImage)
            (if (equal? (car (gimp-selection-is-empty inImage)) FALSE)
                (gimp-edit-fill theNewlayer FILL-TRANSPARENT)
            )
            (gimp-selection-invert inImage)
        )
    )
    (gimp-image-remove-channel inImage currentselection)
    
    (set! finalLayer (car (gimp-image-flatten inImage)))
    
    ; added by karlhof26 
    (gimp-drawable-hue-saturation finalLayer HUE-RANGE-ALL 0.0 10.1 80.3 5.0)
    (gimp-drawable-levels-stretch finalLayer)
    
    (gimp-message "water paint done")
    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
    
    (gc); Memory garbage cleanup
  )
)

(script-fu-register
    "script-fu-water-paint-effect-ga"
    "<Toolbox>/Script-Fu/Decor/Artistic/Water Paint Effect..."
    "Draws a water paint effect. Requires a layer. \nFile:photoeffects_246_02_part4.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "Jul, 2001"
    "RGB*, GRAY*"
    SF-IMAGE            "Image"             0
    SF-DRAWABLE         "Drawable"          0
    SF-ADJUSTMENT       "Effect Size (pixels)"      '(5 0 60 1 10 0 0)
)

;end of script