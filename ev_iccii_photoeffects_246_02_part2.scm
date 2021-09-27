;*************************************************************************************** 
; Note paper image script  for GIMP 1.2 
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/22
;     - Initial relase 
; version 0.1a by Iccii 2001/07/26
;     - Add Background color selector
; version 0.1b by Iccii 2001/09/25
;     - Add Cloud option in Background Texture
; version 0.2  by Iccii 2001/10/01 <iccii@hotmail.com>
;     - Changed menu path because this script attempts to PS's filter
;     - Added some code (if selection exists...)
; version 0.2a by Iccii 2001/10/02 <iccii@hotmail.com>
;     - Fixed bug in keeping transparent area
; version 0.2b by Iccii 2001/10/02 <iccii@hotmail.com> 
;     - Fixed bug (get error when drawable doesn't have alpha channel)
;
; --------------------------------------------------------------------


    ;; ノート用紙スクリプト
(define (script-fu-note-paper-gb
            img         ;; 対象画像
            drawable    ;; 対象ドロアブル (レイヤー)
            threshold1  ;; 閾値1
            threshold2  ;; 閾値2
            base-color  ;; 着色する色
            bg-color    ;; 背景の色
            bg-type     ;; 背景のテクスチャの種類
    )
    
  (let* (
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            (old-fg (car (gimp-palette-get-foreground)))
            (old-selection (car (gimp-selection-save img)))
            (layer-copy1 (car (gimp-layer-copy drawable TRUE)))
            (layer-copy2 (car (gimp-layer-copy drawable TRUE)))
            (layer-color1 (car (gimp-layer-new img width height RGBA-IMAGE "Color Upper" 100 LAYER-MODE-MULTIPLY-LEGACY)))
            (color-mask1 (car (gimp-layer-create-mask layer-color1 ADD-MASK-WHITE)))
            (layer-color2 (car (gimp-layer-new img width height RGBA-IMAGE "Color Under" 100 LAYER-MODE-MULTIPLY-LEGACY)))
            (color-mask2 (car (gimp-layer-create-mask layer-color2 ADD-MASK-WHITE)))
            (invertY FALSE) ; add by KH
            (final-layer2 (car (gimp-layer-new img width height RGBA-IMAGE "Final layer2" 100 LAYER-MODE-NORMAL))) ; added by KH
        ) ; end variable definition
        
    ;; start-up
    (gimp-image-undo-group-start img)
    (gimp-selection-none img)
    (gimp-image-insert-layer img layer-copy1 0 -1)
    (gimp-image-insert-layer img layer-copy2 0 -1)
    (gimp-layer-set-name layer-copy1 "Layer copy1")
    (gimp-layer-set-name layer-copy2 "Layer copy2")
    (gimp-drawable-desaturate layer-copy2 DESATURATE-LUMINANCE)
    (gimp-drawable-desaturate layer-copy1 DESATURATE-LIGHTNESS)
    
    (cond
      ((eqv? bg-type 0)
         (gimp-edit-fill layer-copy1 FILL-WHITE)
         (gimp-drawable-brightness-contrast layer-copy1 0.0 0.25)) ; was 0 63
      ((eqv? bg-type 1)
         (gimp-edit-fill layer-copy1 FILL-WHITE)
         (plug-in-noisify 1 img layer-copy1 FALSE 1.0 1.0 1.0 0)
         (gimp-drawable-brightness-contrast layer-copy1 0.0 0.25))
      ((eqv? bg-type 2)
         (plug-in-solid-noise 1 img layer-copy1 FALSE FALSE (rand 65535) 15 16 16)
         (plug-in-edge 1 img layer-copy1 4 1 4)  ; ev: needed too add the type (new plug-in)
         (gimp-drawable-brightness-contrast layer-copy1 0.0 -0.25))
      ((eqv? bg-type 3)
         (plug-in-plasma 1 img layer-copy1 (rand 65535) 4.0)
         (gimp-drawable-desaturate layer-copy1 DESATURATE-LIGHTNESS)
         (plug-in-gauss-iir2 1 img layer-copy1 1 1)
         (gimp-drawable-brightness-contrast layer-copy1 0.0 0.25)
      )
    ) ; end of cond
    (if (> threshold1 threshold2)
        (begin                  ;; always (threshold1 < threshold2)
          (set! tmp threshold2)
          (set! threshold2 threshold1)
          (set! threshold1 tmp)
          (set! invertY TRUE)
        )
        (set! invertY FALSE)
    )
    (if (= threshold1 threshold2)
        (gimp-message "Execution error:\n Threshold1 equals to threshold2!")
        (gimp-drawable-threshold layer-copy2 HISTOGRAM-VALUE (/ threshold1 255) (/ threshold2 256))
    )
    (gimp-edit-copy layer-copy2)
    (plug-in-bump-map 1 img layer-copy1 layer-copy1 135 35 3 0 0 0 0 TRUE FALSE 1)
    (plug-in-bump-map 1 img layer-copy1 layer-copy2 135 35 3 0 0 0.5 0.5 FALSE invertY 1); was 0 0 FALSE invertY 0
    (gimp-drawable-brightness-contrast layer-copy2 0.45 0.0) ; was 0.5 0.0
    
    ;;
    (gimp-image-insert-layer img layer-color1 0 -1)
    (gimp-layer-add-mask layer-color1 color-mask1)
    (gimp-context-set-foreground base-color)
    (gimp-drawable-fill layer-color1 FILL-FOREGROUND)
    (gimp-floating-sel-anchor (car (gimp-edit-paste color-mask1 0)))
    (gimp-image-insert-layer img layer-color2 0 -1)
    (gimp-layer-add-mask layer-color2 color-mask2)
    (gimp-context-set-foreground bg-color)
    (gimp-drawable-fill layer-color2 FILL-FOREGROUND)
    (gimp-floating-sel-anchor (car (gimp-edit-paste color-mask2 0)))
    (gimp-drawable-invert color-mask2 FALSE)
    
    (gimp-image-insert-layer img final-layer2 0 -1)
    
    ;; レイヤー後始末
    ;(gimp-layer-set-mode layer-copy2 LAYER-MODE-SCREEN-LEGACY)
    (gimp-layer-set-mode layer-copy2 LAYER-MODE-SOFTLIGHT)
    (gimp-layer-set-opacity layer-copy2 55.0) ; was 75
    
    ;(gimp-image-merge-down img layer-copy2 EXPAND-AS-NECESSARY)
    ;(gimp-image-merge-down img layer-color1 EXPAND-AS-NECESSARY)
    ;(gimp-image-merge-down img layer-color2 EXPAND-AS-NECESSARY)
    
    (set! final-layer2 (car (gimp-image-get-active-layer img)))
    (plug-in-bump-map 1 img final-layer2 final-layer2 135 45 3 0 0 0 0 TRUE FALSE 3) ; added ev;  was true faLSE 0
    (if (eqv? (car (gimp-drawable-has-alpha drawable)) TRUE)
        (gimp-selection-layer-alpha drawable)
    )
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE)
        (begin
          (gimp-selection-invert img)
          (gimp-edit-clear final-layer2)
        )
    )
    (gimp-selection-load old-selection)
    (gimp-edit-copy final-layer2)
    (gimp-image-remove-layer img final-layer2)
    (gimp-floating-sel-anchor (car (gimp-edit-paste drawable 0)))
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)
    
    
    ;; final actions
    (gimp-context-set-foreground old-fg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc); garbage cleanup
  )
)

(script-fu-register "script-fu-note-paper-gb"
    "Note Paper..."
    "Creates note paper, which simulates Photoshop's Textureizer filter \nfile:ev_iccii_photoeffects_246_02_part2.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Oct "
    "RGB*"
    SF-IMAGE      "Image"            0
    SF-DRAWABLE   "Drawable"         0
    SF-ADJUSTMENT "Threshold (Bigger 1<-->255 Smaller)"    '(127 0 255 1 10 0 0)
    SF-ADJUSTMENT "Threshold (Bigger 1<-->255 Smaller)"    '(255 0 255 1 10 0 0)
    SF-COLOR      "Base Color"            '(255 255 255)
    SF-COLOR      "Background Color"      '(221 221 221)
    SF-OPTION     "Background Texture"    '("Plain" "Sand" "Paper" "Cloud")
)

(script-fu-menu-register "script-fu-note-paper-gb" "<Toolbox>/Script-Fu/Artistic")

;*************************************************************************************** 
; Pastel image script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
;  
; This script is based on pastel-windows100.scm
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/10/19 <iccii@hotmail.com>
;     - Initial relase
;
; --------------------------------------------------------------------
;     Reference Book
; Windows100% Magazine October, 2001
;   Tamagorou's Photograph touching up class No.29
;     theme 1 -- Create the Pastel image
; --------------------------------------------------------------------
; 

(define (script-fu-pastel-image-ga
            img         
            drawable        
            Dbord           
            detail          
            lengthB          
            amount          
            angleB           
            canvas?         
    )
    
  (let* (
            (Dbordx  (cond ((= Dbord 0) 4) ((= Dbord 1) 0) ((= Dbord 2) 1) ((= Dbord 3) 2) ((= Dbord 4) 3) ((= Dbord 5) 5)  ))
            (old-selection (car (gimp-selection-save img)))
            (layer-copy0 (car (gimp-layer-copy drawable TRUE)))
            (dummy (if (< 0 (car (gimp-layer-mask layer-copy0)))
                  (gimp-image-remove-layer-mask img layer-copy0 MASK-DISCARD)))
            (layer-copy1 (car (gimp-layer-copy layer-copy0 TRUE)))
            (lengthA (if (<= lengthB 1) 0 lengthB))
            (layer-copy2 (car (gimp-layer-copy layer-copy0 TRUE)))
            (marged-layer (car (gimp-layer-copy layer-copy0 TRUE))) ; added by KH 
            (final-layer (car (gimp-layer-copy layer-copy0 TRUE)))  ; added by KH
            (width (car (gimp-image-width img)))
      )
     
    (gimp-image-undo-group-start img)
    (gimp-image-insert-layer img layer-copy0 0 -1)
    (gimp-image-insert-layer img layer-copy2 0 -1)
    (gimp-image-insert-layer img marged-layer 0 -1)
    (gimp-image-insert-layer img layer-copy1 0 -1)
    
    ;inserted by KH
    
    (gimp-image-insert-layer img final-layer 0 -1)
    (gimp-item-set-name layer-copy0 "layer copy 0")
    (gimp-item-set-name layer-copy1 "layer copy 1")
    (gimp-item-set-name layer-copy2 "layer copy 2")
    (gimp-item-set-name marged-layer "marged layer")
    (gimp-item-set-name final-layer "final layer")
    
    ;(gimp-message (number->string lengthA))
    ;(gimp-message (number->string angleB))
    ;(plug-in-mblur TRUE img layer-copy0 0 lengthA angleB TRUE TRUE)
    (plug-in-mblur 1 img layer-copy0 0 lengthA angleB 500 500) 
    ;       
    (gimp-displays-flush)
    
    
    (plug-in-mblur 1 img layer-copy0 0 lengthA (+ angleB 100) (/ width 2) 500)
    
    (plug-in-gauss-iir 1 img layer-copy1 (- 22 detail) TRUE TRUE)
    (plug-in-edge TRUE img layer-copy1 6.0 0 Dbordx)  
    (gimp-layer-set-mode layer-copy1 LAYER-MODE-PIN-LIGHT) ; was DIVIDE-LEGACY
    ;(gimp-image-merge-down img layer-copy1 EXPAND-AS-NECESSARY)
    
    
    (set! marged-layer (car (gimp-image-get-active-layer img)))
    ; dont use (set! marged-layer (car (gimp-image-merge-down img layer-copy1 EXPAND-AS-NECESSARY)))
    (gimp-layer-set-mode marged-layer LAYER-MODE-HSV-VALUE-LEGACY)
    
    (if (equal? canvas? TRUE)
        (plug-in-apply-canvas TRUE img marged-layer 0 5)
    )
    (plug-in-unsharp-mask TRUE img layer-copy0 (+ 1 (/ lengthA 5)) amount 0)
    ;(plug-in-unsharp-mask TRUE img marged-layer (+ 1 (/ length 5)) amount 0)
    ;(gimp-image-merge-down img marged-layer EXPAND-AS-NECESSARY)
    ; (gimp-image-merge-down img layer-copy0 EXPAND-AS-NECESSARY)
    (set! final-layer (car (gimp-image-get-active-layer img)))
    ; dont use (set! final-layer (car (gimp-image-merge-down img marged-layer EXPAND-AS-NECESSARY)))
    
    
    (gimp-selection-load old-selection)
    (gimp-edit-copy final-layer)
    ;Kh edit
    ;(gimp-image-set-active-layer img final-layer) 
    ;(gimp-item-set-visible layer-copy0 FALSE)
    (gimp-item-set-visible final-layer TRUE)
    (gimp-item-set-visible marged-layer TRUE)
    ;(gimp-item-set-visible layer-copy1 FALSE)
    (gimp-item-set-visible layer-copy2 TRUE)
    (gimp-image-set-active-layer img layer-copy1)
    ;(gimp-message "here")
    (gimp-layer-set-mode marged-layer LAYER-MODE-VIVID-LIGHT)
    (gimp-drawable-fill layer-copy2 FILL-WHITE)
    ;(gimp-message "here2")
    (gimp-displays-flush)
    ;(set! layer-copy1 (car (gimp-image-get-active-layer img)))
    ;(gimp-layer-set-mode layer-copy0 LAYER-MODE-HARD-MIX)
    ;end KH edit
    
    (gimp-displays-flush)
    
    
;    (gimp-image-remove-layer img final-layer)                     ;let's keep the original layer
;    (gimp-floating-sel-anchor (car (gimp-edit-paste drawable 0)))  
    
    ;(gimp-selection-load old-selection)
    ;(gimp-image-remove-channel img old-selection)
    
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc)
  )
)

(script-fu-register
  "script-fu-pastel-image-ga"
  "Pastel Line Mix..."
  "Creates a Pastel Line Mix image. Lines are emphasized in pastel colors. \nfile:ev_iccii_photoeffects_246_02_part2.scm"
  "Iccii <iccii@hotmail.com>"
  "Iccii"
  "2001, Oct"
  "RGB*"
  SF-IMAGE      "Image"	         0
  SF-DRAWABLE   "Drawable"       0
  SF-OPTION     "Edge detection" '("Differential" "Sobel" "Prewitt" "Gradient" "Roberts" "Laplace")
  SF-ADJUSTMENT "Detail Level"   '(12.0 0 20.0 0.1 0.5 1 1)
  SF-ADJUSTMENT "Sketch Length" '(10 0 32 1 1 0 1)
  SF-ADJUSTMENT "Sketch Amount" '(5.0 0 5.0 0.1 0.5 1 1)
  SF-ADJUSTMENT "Angle"          '(45 0 180 1 15 0 0)
  SF-TOGGLE     "Add a canvas texture" FALSE
)
 
(script-fu-menu-register "script-fu-pastel-image-ga" "<Toolbox>/Script-Fu/Artistic")

;end of script