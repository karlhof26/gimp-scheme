;*************************************************************************************** 
; Scroll script  for GIMP 2.10.18
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/12/08
;     - Initial relase
; version 0.2  by karlhof26 2020/04/09
;     - fixed fro Gimp 2.10.18

; --------------------------------------------------------------------
; 
;; Scroll

(define (script-fu-scroll-ga
            img
            drawable
            width
            height
            wrap-type
    )
    
    
  (let* (
            (old-fg (car (gimp-context-get-foreground)))
            (drawable-width (car (gimp-drawable-width drawable)))
            (drawable-height (car (gimp-drawable-height drawable)))
            (offset-x (min drawable-width width))       ; <- needs abs
            (offset-y (min drawable-height  height))    ; <- needs abs
        ) ; end variable definition
    
    (gimp-image-undo-group-start img)
    
    (cond ((= wrap-type 0)
                (gimp-drawable-offset drawable FALSE OFFSET-BACKGROUND offset-x offset-y))
          ((= wrap-type 1)
                (gimp-drawable-offset drawable FALSE OFFSET-TRANSPARENT offset-x offset-y))
          ((= wrap-type 2)
                (gimp-drawable-offset drawable TRUE OFFSET-TRANSPARENT offset-x offset-y))
          ((= wrap-type 3)
             (gimp-drawable-offset drawable FALSE OFFSET-TRANSPARENT offset-x offset-y)
             ;(gimp-rect-select img (if (> 0 offset-x) 0 offset-x)
             ;                      (if (> 0 offset-y) (+ drawable-height offset-y -1) offset-y)
             ;                      (- drawable-width (abs offset-x)) 1 REPLACE FALSE 0)
             (gimp-image-select-rectangle img CHANNEL-OP-REPLACE
                                   (if (> 0 offset-x) 0 offset-x)
                                   (if (> 0 offset-y) (+ drawable-height offset-y -1) offset-y)
                                   (- drawable-width (abs offset-x))
                                   1   )
    ;             (set! float (car (gimp-selection-float drawable
    ;                                                    0 (if (> 0 offset-y) 0 (- offset-y)))))
      (let* (
                (float (car (gimp-selection-float drawable
                                                0 (if (> 0 offset-y) 0 (- offset-y)))))
            ) ; end float definition                                    
             (gimp-floating-sel-to-layer float)
             
             ;; gimp-layer-scale TRUE
             ;; FALSE
             ;; offset
             (gimp-layer-scale float (- drawable-width (abs offset-x)) (+ (abs offset-y) 1) FALSE)
             
             (set! drawable (car (gimp-image-merge-down img float EXPAND-AS-NECESSARY)))
             ;(gimp-rect-select img (if (> 0 offset-x) (+ drawable-width offset-x -1) offset-x)
             ;                      (if (> 0 offset-y) 0 offset-y)
             ;                      1 (- drawable-height (abs offset-y)) REPLACE FALSE 0)
             (gimp-image-select-rectangle img CHANNEL-OP-REPLACE
                                   (if (> 0 offset-x) (+ drawable-width offset-x -1) offset-x)
                                   (if (> 0 offset-y) 0 offset-y)
                                   1 
                                   (- drawable-height (abs offset-y))   )
             (set! float (car (gimp-selection-float drawable
                                                    (if (> 0 offset-x) 0 (- offset-x)) 0)))
             (gimp-floating-sel-to-layer float)
             (gimp-layer-scale float (+ (abs offset-x) 1) (- drawable-height (abs offset-y)) FALSE)
             (set! drawable (car (gimp-image-merge-down img float EXPAND-AS-NECESSARY)))
             ;(gimp-rect-select img (if (> 0 offset-x) (+ drawable-width offset-x) 0)
             ;                      (if (> 0 offset-y) (+ drawable-height offset-y) 0)
             ;                      (abs offset-x) (abs offset-y) REPLACE FALSE 0)
             (gimp-image-select-rectangle img CHANNEL-OP-REPLACE
                                    (if (> 0 offset-x) (+ drawable-width offset-x) 0)
                                    (if (> 0 offset-y) (+ drawable-height offset-y) 0)
                                    (abs offset-x)
                                    (abs offset-y)    )
             (gimp-image-pick-color img drawable ; was ...color-picker
                                (if (> 0 offset-x) (+ drawable-width offset-x) offset-x)
                                (if (> 0 offset-y) (+ drawable-height offset-y) offset-y)
                                FALSE FALSE 1)  
             (gimp-edit-fill drawable FILL-FOREGROUND)
             (gimp-selection-none img)
        ) ; end of let float...
      ) ; end of cond 3 
    ) ; end of cond
    
    (gimp-palette-set-foreground old-fg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-scroll-ga"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/The Others/Scroll..."
    "Apply scroll, which simulates Photoshop's Photocopy filter. \n file: photoeffects_246_02_part8.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Dec"
    "RGB* GRAY*"
    SF-IMAGE        "Image"       0
    SF-DRAWABLE     "Drawable"    0
    SF-ADJUSTMENT   "Offset X"    '(10 -1000 1000 1 1 0 1)
    SF-ADJUSTMENT   "Offset Y"    '(10 -1000 1000 1 1 0 1)
    SF-OPTION       "Wrap Method" '("BG Color" "Transparent"
                                    "Wrap Around" "Repeat Edge Pixels")
)

;*************************************************************************************** 
; Funky color script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/12/03
;     - Initial relase
; version 0.1a by Iccii 2001/12/08
;     - Now only affects to selection area
; version 0.2  by karlhof26 2020/04/09
;     - fixed for Gimp 2.10.18
;
; --------------------------------------------------------------------
; 

(define (apply-easy-glowing-effect
            img
            img-layer
            blur)
    
  (let* (
            (img-width (car (gimp-drawable-width img-layer)))
            (img-height (car (gimp-drawable-height img-layer)))
            (layer (car (gimp-layer-new img img-width img-height RGBA-IMAGE
                                            "Base Layer" 100 LAYER-MODE-NORMAL)))
            (layer-copy (car (gimp-layer-copy layer TRUE)))
        )
        
    (gimp-image-resize img img-width img-height 0 0)
    (if (equal? (car (gimp-drawable-has-alpha img-layer)) FALSE)
        (gimp-layer-add-alpha img-layer))
    (gimp-image-add-layer img layer -1)
    (gimp-image-lower-layer img layer)
    (gimp-drawable-fill layer FILL-WHITE)
    (set! layer (car (gimp-image-merge-down img img-layer EXPAND-AS-NECESSARY)))
    (set! layer-copy (car (gimp-layer-copy layer TRUE)))
    (gimp-image-add-layer img layer-copy -1)
    (gimp-layer-set-mode layer-copy LAYER-MODE-OVERLAY)
    (plug-in-gauss-iir2 1 img layer blur blur)
    (plug-in-gauss-iir2 1 img layer-copy (+ (/ blur 2) 1) (+ (/ blur 2) 1))
    (let* (
            (point-num 3)
            (control_pts (cons-array (* point-num 2) 'byte))
           )
        (aset control_pts 0 0.0)
        (aset control_pts 1 0.0)
        (aset control_pts 2 0.51) ; was 127
        (aset control_pts 3 1.0)
        (aset control_pts 4 1.0)
        (aset control_pts 5 0.0)
        (gimp-drawable-curves-spline layer HISTOGRAM-VALUE (* point-num 2) control_pts)
        (gimp-drawable-curves-spline layer-copy HISTOGRAM-VALUE (* point-num 2) control_pts)
    )
    (plug-in-gauss-iir2 1 img layer (+ (* blur 2) 1) (+ (* blur 2) 1))
    (let* ((point-num 4)
           (control_pts (cons-array (* point-num 2) 'byte)))
       (aset control_pts 0 0.0)
       (aset control_pts 1 0.0)
       (aset control_pts 2 0.247) ; was 63
       (aset control_pts 3 1.0)
       (aset control_pts 4 0.749) ; was 191
       (aset control_pts 5 0.0)
       (aset control_pts 6 1.0)
       (aset control_pts 7 1.0)
       (gimp-drawable-curves-spline layer HISTOGRAM-VALUE (* point-num 2) control_pts)
       (gimp-drawable-curves-spline layer-copy HISTOGRAM-VALUE (* point-num 2) control_pts))
    
    (list layer layer-copy)	; Return
  ) ; end of let*
) ; end of define



(define (script-fu-funky-color-alpha
            img
            layer
            blur
    )
    
    (gimp-image-undo-group-start img)
  (let* (
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (old-layer-name (car (gimp-layer-get-name layer)))
            (layer-list (apply-easy-glowing-effect img layer blur))
        ) ; end variable definition
    
    (gimp-layer-set-name (car layer-list) old-layer-name)
    (gimp-layer-set-name (cadr layer-list) "Change layer mode")
    (if (equal? (car (gimp-selection-is-empty img)) FALSE)
        (begin
            (gimp-selection-invert img)
            (gimp-edit-clear (cadr layer-list))
            (gimp-selection-invert img)
        )
    )
    (gimp-context-set-foreground old-fg)
    (gimp-context-set-background old-bg)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc)
  )
)

(script-fu-register
    "script-fu-funky-color-alpha"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Style/Funky Color..."
    "Create funky color image. An Easy glowing effect. \nBe sure to check out the MODE in the layers dialog after running the script. 
    Different modes can yield much different appearances... \n file: photoeffects_246_02_part8.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Dec"
    "RGB*"
    SF-IMAGE       "Image"          0
    SF-DRAWABLE    "Drawable"       0
    SF-ADJUSTMENT  "Blur Amount"    '(10 1 100 1 1 0 1)
)


; end of script