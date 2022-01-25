;*************************************************************************************** 
; Soft focus script  for GIMP 2.10.18
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; improved by karlhof26 - 03 2020
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/07/22
;     - Initial relase
; version 0.2 by karlhof26 2020/03/26
;     - updated for Gimp 2.10.18 
; --------------------------------------------------------------------
; 


;;
(define (script-fu-soft-focus-G2
            img         ;;
            drawable    ;;
            blur        ;;
    )
  (let* (
            
            (layer-copy (car (gimp-layer-copy drawable TRUE)))
            (layer-mask (car (gimp-layer-create-mask layer-copy ADD-MASK-WHITE)))
        )
        
        
    (gimp-image-undo-group-start img)
    (gimp-image-insert-layer img layer-copy 0 -1)
    
    (gimp-image-add-layer-mask img layer-copy layer-mask)
    
    (gimp-edit-copy layer-copy)
    
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
    
    (gimp-image-remove-layer-mask img layer-copy MASK-APPLY)
    ;;
    (plug-in-gauss-iir2 1 img layer-copy blur blur)
    ;;
    (gimp-layer-set-opacity layer-copy 80)
    ;;
    (gimp-layer-set-mode layer-copy LAYER-MODE-SCREEN)
    
    ;;
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
    "script-fu-soft-focus-G2"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Style/Soft Focus..."
    "Soft focus effect \n file:ev_iccii_photoeffects_246_02_part7.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Jul"
    "RGB* GRAYA"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-ADJUSTMENT   "Blur Amount"   '(10 1 100 1 10 0 0)
)

;*************************************************************************************** 
; Solarization script  for GIMP 1.2
; Copyright (C) 2001 Iccii <iccii@hotmail.com>
; 
; --------------------------------------------------------------------
; version 0.1  by Iccii 2001/12/08
;     - Initial relase
; version 0.1a by Iccii 2001/12/09
;     - Added Threshold adjuster
;
; --------------------------------------------------------------------
; 



    ;; Solarization Effect

(define (script-fu-solarization-g2
            img
            drawable
            threshold
            target-channel
            invert?
            value-change?
    )
    
  (define (apply-solarization channel) 
    (let* (
            (point-num 256)
            (control_pts (cons-array point-num 'byte))
            (start-value (if (< threshold 128) (- 255 (* threshold 2)) 0))
            (end-value   (if (< threshold 128) 0 (* (- threshold 128) 2)))
            (grad (if (< threshold 128)
                     (/ (- 127 start-value) 127)
                     (/ (- end-value 127)   127)))
            (count 0)
          )
          (gimp-message "apply solarization inside")
      (while (< count point-num)
        (let* (
                (value1 (if (< threshold 128)
                           (if (< count 128)
                               (+ start-value (* grad count))
                               (- 255 count))
                           (if (< count 128)
                               count
                               (+ 127 (* grad (- count 128))))
                       )
                )
                (value2 (if (equal? value-change? TRUE) (+ value1 127) value1))
                (value  (if (equal? invert? TRUE) (- 255 value2) value2)))
          (aset control_pts count value)
          (set! count (+ count 1))
        )
      )
      (gimp-curves-explicit drawable channel point-num control_pts)
    )
  )
    
    
  (let* (
         (image-type (car (gimp-image-base-type img)))
         (has-alpha? (car (gimp-drawable-has-alpha drawable)))
        ) ; end variable definition
        
    (gimp-image-undo-group-start img)
    
    (if (or (= target-channel 0) (equal? image-type GRAY))
        (begin
            (apply-solarization HISTOGRAM-VALUE)
        )
        (begin
            (cond ((= target-channel 1)
                    (apply-solarization HISTOGRAM-RED))
                  ((= target-channel 2)
                    (apply-solarization HISTOGRAM-GREEN))
                  ((= target-channel 3)
                    (apply-solarization HISTOGRAM-BLUE))
                  ((= target-channel 4)
                    (if (equal? has-alpha? TRUE)
                        (apply-solarization HISTOGRAM-ALPHA)
                        (gimp-message "Drawable doesn't have an alpha channel! Abort.")
                    )
                  )
            )
        )
    )
     
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc) ; garbage cleanup; array was used
  )
)

(script-fu-register
    "script-fu-solarization-g2"
    "<Toolbox>/Script-Fu/Decor/Photo Effects/Style/Solarization..."
    "Apply solarization effect, which simulates Photoshop's Solarization filter [Part7] \n file:ev_iccii_photoeffects_246_02_part7.scm"
    "Iccii <iccii@hotmail.com>"
    "Iccii"
    "2001, Dec"
    "RGB* GRAY*"
    SF-IMAGE        "Image"         0
    SF-DRAWABLE     "Drawable"      0
    SF-ADJUSTMENT   "Threshold"     '(127 0 255 1 1 0 0)
    SF-OPTION       "Target Channel"    '("RGB (Value)" "Red" "Green" "Blue" "Alpha")
    SF-TOGGLE       "Invert"        FALSE
    SF-TOGGLE       "Value Change"  FALSE
)

; end of script