; smart-sharpening.scm    version 1.00    Jan 07, 2004
;
; script-fu-smart-sharpening - Smart sharpening of image. This script finds
; the edges of images and only sharpens those.
;
; You can find more about smart sharpening at
; http://www.gimpguru.org/Tutorials/SmartSharpening/
; 
; This plugin is made for Gimp-2.10.30. 
;
; Changelog:
; 1.00 - initial release
;
; Copyright 2004 Olli Salonen <olli@cabbala.net>
;
(define (spline)
    (let* (
            (a (cons-array 8 'byte))
        )
    (set-pt a 0 0 0)
    (set-pt a 1 166 0)
    (set-pt a 2 246 255)
    (set-pt a 3 255 255)
    a)
)

(define (script-fu-smart-sharpening inImg inDrw inAmount inRadius inEdge)
    
    (let* (
            (original inImg)
            (template (car (gimp-image-duplicate original)))
            (original-layers (cadr (gimp-image-get-layers inImg)))
            (template-layers (cadr (gimp-image-get-layers template)))
            (template-bg-copy (car (gimp-layer-copy (aref template-layers 0) TRUE)))
            (width (car (gimp-image-width original)))
            (height (car (gimp-image-height original)))
            (sharpen-mask 0)
            (lab-image 0)
            (lab-layers 0)
            (final-mask 0)
            (result-image 0)
            (result-layers 0)
            
            (sharpened)
        )
        
        (gimp-image-undo-group-start inImg)
        
        (gimp-image-insert-layer template template-bg-copy 0 -1)
        (gimp-image-set-active-layer template template-bg-copy)
        (gimp-selection-all template)
        (gimp-edit-copy template-bg-copy)
        (set! sharpen-mask (car (gimp-channel-new template width height "SharpenMask" 50 '(255 0 0))))
     ;   (gimp-image-add-channel template sharpen-mask 0)
        (gimp-image-insert-channel template sharpen-mask 0 0)
        (gimp-floating-sel-anchor (car (gimp-edit-paste sharpen-mask FALSE)))
        (plug-in-edge TRUE template sharpen-mask inEdge 1 0)
        (gimp-drawable-invert sharpen-mask FALSE)    
        (gimp-curves-spline sharpen-mask HISTOGRAM-VALUE 8 (spline)) ; was 0
        (plug-in-gauss-iir TRUE template sharpen-mask 1 TRUE TRUE)
        (gimp-edit-copy sharpen-mask)
        
        ; split to L*a*b* and sharpen only L-channel
        (set! lab-image (car (plug-in-decompose TRUE original (aref original-layers 0) "LAB" TRUE)))
        (set! lab-layers (cadr (gimp-image-get-layers lab-image)))
        (set! final-mask (car (gimp-channel-new lab-image width height "FinalMask" 50 '(255 0 0))))
       ; (gimp-image-add-channel lab-image final-mask 0)
        (gimp-image-insert-channel lab-image final-mask 0 0)
        (gimp-floating-sel-anchor (car (gimp-edit-paste final-mask FALSE)))
      ;  (gimp-image-delete template)
        (gimp-selection-load final-mask)
        (gimp-selection-invert lab-image)
        (gimp-selection-shrink lab-image 1)
       ; (gimp-image-remove-channel lab-image final-mask)
        ;(plug-in-unsharp-mask TRUE lab-image (aref lab-layers 2) inRadius inAmount 0)
        (plug-in-unsharp-mask TRUE lab-image (aref lab-layers 0) inRadius inAmount 3)
       ; (gimp-selection-none lab-image)
       
       ;(gimp-display-new lab-image)
       ;(gimp-message "LAB stop")
       
        
        ; compose image from Lab-channels
        
        (set! result-image (car (plug-in-drawable-compose TRUE 0 (aref lab-layers 0) (aref lab-layers 1) (aref lab-layers 2) 0 "LAB")))
        
        ;(gimp-drawable-invert (car (gimp-image-get-layer-by-name result-image "Background")) FALSE)
        ;(gimp-display-new result-image)
        ;(quit)
        
        (set! result-layers (cadr (gimp-image-get-layers result-image)))
        (gimp-edit-copy (aref result-layers 0))
      
      ;  (gimp-image-delete lab-image)
      ;  (gimp-image-delete result-image)
        
        (set! sharpened (car (gimp-layer-new original width height 1 "Result" 100 0)))
        (gimp-image-insert-layer original sharpened 0 -1)
        
        (gimp-floating-sel-anchor (car (gimp-edit-paste sharpened FALSE))) 
        ;(gimp-floating-sel-anchor (car (gimp-edit-paste (aref original-layers 0) FALSE))) ; was layers 0)
        
        ;(gimp-display-new result-image)
        (gimp-image-delete lab-image)
        (gimp-image-delete result-image)
      
        (gimp-image-undo-group-end inImg)
        (gimp-displays-flush)
        (gc) ; memory cleanup; garbage cleanup
    )
)

(script-fu-register "script-fu-smart-sharpening"
        "Smart Sharpening"
        "Sharpen images intelligently. Smart sharpen only sharpens images on the edges, where sharpening counts. Even areas are not sharpened, so noise levels are kept down when compared to normal unsharp mask. You may need to tweak the parameters for best result. \nfile:smart-sharpening_02.scm"
        "Olli Salonen <olli@cabbala.net>"
        "Olli Salonen"
        "Jan 07, 2004"
        "*"
        SF-IMAGE              "Image"                0
        SF-DRAWABLE           "Drawable"             0
        SF-ADJUSTMENT         "Amount of USM"        '(0.5 0 10 0.01 0.01 2 0)
        SF-ADJUSTMENT         "Radius of USM"        '(0.5 0 10 0.01 0.01 2 0)
        SF-ADJUSTMENT         "FindEdge amount"      '(2.0 0 10 0.01 0.01 2 0)
)

(script-fu-menu-register "script-fu-smart-sharpening" "<Image>/Script-Fu2/Enhance")


; end of file