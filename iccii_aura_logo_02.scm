;  AURA-LOGO
;  draw the specified text in glowy effect and adds an aura around it
;  This logo was inspired from an add I saw on TV
;
;  Any comments? e-mail me at: Samer-Yhya@yahoo.com
;
;; Version de abcdugimp.free.fr
;
; Created by kward1979uk
;
;----------------------------------------------------------------------------------------------------------
; License: GPLv3
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;----------------------------------------------------------------------------------------------------------
; Modification History:
; Updated for Gimp-2.10.20
; ------------------------------------------------------------------------------------------------------------------


(define (apply-aura1-logo-effect img
                logo-layer
                bg-color
                text-color)
    (let* (
            (width (car (gimp-drawable-width logo-layer)))
            (height (* 2 (car (gimp-drawable-height logo-layer))))
            (bg-layer (car (gimp-layer-new img width height RGBA-IMAGE "Background" 100 LAYER-MODE-NORMAL)))
            (shadow-layer (car (gimp-layer-new img width height RGBA-IMAGE "Shadow" 100 LAYER-MODE-MULTIPLY)))
            (aura-layer (car (gimp-layer-new img width height RGBA-IMAGE "Aura" 100 LAYER-MODE-NORMAL)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (gausht 0)
          )
        (gimp-image-resize img width height 0 (/ height 4))
        (gimp-image-insert-layer img shadow-layer 0 1)
        (gimp-image-insert-layer img aura-layer 0 2)
        (gimp-image-insert-layer img bg-layer 0 3)
        (gimp-context-set-foreground text-color)
        (gimp-layer-set-preserve-trans logo-layer TRUE)
        (gimp-edit-fill logo-layer FILL-FOREGROUND)
        (gimp-context-set-background bg-color)
        (gimp-edit-fill bg-layer FILL-BACKGROUND)
        (gimp-edit-clear shadow-layer)
        (gimp-edit-clear aura-layer)
        (gimp-selection-layer-alpha logo-layer)
        (gimp-context-set-background '(0 0 0))
        (gimp-selection-feather img 7.5)
        (gimp-edit-fill shadow-layer FILL-BACKGROUND)
        
        (gimp-selection-layer-alpha logo-layer)
        (gimp-edit-fill aura-layer FILL-FOREGROUND)
        (gimp-selection-none img)
        
        (if (> (/ height 2) 500)
            (set! gausht 450)
            (set! gausht (/ height 2))
        )
        (plug-in-gauss-rle2 1 img aura-layer 10 gausht)
        (gimp-edit-copy aura-layer)
        (gimp-edit-paste aura-layer 0)
        (gimp-edit-paste aura-layer 0)
        (gimp-edit-paste aura-layer 0)
        (gimp-message "line76")
        (gimp-floating-sel-anchor (car (gimp-edit-paste aura-layer 0)))
        (script-fu-erase-rows img aura-layer 1 1 0)
        
        (gimp-context-set-foreground '(255 255 255))    
        (gimp-context-set-background text-color)
        (gimp-edit-blend logo-layer BLEND-FG-BG-RGB LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 FALSE 0 0 width height)
        
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
    )
)
        
(define (script-fu-aura1-logo-alpha img
                    logo-layer
                    bg-color
                    text-color)
  (begin
    (gimp-image-undo-group-start img)
    (apply-aura1-logo-effect img logo-layer bg-color text-color)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-aura1-logo-alpha"
            "<Image>/Script-Fu/Alpha-to-Logo/Aura..."
            "Creates a glowy effect and adds an aura around it. \nfile:iccii_aura_logo_02.scm"
            "Hani Al-Ers & Samer Yhya"
            "Hani Al-Ers & Samer Yhya"
            "2001"
            "RGBA"
            SF-IMAGE      "Image" 0
            SF-DRAWABLE   "Drawable" 0
            SF-COLOR      _"Background Color" '(0 0 0)
            SF-COLOR      _"Text Color" '(0 255 165)
)
            
(define (script-fu-aura1-logo text
                    size
                    font
                    bg-color
                    text-color)
  
  (let* (
            (img (car (gimp-image-new 256 256 RGB)))
            (text-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE size PIXELS font)))
        )
    (gimp-image-undo-disable img)
    (gimp-layer-set-name text-layer text)
    (apply-aura1-logo-effect img text-layer bg-color text-color)
    (gimp-image-undo-enable img)
    (gimp-message "Good finish OK")
    (gimp-display-new img)
  )
)

(script-fu-register "script-fu-aura1-logo"
        "<Toolbox>/Script-Fu/Logos/Aura..."
        "Text with an aura and halo.\nCree votre texte avec un affet brillant et un halo autour. Version de abcdugimp.free.fr  \nfile:iccii_aura_logo_02.scm"
        "Hani Al-Ers & Samer Yhya"
        "Hani Al-Ers & Samer Yhya"
        "2001"
        ""
        SF-STRING     "Text"                "The GIMP"
        SF-ADJUSTMENT "Font Size (pixels)"  '(100 2 1000 1 10 0 1)
        SF-FONT       "Font"                "Courier New Bold"
        SF-COLOR      "Background Color"    '(0 0 0)
        SF-COLOR      "Text Color"          '(0 255 165)
)

;end of script