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
; Updated for Gimp-2.10.32
;
; Depends on the following scripts
; layerfx-outer-glow
; plug-in-shift
; plug-in-waves
;
; ------------------------------------------------------------------------------------------------------------------

(define (duplicate-layer image layer )
    (let* ((dup-layer (car (gimp-layer-copy layer 1))))
              (gimp-image-insert-layer image dup-layer 0 0)
        dup-layer
    )
)
; -------------------------------------------------------
; -------------------------------------------------------
(define (fire text font font-size back final-height final-width)
  (let* (
            (image 0)
            (blank 0)
            (spare 0)
            (delete 0)
            (yellow 0)
            (red 0)
            (fire-text 0)
            (space-layer 0)
            (fireHeight 0)
            (fireWidth 0)
            (theHeight 0)
            (theWidth 0)
            (moveHeight 0)
            (moveWidth 0)
            (bg-layer 0)
            (spare2 0)
            (merge1 0)
            (final 0)
            (textlayer 0)
            
            (baselayer )
            (width)
            (height)
     )
        ;(gimp-message "line65")
        (gimp-progress-update (/ 10 250))
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (set! image (car (gimp-image-new final-width final-height RGB))) ; was 256 256 RGB
        (gimp-image-undo-group-start image)
        ;(gimp-message "line71")
        (gimp-progress-update (/ 25 250))
        (set! blank (car (gimp-text-fontname image -1 0 0 text 25 TRUE font-size PIXELS font)))
        (gimp-image-resize-to-layers image)
        
        ; karlhof26 - none of the below calls to scripts are working
        ; (FU-glow-selection 1 image blank 150 0 '(220 0 0) 12 FALSE)  
        (set! width (car (gimp-drawable-width blank)))
        (set! height (car (gimp-drawable-width blank)))
        (set! baselayer (car (gimp-layer-new image final-width final-height RGBA-IMAGE "baselayer" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image baselayer 0 1)
        ;(gimp-message "line82")
        ;(set! textlayer (duplicate-layer image blank))
        ;(gimp-message "line84")
        ;(gimp-image-insert-layer image textlayer 0 0)
        ;(gimp-display-new image)
        ;(gimp-message "line87")
        (gimp-progress-update (/ 87 250))
        (script-fu-layerfx-outer-glow image blank '(220 0 0)
                    99   ; opacity
                    0    ; contour style
                    0.5  ; noise
                    0    ; normal mode
                    15.0 40.0 FALSE FALSE)
        ; (script-fu-glowing-logo-alpha image blank 150 '(0 0 0))
        
        (gimp-image-resize-to-layers image)
        ;(gimp-message "line98")
        (gimp-progress-update (/ 98 250))
        
        
        (set! textlayer blank)
        
        
        ;(set! spare (car (gimp-layer-new image 256 256
        ;        RGBA-IMAGE "spare" 100 LAYER-MODE-NORMAL)))
        ;(gimp-image-insert-layer image spare 0 0)
        
        ;(gimp-message "line109")
        (gimp-progress-update (/ 109 250))
        ;(gimp-display-new image)
        ;(quit)
        
        ;(gimp-drawable-fill spare 3)
        
        (gimp-image-raise-layer-to-top image textlayer)
        ;(set! delete (car (gimp-image-merge-down image spare 0)))
        ;(gimp-image-remove-layer image delete)
        
        (set! yellow (duplicate-layer image textlayer))
        (gimp-layer-set-preserve-trans yellow 1)
        (gimp-selection-all image)
        ;(gimp-message "line123")
        (gimp-progress-update (/ 123 250))
        
        (gimp-context-set-foreground '(251 247 128))
        (gimp-edit-bucket-fill yellow 0 0 100 0 0 0 0)
        (gimp-layer-set-preserve-trans yellow 0)
        (plug-in-shift 1 image yellow 50 0)
        (plug-in-waves 1 image yellow 12 0 50 0 0)
        (plug-in-gauss 1 image yellow 5 5 1)
        (plug-in-mblur 1 image yellow 0 5 5 0 0)
        ;(gimp-message "line133")
        (gimp-progress-update (/ 133 250))
        
        
        (set! red (duplicate-layer image yellow))
        (gimp-layer-set-preserve-trans red 1)
        (gimp-selection-all image)
        (gimp-context-set-foreground '(247 79 56))
        (gimp-edit-bucket-fill red 0 0 100 0 0 0 0)
        (gimp-item-set-name red "red")
        (gimp-item-set-name yellow "yellow")
        (gimp-layer-set-mode red LAYER-MODE-OVERLAY)
        
        (gimp-image-raise-layer-to-top image textlayer)
        ;(gimp-display-new image)
        ;(gimp-message "line148")
        (gimp-progress-update (/ 148 250))
        
        
        
        ;(set! fire-text (car (gimp-layer-copy textlayer TRUE)))
        ;(set! fire-text (car (gimp-image-merge-visible-layers image 0))) 
        (set! fire-text (car (gimp-image-merge-down image textlayer EXPAND-AS-NECESSARY)))
        (set! fire-text (car (gimp-image-merge-down image fire-text EXPAND-AS-NECESSARY)))
        (set! fire-text (car (gimp-image-merge-down image fire-text EXPAND-AS-NECESSARY)))
        
        (set! space-layer (car (gimp-layer-new image final-width final-height
            RGBA-IMAGE "background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image space-layer 0 1) ; was 0 0 
        (gimp-item-set-name space-layer "space-layer")
        
        (gimp-drawable-fill space-layer FILL-TRANSPARENT) ; was 3
        
        
        
        (gimp-image-resize-to-layers image)
        
        ;(gimp-progress-pulse)
        ;(gimp-message "line171")
        (gimp-progress-update (/ 171 250))
        ;(gimp-display-new image)
        
        
        
        (plug-in-autocrop-layer 1 image fire-text)
        
        
        
        
        
        
        ;(gimp-progress-pulse)
        ;(gimp-message "line184")
        (gimp-progress-update (/ 184 250))
        ;(gimp-display-new image)
        
        
        (if (= back TRUE)
            ;;;; (image lavabg theHeight theWidth))  ; to expand?
            (begin
                (set! fireHeight (car (gimp-drawable-height fire-text)))
                (set! fireWidth (car (gimp-drawable-width fire-text)))
                (set! theHeight (car (gimp-image-height image)))
                (set! theWidth (car (gimp-image-width image)))
                (set! moveHeight (- theHeight fireHeight))
                (set! moveWidth (/ (- theWidth fireWidth) 2))
                (gimp-layer-translate fire-text moveWidth moveHeight )
                
                
                
                (gimp-selection-none image)
                (gimp-context-set-gradient "German flag smooth")
                ;(gimp-image-undo-group-start image)
                (set! bg-layer (car (gimp-layer-new image theWidth theHeight
                    RGBA-IMAGE "background" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image bg-layer 0 0)
                (gimp-drawable-fill bg-layer 3)
                (gimp-image-raise-layer-to-top image fire-text)
                (gimp-context-set-foreground '(255 148 0))
                (gimp-context-set-background '(0 0 0))
                
                (set! spare2 (car (gimp-layer-new image theWidth theHeight
                    RGBA-IMAGE "spare2" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image spare2 0 1)
                (gimp-drawable-fill spare2 3)
                
                (gimp-edit-blend spare2 1 1 1 100 0 0 FALSE FALSE 3 0.20 TRUE 0 0 0 theHeight)
                 
                (script-fu-lava image bg-layer 10 10 7 "German flag smooth" TRUE TRUE TRUE FALSE)
                
                (gimp-layer-set-mode spare2 LAYER-MODE-OVERLAY)
                
                
                
                
                ;(gimp-progress-pulse)
                ;(gimp-message "line230")
                (gimp-progress-update (/ 230 250))
                ;(gimp-displays-flush)
                
                
                (set! merge1 (car (gimp-image-merge-down image spare2 0)))
                ;(gimp-displays-flush)
                ;(gimp-display-new image)
                ;(quit)
                ;(gimp-layer-set-mode merge1 COLOR)
                ;(gimp-layer-set-mode merge1 LAYER-MODE-HARDLIGHT)    ; changed
                ;;; (gimp-image-raise-layer-to-top image fire-text) ; error on layer ID
                ;(gimp-image-undo-group-end image)
            ) ; end begin
        ) ; end if 
        
        (set! final (car (gimp-image-merge-visible-layers image 0)))
        (gimp-color-balance final 1 1 80 10 -40)
        ;(gimp-brightness-contrast final -20 40)
        
        ;(gimp-message "line249")
        (gimp-progress-update (/ 250 250))
        (gimp-image-undo-group-end image)
        (gc) ; garbage cleanup; memory cleanup
        (gimp-displays-flush)
        (gimp-display-new image)
    )
)

(script-fu-register "fire"
            "<Toolbox>/Script-Fu/Logos/Fire Logo..."
            "Set your text on fire with the option of a lava background. \nfile:kward1979uk_fire.scm"
            "Karl Ward"
            "Karl Ward"
            "October 2005"
            ""
            SF-STRING   "Text"      "FieryLogo"
            SF-FONT     "Font"      "Courier New Bold"
            SF-ADJUSTMENT   "Font-size"     '(100 50 300 1 10 0 1)
            SF-TOGGLE       "Render Lava"   TRUE
            SF-ADJUSTMENT   "Final Height: If text is greater text height will be used" '(256 100 2600 1 10 0 1)
            SF-ADJUSTMENT   "Final width: If text is greater text width will be used" '(256 100 2600 1 10 0 1)
            
            
            
)

;end of script