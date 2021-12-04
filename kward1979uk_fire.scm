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
     )
        ;(gimp-message "line53")
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(256 256 256))
        (set! image (car (gimp-image-new 256 256 RGB)))
        (gimp-image-undo-group-start image)
        ;(gimp-message "line60")
        
        (set! blank (car (gimp-text-fontname image -1 0 0 text 25 TRUE font-size PIXELS font)))
        (gimp-image-resize-to-layers image)
        
        ; karlhof26 - none of the below calls to scripts are working
        ; (FU-glow-selection 1 image blank 150 0 '(220 0 0) 12 FALSE)
        ; (script-fu-glowing-logo-alpha image blank 150 '(0 0 0))
        ;(gimp-message "line67")
        ;(set! textlayer (car (script-fu-aura1-logo 1 text font-size font '(220 3 0) '(100 80 90)))) 
        
        ;(set! text (car (gimp-image-merge-down image blank 0)))
        
        ;(gimp-floating-sel-anchor blank)
        (set! textlayer blank)
        
        (set! spare (car (gimp-layer-new image 256 256
                RGBA-IMAGE "spare" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image spare 0 0)
        
        ;(gimp-message "line79")
        ;(gimp-display-new image)
        
        (gimp-drawable-fill spare 3)
        (gimp-image-raise-layer-to-top image textlayer)
        ;(set! delete (car (gimp-image-merge-down image spare 0)))
        ;(gimp-image-remove-layer image delete)
        
        (set! yellow (duplicate-layer image textlayer))
        (gimp-layer-set-preserve-trans yellow 1)
        (gimp-selection-all image)
        ;(gimp-message "line90")
        
        (gimp-context-set-foreground '(251 247 128))
        (gimp-edit-bucket-fill yellow 0 0 100 0 0 0 0)
        (gimp-layer-set-preserve-trans yellow 0)
        (plug-in-shift 1 image yellow 50 0)
        (plug-in-waves 1 image yellow 12 0 50 0 0)
        (plug-in-gauss 1 image yellow 5 5 1)
        (plug-in-mblur 1 image yellow 0 5 5 0 0)
        ;(gimp-message "line99")
        
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
        ;(gimp-message "line112")
        
        
        
        
        ;(set! fire-text (car (gimp-layer-copy textlayer TRUE)))
        ;(set! fire-text (car (gimp-image-merge-visible-layers image 0))) 
        (set! fire-text (car (gimp-image-merge-down image textlayer EXPAND-AS-NECESSARY)))
        (set! fire-text (car (gimp-image-merge-down image fire-text EXPAND-AS-NECESSARY)))
        
        (set! space-layer (car (gimp-layer-new image final-width final-height
            RGBA-IMAGE "background" 100 LAYER-MODE-NORMAL)))
        (gimp-image-insert-layer image space-layer 0 0)
        (gimp-item-set-name space-layer "space-layer")
        
        (gimp-drawable-fill space-layer FILL-TRANSPARENT) ; was 3
        (gimp-image-resize-to-layers image)
        
        (gimp-progress-pulse)
        ;(gimp-message "line130")
        ;(gimp-display-new image)
       
        
        (plug-in-autocrop-layer 1 image fire-text)
        (set! fireHeight (car (gimp-drawable-height fire-text)))
        (set! fireWidth (car (gimp-drawable-width fire-text)))
        (set! theHeight (car (gimp-image-height image)))
        (set! theWidth (car (gimp-image-width image)))
        (set! moveHeight (- theHeight fireHeight))
        (set! moveWidth (/ (- theWidth fireWidth) 2))
        (gimp-layer-translate fire-text moveWidth moveHeight )
        (gimp-image-undo-group-end image)
        
        (gimp-progress-pulse)
        ;(gimp-message "line144")
        ;(gimp-display-new image)
        
        
        (if (= back TRUE)
            ;;;; (image lavabg theHeight theWidth))  ; to expand?
            (begin
                (gimp-selection-none image)
                (gimp-context-set-gradient "German flag smooth")
                (gimp-image-undo-group-start image)
                (set! bg-layer (car (gimp-layer-new image theWidth theHeight
                    RGBA-IMAGE "background" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image bg-layer 0 0)
                (gimp-drawable-fill bg-layer 3)
                (gimp-image-raise-layer-to-top image fire-text)
                (gimp-context-set-foreground '(255 148 0))
                (gimp-context-set-background '(0 0 0))
                (gimp-edit-blend bg-layer 1 1 1 100 0 0 FALSE FALSE 3 0.20 TRUE 0 0 0 theHeight)
                (script-fu-lava image bg-layer 10 10 7 "German flag smooth" TRUE TRUE TRUE FALSE)
                
                (set! spare2 (car (gimp-layer-new image theWidth theHeight
                    RGBA-IMAGE "spare2" 100 LAYER-MODE-NORMAL)))
                (gimp-image-insert-layer image spare2 0 0)
                (gimp-drawable-fill spare2 3)
                
                (gimp-progress-pulse)
                ;(gimp-message "line170")
                ;(gimp-displays-flush)
                
                (set! merge1 (car (gimp-image-merge-down image spare2 0)))
                ;(gimp-layer-set-mode merge1 COLOR)
                (gimp-layer-set-mode merge1 LAYER-MODE-HARDLIGHT)    ; changed
                ;;; (gimp-image-raise-layer-to-top image fire-text) ; error on layer ID
                (gimp-image-undo-group-end image)
            ) ; end begin
        ) ; end if 
        (set! final (car (gimp-image-merge-visible-layers image 0)))
        (gimp-color-balance final 1 1 80 10 -40)
        (gimp-brightness-contrast final -20 40)
        ;(gimp-message "line183")
        (gimp-displays-flush)
        (gimp-display-new image)
    )
)

(script-fu-register "fire"
            "<Image>/Script-Fu/Logos/Fire Logo..."
            "Set your text on fire with the option of a lava background. \nfile:kward1979uk_fire.scm"
            "Karl Ward"
            "Karl Ward"
            "October 2005"
            ""
            SF-STRING   "Text"      "FieryLogo"
            SF-FONT     "Font"      "Courier New Bold"
            SF-ADJUSTMENT   "Font-size"     '(100 50 300 1 10 0 1)
            SF-TOGGLE       "Render Lava"   TRUE
            SF-ADJUSTMENT   "Final Height: If text is greater text height will be used" '(256 100 600 1 10 0 1)
            SF-ADJUSTMENT   "Final width: If text is greater text width will be used" '(256 100 600 1 10 0 1)
            
            
            
)

;end of script