;    
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Shiny painting script  for GIMP 2.4
; Copyright (C) 2001 Laetitia Marin titix@gimpforce.org
; Copyright (C) 2001 Ostertag Raymond coordinateur@gimp-fr.org
; Copyright (C) 2007 Philippe Demartin philippe@demartinenchile.com Paint corroded version for GIMP 2.4
;
; Tags: logo
; 
; Author statement:
;
; This is the official English version you'll find a french version at http://www.gimp-fr.org/
;
; Script-fu Shiny corroded Painting an attempt to realise the Scott-Effect with painted surface
;
; Start : a selection in an image or a layer with a transparency area who will be transformed 
; in selection
;
; See the manual at the tutorial section of the gug http://gug.sunsite.dk/
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
; version 0.2 2007-october-21
;     - Initial relase
;  Update for Gimp-2.10.24 - Sept 2021
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


(define (Aply-script-fu-shiny img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
  (let* (
            (sizeX (car (gimp-drawable-width Bump-Layer)))
            (sizeY (car (gimp-drawable-height Bump-Layer)))
            (Bunped_layer (car (gimp-layer-copy Bump-Layer FALSE)))
            (seed (* 30 30))
            (activ_selection (car (gimp-selection-is-empty img)))
            
            (calque1 (car (gimp-layer-new img sizeX sizeY RGBA-IMAGE "plasma" 100 LAYER-MODE-NORMAL-LEGACY)))
            (masque1 (car (gimp-layer-create-mask calque1 1)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (blanc '(255 255 255))
            (pick_color '(255 255 255))
            
        )
        
        (set! damage (* damage 2.55))
        ;(gimp-message "started shiny OK")
        ; undo initialisation
        (gimp-image-undo-group-start img)
        ;(gimp-message "line 68")
        (gimp-image-insert-layer img calque1 0 -1)
        (gimp-image-resize-to-layers img)
        ;(gimp-message "line 71")
        (gimp-layer-resize-to-image-size calque1)
        
                
        (gimp-selection-layer-alpha Bump-Layer)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-bucket-fill Bump-Layer BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
        (gimp-selection-invert img)
        (gimp-context-set-foreground fond-color)
        ;(gimp-message "line 79")
        (gimp-edit-bucket-fill Bump-Layer BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
        (gimp-image-insert-layer img Bunped_layer 0 0)
        (gimp-selection-invert img)
        ;(gimp-message "line 86")
        ; layer 1
        ;(gimp-image-insert-layer img calque1 0 -1)
        ;(gimp-selection-layer-alpha Bump-Layer)
        ;(gimp-selection-invert img)
        (gimp-context-set-foreground '(240 240 120))
        (gimp-edit-bucket-fill calque1 BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
        
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(gimp-message "line 96")
        ;; TEMP QUIT TO DEBUG
        ; plasma
        (set! seed (* 30 30))
        ;(plug-in-plasma 1 img calque1 12377 4.8) ; was 123 2.8 - plasma has a bug in 2.10.22
        ;(plug-in-solid-noise 1 img calque1 FALSE TRUE 1236 14.8 1.1 2.5)
        (plug-in-rgb-noise 1 img calque1 TRUE TRUE (/ damage 255) (/ damage 255) (/ damage 255) 0) ;0.76 0.9 0.7 0.82
        ;(gimp-message "noise done")
        ;(gimp-message (number->string damage))
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        
        
        ;(gimp-message "to threshold")
        (gimp-drawable-threshold calque1 HISTOGRAM-LUMINANCE (/ damage 256) 1.0) ; was damage / 255
        
        (gimp-displays-flush)
        ;(gimp-display-new img)
        
        ;(gimp-message "line 114")
        
         
        (gimp-image-add-layer-mask img calque1 masque1)
        
        (let* (
                (calque2 (car (gimp-layer-copy calque1 TRUE)))
                (masque2 (car (gimp-layer-get-mask calque2))) 
              )
            
            
            ; layer 2
            (gimp-image-insert-layer img calque2 0 0)
            ;(gimp-message "line 127")
            ; fill the layer-mask
            (gimp-context-set-foreground blanc)
            (set! activ_selection (car (gimp-selection-is-empty img)))
            (cond
                ((= activ_selection 0) ; selection activ
                    
                    (gimp-edit-bucket-fill masque1 BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
                    (gimp-edit-bucket-fill masque2 BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
                    (gimp-selection-none img)
                )
                ((= activ_selection 1) ; no selection activ
                    (gimp-selection-layer-alpha Bump-Layer)
                    (gimp-edit-bucket-fill masque1 BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
                    (gimp-edit-bucket-fill masque2 BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
                )
            ) ; end of cond
            ;(gimp-message "line 144")
            ;(gimp-display-new img)
            ;(gimp-displays-flush)
            
            
            (gimp-context-set-sample-threshold 0.25)
            (gimp-context-set-antialias FALSE)
            (gimp-context-set-sample-criterion SELECT-CRITERION-COMPOSITE)
            (gimp-image-select-color img CHANNEL-OP-REPLACE calque1 '(255 255 255)) 
            ;;(gimp-by-color-select calque1 '(255 255 255) 0 0 FALSE FALSE 0 FALSE)
            (gimp-context-set-background Bcolor)
            ;(gimp-message "line 156")
            (gimp-edit-bucket-fill calque2 BUCKET-FILL-BG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
            (gimp-selection-invert img)
            (gimp-edit-clear calque2)
            
            (gimp-selection-invert img)
            (gimp-selection-all img)
            
            ;(gimp-message "line 164")
            ;(gimp-display-new img)
            ;(gimp-displays-flush)
            
            
            ; Bumping the letters
            (plug-in-gauss 1 img Bump-Layer 10 10 0)
            (plug-in-bump-map TRUE img Bunped_layer Bump-Layer  125 30 bumpmap_depth 0 0 0 0 TRUE TRUE 1)
            ; bumpmap on layer 2
            (plug-in-bump-map TRUE img calque2 Bump-Layer     125 30 bumpmap_depth 0 0 0 0 TRUE TRUE 1)
            (plug-in-bump-map TRUE img calque2 calque2     125 45 bumpmap_depth 0 0 0 0 TRUE FALSE 1)
            ;(gimp-message "line 175")
            ;Light efect
            (plug-in-lighting 1 img calque2 calque2 0 TRUE FALSE 0 0 blanc LightPX LightPY    
               LightA -1.19 -7.14 1.00 0.9 2 2 LightA 10 TRUE FALSE FALSE)
            
            ;(gimp-message "line 180")
            ;(gimp-displays-flush)
            ;(gimp-display-new img)
            
            
            (gimp-layer-set-mode calque1 8)
            
            
            (gimp-layer-set-offsets Bump-Layer 18 12)
            
            (plug-in-gauss 1 img Bump-Layer 20 20 0)
            
            (gimp-layer-resize-to-image-size Bump-Layer)
            (gimp-selection-layer-alpha Bump-Layer)
            (gimp-selection-invert img)
            (gimp-context-set-foreground fond-color)
            (gimp-edit-bucket-fill Bump-Layer BUCKET-FILL-FG LAYER-MODE-NORMAL-LEGACY 100 0 FALSE 0 0)
            (gimp-selection-all img)
            ; back to the initials colours and display the result
            ;(gimp-message "line 199")
            
            (gimp-context-set-foreground old-fg)
            (gimp-context-set-background old-bg)
            
            (if (= Crackeled TRUE)
                ; layer 3
                (let* (
                        (calque3 (car (gimp-layer-copy calque2 TRUE)))
                      )
                    ;(gimp-message "line 209")
                    (gimp-image-insert-layer img calque3 0 0)
                    (gimp-selection-layer-alpha calque2)
                    (gimp-context-set-background Bcolor)
                    ;(plug-in-mosaic 1 img calque2 10 10 1 0 TRUE 175.2 0.3 1 0 3 0 1)
                    
                    (plug-in-mosaic 1 img calque2 15 20 1 0 TRUE 175.2 0.3 1 0 0 0 0)
                    (gimp-layer-set-mode calque3 21)
                    (gimp-item-set-name calque2 "mosaic")
                    (gimp-item-set-name calque3 "mosaic-alpha")
                )
            )
            (gimp-selection-all img)
            ;(gimp-message "line 222")
            
            (gimp-context-set-foreground old-fg)
            (gimp-context-set-background old-bg)
            ;Finish the undo group for the process
            (gimp-image-undo-group-end img)
            
            (gimp-displays-flush)
            (gimp-message "Good finish OK")
        )
  )
)

(define (script-fu-corroded-shiny-logo-alpha img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
    (begin
        (Aply-script-fu-shiny img Bump-Layer fond-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-corroded-shiny-logo-alpha"
    "<Image>/Script-Fu/Alpha to Logo/Corroded Painting"
    "Scott-effect : Shiny Corroded Painting \nfile:xtns-demartin-shiny-painting_02.scm"
    "titix raymond and philippe"
    "2001, titix and raymond 2007 Philippe Demartin"
    "20.10.2007"
    "*"
    SF-IMAGE "Image" 0
    SF-DRAWABLE "Drawable" 0
    SF-COLOR "Background" '(178 178 178)
    SF-ADJUSTMENT "Painting Damage %" '(70 10 100 1 0 0 0)
    SF-ADJUSTMENT "Light Amount" '(0.70 0 10 0.1 1 2 0)
    SF-ADJUSTMENT "Light Position X" '(0 -50 50 1 0 0 0)
    SF-ADJUSTMENT "Light Position y" '(0 -50 50 1 0 0 0)
    SF-COLOR "Painting Color" '(255 0 0)
    SF-TOGGLE "Crackeled" FALSE
    SF-ADJUSTMENT "Bumpmap depth" '(15 1 50 1 0 0 0)
)

(define (script-fu-corroded-shiny-logo font text Text-Color Back-color size damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
    
    (let* (
            (img (car (gimp-image-new 256 256  RGB)))    ; nouvelle image -> img
            (border (/ size 4))
            ; (background (car (gimp-layer-new img 256 256 RGBA-IMAGE "background" 90 0)))
            (Text-Color (car  (gimp-context-set-foreground Text-Color)))
            ; (Back-color (car  (gimp-context-set-background Back-color)))
            (text-layer (car (gimp-text-fontname img -1 0 0 text border TRUE size PIXELS font)))
            
        )
        
        (gimp-layer-new img 256 256 RGBA-IMAGE "background" 90 0)
        
        ;(gimp-edit-bucket-fill-full background 1 0 100 255 FALSE FALSE 0 0 0 )
        ;(gimp-image-undo-disable img)
        ;(gimp-message "logo started OK")
        ;(gimp-message (number->string size))
        ;(gimp-message (number->string damage))
        (gimp-drawable-set-name text-layer text)
        (Aply-script-fu-shiny img text-layer Back-color damage LightA LightPX LightPY Bcolor Crackeled bumpmap_depth)
        ;(gimp-image-undo-enable img)
        (gimp-display-new img)
    )
)

(script-fu-register "script-fu-corroded-shiny-logo"
    "Corroded Painting"
    "Create corroded painted logo. \nfile:xtns-demartin-shiny-painting_02.scm"
    "Philippe Demartin"
    "Inspired from the Corrosion script from titix and raymond"
    "10/21/2007"
    ""
    SF-FONT "Font Name" "Tahoma Bold"
    SF-STRING "Enter your text" "Corroded..."
    SF-COLOR "Font Color" '(133 52 2)
    SF-COLOR "Background" '(178 178 178)
    SF-ADJUSTMENT "Font size (pixels)" '(150 2 1000 1 10 0 1)
    SF-ADJUSTMENT "Painting Damage %" '(70 10 100 1 0 0 0)
    SF-ADJUSTMENT "Light Amount" '(0.70 0 10 0.01 1 2 0)
    SF-ADJUSTMENT "Light Position X" '(0 -2 2 0.1 1 1 0)
    SF-ADJUSTMENT "Light Position y" '(0 -2 2 0.1 1 1 0)
    SF-COLOR    "Painting Color" '(255 0 0)
    SF-TOGGLE   "Crackeled" FALSE
    SF-ADJUSTMENT "Bumpmap depth" '(15 1 50 1 0 0 0)
)

(script-fu-menu-register "script-fu-corroded-shiny-logo"
             "<Toolbox>/Script-Fu/Logos")

;end of script