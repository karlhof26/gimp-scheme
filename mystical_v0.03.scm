;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	This program is free software: you can redistribute it and/or modify								;;
;	it under the terms of the GNU General Public License as published by								;;
;	the Free Software Foundation, either version 3 of the License, or									;;
;	(at your option) any later version.																	;;
;																										;;
;	This program is distributed in the hope that it will be useful,										;;
;	but WITHOUT ANY WARRANTY without even the implied warranty of										;;
;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the										;;
;	GNU General Public License for more details.														;;
;               																						;;
;   You should have received a copy of the GNU General Public License									;;
;   along with this program.  If not, see <http://www.gnu.org/licenses/>.								;;
;               																						;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  v0.03 Mystical; Gimp v2.8.16 and now also Gimp 2.10.18                                              ;;
;;  (de) http://www.3d-hobby-art.de/news/196-gimp-script-fu-mythical.html                               ;;
;;  (eng) http://www.3d-hobby-art.de/en/blog/197-gimp-script-fu-mythical.html                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (script-fu-mythical img drawable inBgColor inLightWidth inGradientName inLightDirection inRunMode)
    
    (define (get-base-layer new-layer-marker)
        (let* (
                (parent (car (gimp-item-get-parent new-layer-marker)))
                (siblings 
                    (if (= -1 parent)
                        (vector->list (cadr (gimp-image-get-layers img)))
                        (vector->list (cadr (gimp-item-get-children parent))) 
                    )
                )
            )
            (let 
                loop ((layers (cdr (memv new-layer-marker siblings))))
                (if (= (car (gimp-item-get-visible (car layers))) TRUE)
                    (car layers)
                    (loop (cdr layers))
                )
            )
        )
    )
    ;;
    (define (gimp-message-and-quit message)
        (let  
            ;;
            ((old-handler (car (gimp-message-get-handler))) )
            (gimp-message-set-handler MESSAGE-BOX)
            (gimp-message message)
            ;;
            (gimp-message-set-handler old-handler)
            (quit)
        )
    )
    ; repeat func move Layer
    (define else #t)
    
    (define (my-duplicate-layer img layer)
        (let* ((dup-layer (car (gimp-layer-copy layer 1)))) (gimp-image-add-layer img dup-layer 1) dup-layer)
    )
    
    (define (repeat func times)
        (cond ((> times 0)
            (cons (func)
                (repeat func (- times 1))))
                (else '())
        )
    )
    
    (let* ( 
            (bg-layer (car (gimp-image-get-layer-by-name img "background")))
            (brush-mask-layer (car (gimp-image-get-layer-by-name img "brush-mask")))
            (ImageWidth  (car (gimp-image-width  img)))
            (ImageHeight (car (gimp-image-height img)))
            (old-bg (car (gimp-context-get-background)))
            (old-fg (car (gimp-context-get-foreground)))
            (new-layer-marker (car (gimp-layer-new img 100 100 RGBA-IMAGE "marker (tmp)" 100 LAYER-MODE-NORMAL)))
            (new-bg-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "background-fill" 100 LAYER-MODE-NORMAL)))
            (object-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "faded off object" 100 LAYER-MODE-NORMAL)))
            (object-group (car (gimp-layer-group-new img)))
            (Ls)
            (ls-layer-mask)
            (gradient-layer)
            (light-group)
            (light-group-layer)
            (streaks-layer)
            (streaks-layer-mask)
            (streaks-layer-copy)
            (streaks-layer-copy2)
            (move-layer)
            (lmap-layer)
            (main-light-layer)
            (main-light-layer-mask)
            (floating-selection)
            (object-fade-off-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "object fade off" 100 LAYER-MODE-NORMAL)))
            (object-fade-off-layer-mask (car (gimp-layer-create-mask object-fade-off-layer ADD-MASK-WHITE)))
            (mo-layer) ;;
            (main-object-layer-mask) ;;
            (light-zoom-layer)
            (contrast-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "contrast" 100 LAYER-MODE-NORMAL)))
            (clouds-layer (car (gimp-layer-new img (* ImageWidth 1.3) (* ImageHeight 1.3) RGBA-IMAGE "clouds (texture)" 25 LAYER-MODE-SOFTLIGHT)))
            (grain-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "Grain" 20 LAYER-MODE-OVERLAY)))
            (grain-shadow-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "Grain - shadows" 70 LAYER-MODE-DARKEN-ONLY)))
            (noise-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "noise" 100 LAYER-MODE-SCREEN)))
            (color-layer (car (gimp-layer-new img ImageWidth ImageHeight RGBA-IMAGE "color" 50 LAYER-MODE-SOFTLIGHT)))
            (seed)
            
            (object-layer-high-pass)
            (object-layer-high-pass-dupl)
            (main-object-group)
            
            (gradient-actual-name "Abstract 2")
        )
        
        
        ;;
        (if  ( = (car (gimp-image-get-layer-by-name img "background")) -1)
            (gimp-message-and-quit "There is no \"background\" layer! Tutorial - please read. \n Keine \"background\" -Ebene gefunden! Bitte lesen Sie mein Tutorial.")
        )
        (if  ( = (car (gimp-image-get-layer-by-name img "brush-mask")) -1)
            (gimp-message-and-quit "There is no \"brush-mask\" layer! Tutorial - please read. \n Keine \"brush-mask\" -Ebene gefunden! Bitte lesen Sie mein Tutorial.")
        )
        
        (gimp-image-undo-group-start img)
        
        (gimp-context-push)
        
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        
        ;;
        (gimp-image-set-active-layer img bg-layer)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img new-layer-marker 0 0)
        (gimp-image-set-active-layer img new-layer-marker)
        (gimp-context-set-background '(245 0 0))
        (gimp-edit-fill new-layer-marker FILL-BACKGROUND)
        ;;
        (gimp-message "line 136")
        
        (gimp-image-set-active-layer img new-layer-marker)
        (let* ( 
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
              )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            ;;
            (gimp-layer-set-offsets new-layer-marker (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
            ;;
            (gimp-item-set-visible new-layer-marker FALSE)
        )
        
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img new-bg-layer 0 2)
        (gimp-image-set-active-layer img new-bg-layer)
        (gimp-context-set-background inBgColor)
        (gimp-edit-fill new-bg-layer FILL-BACKGROUND)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-selection-layer-alpha brush-mask-layer)
        (gimp-item-set-visible brush-mask-layer FALSE)
        (gimp-message "line 168")
        (gimp-displays-flush)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-item-set-lock-content bg-layer TRUE)
        (gimp-edit-copy bg-layer)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img object-layer 0 2)
        (set! object-layer (car (gimp-edit-paste object-layer FALSE)))
        (gimp-floating-sel-to-layer object-layer)
        (gimp-image-merge-down img object-layer CLIP-TO-BOTTOM-LAYER)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-item-set-name object-group "Object Group")
        (gimp-layer-set-mode object-group LAYER-MODE-NORMAL)
        (gimp-layer-set-opacity object-group 100)
        (gimp-image-insert-layer img object-group 0 2)
        (gimp-displays-flush)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-reorder-item img (car (gimp-image-get-layer-by-name img "faded off object")) object-group 0)
        
        ;;
        ;; ************************************************************************************************************************************
        (set! Ls (car (gimp-layer-new-from-visible img img "Ls (tmp)")))
        (gimp-image-insert-layer img Ls 0 2)
        (gimp-message "line 199")
        
        (if (= inLightDirection 0)
            (begin
                ;;
                ;; **********************************************************************************************************
                (gimp-message (number->string inLightWidth)) 
                ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img Ls "Flare Rays - 3d-hobby-art.de" GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL (/ ImageWidth 2) (/ ImageWidth 2) -40 inLightWidth FALSE)
                (script-fu-layerfx-gradient-overlay img Ls BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Radial 103" GRADIENT-LINEAR REPEAT-NONE
                    FALSE 100 LAYER-MODE-NORMAL-LEGACY
                    (/ ImageWidth 2)
                    (/ ImageWidth 2)
                    (- (/ ImageWidth 2) (* (/ ImageWidth 2) (/ inLightWidth 8000)))
                    (+ (/ ImageHeight 2) (* (/ ImageHeight 2) (/ inLightWidth 8000)))
                    FALSE)
                (gimp-message "line 202")
            )
            (begin
                (gimp-message "line 209")
                (if (= inLightDirection 1)
                    (begin
                        (gimp-message (number->string inLightWidth)) 
                        ;;
                        ;; *********************************************************************************************************
                        ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img Ls "Flare Rays - 3d-hobby-art.de" GRADIENT-LINEAR REPEAT-NONE FALSE 100 NORMAL-MODE (/ ImageWidth 2) (/ ImageWidth 2) 40 inLightWidth FALSE)
                        (script-fu-layerfx-gradient-overlay img Ls BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Radial 102" GRADIENT-LINEAR REPEAT-NONE
                            FALSE 100 LAYER-MODE-NORMAL-LEGACY 
                            (/ ImageWidth 2)
                            (/ ImageHeight 2)
                            (+ (/ ImageWidth 2) (* (/ ImageWidth 2) (/ inLightWidth 8000)))
                            (* ImageHeight 0.75)
                            FALSE)
                        (gimp-message "line 216")
                    )
                )
            )
        )
        
        ;removed by karlhof26 
        ;(gimp-image-remove-layer img Ls)
        
        ;;
        ;new by karlhof26
        (gimp-message "line 227")
        (set! light-group (car (gimp-layer-group-new img)))
        (gimp-item-set-name light-group "light-group")
        (gimp-image-insert-layer img light-group 0 -1)
        
        
        (set! light-group-layer (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "Ls (tmp)-gradient")) TRUE))) ; was Ls (tmp)-with-gradient
        (gimp-image-insert-layer img light-group-layer light-group -1)
        (gimp-message "line 235")
        
        (gimp-displays-flush)
        ;(quit)
        
        ;;;????(gimp-image-remove-layer img Ls)
        (gimp-item-set-visible Ls FALSE)
        
        (set! gradient-layer (car (gimp-image-get-layer-by-name img "Ls (tmp)-gradient")))
        (gimp-layer-set-name light-group-layer "Lighting Setup")
        (gimp-layer-set-name gradient-layer "light source (tmp)")
        
        (gimp-message "line 247")
        ;;
        ;; ************************************************************************************************************************************
        (set! streaks-layer (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "faded off object")) 0)))
        (gimp-image-insert-layer img streaks-layer light-group 0)
        (gimp-layer-set-name streaks-layer "light streaks")
        (gimp-image-set-active-layer img streaks-layer)
        (gimp-message "line 256")
        
        (gimp-displays-flush)
        ;(quit)
        
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
                (set! base-x (car  (gimp-drawable-offsets base-layer)))
                (set! base-y (cadr (gimp-drawable-offsets base-layer)))
                (set! base-width  (car (gimp-drawable-width  base-layer)))
                (set! base-height (car (gimp-drawable-height base-layer)))
                (if (= inLightDirection 0)
                    (begin
                        (gimp-message "line 274")
                        ;;
                        ;; *********************************************************************************************************
                        ; was angle = -50
                        (plug-in-mblur RUN-NONINTERACTIVE img streaks-layer 0 1330 310 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                    )
                    (begin
                        (gimp-message "line 281")
                        (if (= inLightDirection 1)
                            ;;
                            ;; *********************************************************************************************************
                            (plug-in-mblur RUN-NONINTERACTIVE img streaks-layer 0 1330 230 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                        )
                    )
                )
        )
        (gimp-message "line 290")
        (gimp-displays-flush)
        ;
        ; ************************************************************************************************************************************
        (set! move-layer (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "faded off object")) 0)))
        (gimp-image-insert-layer img move-layer 0 2)
        (gimp-layer-set-name move-layer "move (tmp)")
        (gimp-image-set-active-layer img move-layer)
        (if (= inLightDirection 0)
            ;;
            (gimp-layer-translate move-layer -10 10)
            (if (= inLightDirection 1)
                ;;
                (gimp-layer-translate move-layer 10 10)
            )
        )
        (gimp-message "line 305")
        
        (let (
                (pos-x 0)
                (pos-y 0)
                (step 10)
                (layers (repeat (lambda () (my-duplicate-layer img (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "move (tmp)")) 0))))
                    (- 111 1)))
            )
            (for-each (lambda (x)
                (if (= inLightDirection 0)
                    ;;
                    (begin
                        (set! pos-x (- pos-x step))
                    )
                    (begin
                        (if (= inLightDirection 1)
                            ;;
                            (set! pos-x (+ pos-x step))
                        )
                    )
                )
                (set! pos-y (+ pos-y step))
                (gimp-layer-translate x pos-x pos-y)
                )
                layers
            )
        )
        
        (gimp-message "line 334")
        
        
        ;;
        (gimp-item-set-visible bg-layer FALSE)
        (gimp-item-set-visible new-bg-layer FALSE)
        (gimp-item-set-visible light-group FALSE)
        (gimp-item-set-visible object-group FALSE)
        (gimp-item-set-visible gradient-layer FALSE)
        ;;
        
        
        (gimp-image-merge-visible-layers img CLIP-TO-BOTTOM-LAYER)
        
        ;;
        (gimp-item-set-visible light-group TRUE)
        (gimp-item-set-visible object-group TRUE)
        (gimp-item-set-visible new-bg-layer TRUE)
        (gimp-message "line 352")
        
        
        ;;
        ;; ***********************************************************************************************************************************
        ; changed by karlhof26 - added a layer copy 
        (gimp-item-set-name (car (gimp-image-get-layer-by-name img "move (tmp)")) "Light map (tmp)")
        (set! lmap-layer (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "Light map (tmp)")) TRUE)))
        
        (set! lmap-layer (gimp-image-select-item img CHANNEL-OP-ADD (car (gimp-image-get-layer-by-name img "Light map (tmp)"))))
        ;(set! lmap-layer (gimp-selection-layer-alpha (car (gimp-image-get-layer-by-name img "Light map (tmp)"))))
        (gimp-selection-feather img 40)
        (gimp-item-set-visible (car (gimp-image-get-layer-by-name img "Light map (tmp)")) FALSE)
        (gimp-message "line371")
        (gimp-displays-flush)
        ;(quit)
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-set-active-layer img streaks-layer)
        (set! streaks-layer-mask (car (gimp-layer-create-mask streaks-layer ADD-MASK-SELECTION)))
        (gimp-image-add-layer-mask img streaks-layer streaks-layer-mask)
        (gimp-layer-remove-mask streaks-layer MASK-APPLY)
        (gimp-selection-none img)
        (gimp-message "line 375")
        
        (if (= inLightDirection 0)
            ;;
            (gimp-layer-translate streaks-layer -177 270)
                (if (= inLightDirection 1)
                    ;;
                    (gimp-layer-translate streaks-layer 177 270)
                )
        )
        
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            (gimp-message "line 397")
            (if (= inLightDirection 0)
                (begin;;
                    ;; *********************************************************************************************************
                    (plug-in-mblur RUN-NONINTERACTIVE img streaks-layer 0 235 310 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                )
                (begin
                    (if (= inLightDirection 1)
                            (begin ;;
                                ;; *********************************************************************************************************
                                (plug-in-mblur RUN-NONINTERACTIVE img streaks-layer 0 235 50 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                            )
                    )
                )
                
            )
        )
        (gimp-message "line 414")
        ;;
        (set! streaks-layer-copy (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "light streaks")) 0)))
        (gimp-image-add-layer img streaks-layer-copy 3)
        (gimp-layer-set-name streaks-layer-copy "light streaks -copy (tmp)")
        (gimp-image-set-active-layer img streaks-layer-copy)
        
        (set! streaks-layer-copy2 (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "light streaks -copy (tmp)")) 0)))
        (gimp-image-insert-layer img streaks-layer-copy2 0 3)
        (gimp-layer-set-name streaks-layer-copy2 "light streaks -copy (tmp) #2")
        (gimp-image-set-active-layer img streaks-layer-copy2)
        (gimp-message "line 425")
        
        (gimp-image-merge-down img streaks-layer-copy2 CLIP-TO-BOTTOM-LAYER)
        
        ;;
        (gimp-selection-layer-alpha (car (gimp-image-get-layer-by-name img "light streaks -copy (tmp)")))
        (gimp-item-set-visible (car (gimp-image-get-layer-by-name img "light streaks -copy (tmp)")) FALSE)
        
        ;;
        (gimp-context-set-background '(0 0 0))
        (set! ls-layer-mask (car (gimp-layer-create-mask (car (gimp-image-get-layer-by-name img "light source (tmp)")) ADD-MASK-WHITE)))
        (gimp-image-add-layer-mask img (car (gimp-image-get-layer-by-name img "light source (tmp)")) ls-layer-mask)
        (gimp-edit-fill ls-layer-mask FILL-BACKGROUND)
        (gimp-edit-fill ls-layer-mask FILL-BACKGROUND)
        
        (gimp-message "line 440")
        
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            (if (= inLightDirection 0)
                (begin
                    ;;
                    ;; *********************************************************************************************************
                    (plug-in-mblur RUN-NONINTERACTIVE img ls-layer-mask 0 350 310 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                )
                (begin
                    (if (= inLightDirection 1)
                        ;;
                        ;; *********************************************************************************************************
                        (plug-in-mblur RUN-NONINTERACTIVE img ls-layer-mask 0 350 50 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
                    )
                )
            )
        )
        (gimp-message "line 468")
        
        (plug-in-gauss (if (= inRunMode TRUE) (begin RUN-INTERACTIVE) RUN-NONINTERACTIVE) img ls-layer-mask 10 10 1)
        ;;
        (gimp-selection-all img)
        (gimp-edit-copy ls-layer-mask)
        (gimp-selection-none img)
        
        ;;
        (gimp-image-set-active-layer img streaks-layer)
        (gimp-drawable-desaturate streaks-layer DESATURATE-LIGHTNESS)
        
        ;;
        (gimp-item-set-visible streaks-layer FALSE)
        (gimp-item-set-visible object-group FALSE)
        
        (gimp-message "line 484")
        ;;
        ;; ************************************************************************************************************************************
        (set! main-light-layer (car (gimp-layer-new-from-visible img img "main light")))
        (set! main-light-layer-mask (car (gimp-layer-create-mask main-light-layer ADD-MASK-WHITE)))
        (gimp-image-insert-layer img main-light-layer light-group 1)
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            ;;
            (gimp-message "line 502")
            ;; *********************************************************************************************************
            (plug-in-mblur RUN-NONINTERACTIVE img main-light-layer 1 30 0 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
        )
        (gimp-image-add-layer-mask img main-light-layer main-light-layer-mask)
        ;;
        (set! floating-selection (car (gimp-edit-paste main-light-layer-mask 0)))
        (gimp-floating-sel-anchor floating-selection)
        
        ;;
        (gimp-item-set-visible streaks-layer TRUE)
        (gimp-item-set-visible object-group TRUE)
        (gimp-message "line 514")
        ;;
        (gimp-edit-copy (car (gimp-image-get-layer-by-name img "faded off object")))
        
        
        (gimp-image-insert-layer img object-fade-off-layer 0 4)
        (set! floating-selection (car (gimp-edit-paste object-fade-off-layer 0)))
        (gimp-floating-sel-anchor floating-selection)
        (gimp-image-add-layer-mask img object-fade-off-layer object-fade-off-layer-mask)
        ;;
        (gimp-edit-copy (car (gimp-layer-get-mask (car (gimp-image-get-layer-by-name img "light source (tmp)")))))
        ;;
        (set! floating-selection (car (gimp-edit-paste object-fade-off-layer-mask 0)))
        (gimp-floating-sel-anchor floating-selection)
        (gimp-drawable-invert object-fade-off-layer-mask TRUE)
        (gimp-message "line 529")
        ;;
        ;; *********************************************************************************************************
        ;(python-layerfx-color-overlay (if (= inRunMode TRUE) (begin RUN-INTERACTIVE) RUN-NONINTERACTIVE) img (car (gimp-image-get-layer-by-name img "object fade off")) '(0 0 0) 100 NORMAL-MODE FALSE)
        (script-fu-layerfx-color-overlay img (car (gimp-image-get-layer-by-name img "object fade off")) '(0 0 0) 100 LAYER-MODE-NORMAL-LEGACY FALSE)
        (gimp-message "line 534")
        ;; 
        (gimp-image-reorder-item img (car (gimp-image-get-layer-by-name img "object fade off")) object-group 0)
        (gimp-image-reorder-item img (car (gimp-image-get-layer-by-name img "object fade off-color")) object-group 0)
        (gimp-layer-set-name (car (gimp-image-get-layer-by-name img "object fade off-color")) "object fade off -overlay")
        ;;
        
        (gimp-message "line 541")
        (gimp-displays-flush)
        ;(quit)
        ; removed by karlhof26
        ;(gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "object fade off-with-color")))
        
        ;;
        (gimp-layer-set-opacity (car (gimp-image-get-layer-by-name img "faded off object")) 24)
        (gimp-layer-set-opacity (car (gimp-image-get-layer-by-name img "light streaks")) 30)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-message "line 553")
        (set! mo-layer (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "faded off object")) 0)))
        
        (set! main-object-group (car (gimp-layer-group-new img)))
        (gimp-item-set-name main-object-group "Main object group")
        (gimp-image-insert-layer img main-object-group 0 4)
        
        (gimp-image-insert-layer img mo-layer main-object-group -1) ; was img mo-layer 0 4
        (gimp-layer-set-name mo-layer "main object (tmp)")
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (gimp-message "line 564")
        (gimp-displays-flush)
        ;;
        ;; *********************************************************************************************************
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
                
                (calcx 0)
                (calcy 0)
            )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            (gimp-message "line 579")
            (gimp-message (number->string (- (+ base-x (/ base-width  2)) (/ 100  2))))
            (set! calcx (- (+ base-x (/ base-width  2)) (/ 100  2)))
            (set! calcy (- (+ base-y (/ base-height  2)) (/ 100  2)))
            
            (set! gradient-actual-name "Flare Radial 102")
            (cond
                ((= inGradientName 1)
                            (set! gradient-actual-name "Incandescent") ;"Incandescent" "Horizon 1" "Nauseating Headache"
                )
                ((= inGradientName 2)
                            (set! gradient-actual-name "Horizon 1")
                )
                ((= inGradientName 3)
                           (set! gradient-actual-name "Nauseating Headache")
                )
                (else
                    (gimp-message "crashed to the else")
                           (set! gradient-actual-name "Flare Radial 101")
                )
            )
            (if (= inLightDirection 0)
                (begin
                    (gimp-message "line 610")
                    
                    ;;
                    ;; **********************************************************************************************************
                    ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img (car (gimp-image-get-layer-by-name img "main object (tmp)")) (if (= inGradientName 0) "FG to BG (RGB)" (if (= inGradientName 1) "VG nach HG (RGB)")) GRADIENT-LINEAR REPEAT-NONE TRUE 100 NORMAL-MODE (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) -120 700 FALSE)
                    ;(script-fu-layerfx-gradient-overlay img (car (gimp-image-get-layer-by-name img "main object (tmp)")) BLEND-FG-BG-RGB LAYER-MODE-NORMAL-LEGACY "FG to BG (RGB)" GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) -120 inLightWidth FALSE)
                    (script-fu-layerfx-gradient-overlay img mo-layer BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY 
                        (if (= inGradientName 0) "FG to BG (RGB)" (if (>= inGradientName 1) gradient-actual-name))
                        GRADIENT-LINEAR
                        REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY
                        (* (/ ImageWidth 2) 1)
                        (* (/ ImageHeight 2) 1)
                        ;-120 300
                        (* (/ ImageWidth 2) 0.1)
                        (* ImageHeight 0.9)
                        
                        FALSE)
                    
                    ;test;;;(script-fu-layerfx-gradient-overlay img Ls BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Rays-3dhobbyartde" GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY (/ ImageWidth 2) (/ ImageWidth 2) -40 inLightWidth FALSE)
                    (gimp-message "line 619")
                    (gimp-displays-flush)
                )
                (begin 
                    (if (= inLightDirection 1)
                        (begin
                            (gimp-message "line626")
                            ;;
                            ;; *********************************************************************************************************
                            ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img (car (gimp-image-get-layer-by-name img "main object (tmp)")) (if (= inGradientName 0) "FG to BG (RGB)" (if (= inGradientName 1) "VG nach HG (RGB)")) GRADIENT-LINEAR REPEAT-NONE FALSE 100 NORMAL-MODE (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) 120 700 FALSE)
                            (script-fu-layerfx-gradient-overlay img (car (gimp-image-get-layer-by-name img "main object (tmp)"))
                                BLEND-FG-BG-RGB LAYER-MODE-NORMAL-LEGACY
                                (if (= inGradientName 0) "FG to BG (RGB)" (if (>= inGradientName 1) gradient-actual-name))
                                GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY
                                (- (+ base-x (/ base-width  2)) (/ 100  2))
                                (- (+ base-y (/ base-height 2)) (/ 100 2))
                                (* ImageWidth 0.95) ; 120
                                (* ImageHeight 0.95) ; 700
                                FALSE)
                            (gimp-displays-flush)
                        )
                    )
                )
            )
            (gimp-message "line 637")
        )
        
        (gimp-message "line 640")
        ;;
        (gimp-item-set-visible (car (gimp-image-get-layer-by-name img "main object (tmp)")) FALSE)
        (gimp-item-set-visible streaks-layer FALSE)
        (gimp-item-set-visible (car (gimp-image-get-layer-by-name img "light source (tmp)")) FALSE)
        ;;
        (gimp-edit-copy-visible img)
        (gimp-displays-flush)
        
        (gimp-item-set-visible (car (gimp-image-get-layer-by-name img "main object (tmp)")) TRUE)
        (set! main-object-layer-mask (car (gimp-layer-create-mask mo-layer ADD-MASK-WHITE)))
        ;;
        (gimp-image-add-layer-mask img (car (gimp-image-get-layer-by-name img "main object (tmp)")) main-object-layer-mask)
        (set! floating-selection (car (gimp-edit-paste main-object-layer-mask 0)))
        (gimp-floating-sel-anchor floating-selection)
        
        (gimp-message "line 632")
        (gimp-displays-flush)
        ;(quit)
        
        ;removedkh (gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "main object (tmp)-gradient")))
        ;;
        (gimp-layer-set-name (car (gimp-image-get-layer-by-name img "main object (tmp)-gradient")) "main object -group")
        (gimp-layer-set-name (car (gimp-image-get-layer-by-name img "main object (tmp)")) "main object")
        ;;
        (gimp-layer-set-mode (car (gimp-image-get-layer-by-name img "main object -group")) LAYER-MODE-SOFTLIGHT-LEGACY)
        (gimp-layer-set-opacity (car (gimp-image-get-layer-by-name img "main object")) 100)
        
        (gimp-message "line 644")
        (gimp-displays-flush)
        ;;
        ;; ************************************************************************************************************************************
        (set! light-zoom-layer (car (gimp-layer-new-from-visible img img "Light source zoom")))
        (gimp-image-insert-layer img light-zoom-layer 0 4)
        (gimp-displays-flush)
        
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
            (gimp-message "line 659")
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            ;;
            ;;
            (gimp-message "line 666")
            ;;*********************************************************************************************************
            (plug-in-mblur RUN-NONINTERACTIVE img light-zoom-layer 2 90 0 (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)))
            (gimp-displays-flush)
        )
        (gimp-layer-set-mode light-zoom-layer LAYER-MODE-SCREEN)
        (gimp-layer-set-opacity light-zoom-layer 24)
        (gimp-drawable-desaturate light-zoom-layer DESATURATE-LIGHTNESS)
        (gimp-message "line 674")
        (gimp-displays-flush)
        
        ;;
        (gimp-context-set-interpolation INTERPOLATION-CUBIC)
        (gimp-layer-scale light-zoom-layer (* ImageWidth 1.5) (* ImageHeight 1.5) TRUE)
        
        (gimp-message "line 681")
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img contrast-layer 0 4)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-context-set-background '(255 255 255))
        (gimp-edit-fill contrast-layer FILL-BACKGROUND)
        (gimp-displays-flush)
        
        (let* (
                (base-x 0)
                (base-y 0)
                (base-layer (get-base-layer new-layer-marker))
                (base-width (car (gimp-image-width  img)))
                (base-height (car (gimp-image-height img)))
            )
            (set! base-x (car  (gimp-drawable-offsets base-layer)))
            (set! base-y (cadr (gimp-drawable-offsets base-layer)))
            (set! base-width  (car (gimp-drawable-width  base-layer)))
            (set! base-height (car (gimp-drawable-height base-layer)))
            
            (if (= inLightDirection 0)
                (begin
                    (gimp-message "line 704")
                    ;;
                    ;; *********************************************************************************************************
                    ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img contrast-layer (if (= inGradientName 0) "FG to BG (RGB)" (if (= inGradientName 1) "VG nach HG (RGB)")) GRADIENT-LINEAR REPEAT-NONE FALSE 100 NORMAL-MODE (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) 0 (- ImageWidth (/ ImageWidth 3)) TRUE)
                    
                    ;(script-fu-layerfx-gradient-overlay img (car (gimp-image-get-layer-by-name img "main object (tmp)"))BLEND-FG-BG_RGB LAYER-MODE-NORMAL-LEGACY "FG to BG (RGB)" GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) -120 inLightWidth FALSE)
                    (script-fu-layerfx-gradient-overlay img contrast-layer BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Glow Radial 2" GRADIENT-LINEAR
                        REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY 
                        (/ ImageWidth 2)
                        (/ ImageWidth 2)
                        (+ (/ ImageWidth 2) (* (/ ImageWidth 2) 0.5)) ;0  
                        (* ImageHeight 0.2)
                        FALSE)
                    
                    ;test;;;(script-fu-layerfx-gradient-overlay img Ls BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Rays-3dhobbyartde" GRADIENT-LINEAR REPEAT-NONE FALSE 100 LAYER-MODE-NORMAL-LEGACY (/ ImageWidth 2) (/ ImageWidth 2) -40 inLightWidth FALSE)
                    (gimp-message "line 713")
                    (gimp-displays-flush)
                )
                (begin
                    (gimp-message "line 717")
                    (if (= inLightDirection 1)
                        (begin
                            ;;
                            ;; *********************************************************************************************************
                            ; Script-fu-layerfx-gradient-overlay MUst be called with Legacy layer-modes-only!
                            ;(python-layerfx-gradient-overlay RUN-NONINTERACTIVE img contrast-layer (car (gimp-image-get-layer-by-name img "main object")) (if (= inGradientName 0) "FG to BG (RGB)" (if (= inGradientName 1) "VG nach HG (RGB)")) GRADIENT-LINEAR REPEAT-NONE TRUE 100 NORMAL-MODE (- (+ base-x (/ base-width  2)) (/ 100  2)) (- (+ base-y (/ base-height 2)) (/ 100 2)) 0 (- ImageWidth (/ ImageWidth 3)) TRUE)
                            (script-fu-layerfx-gradient-overlay img contrast-layer BLEND-CUSTOM LAYER-MODE-NORMAL-LEGACY "Flare Rays Radial 1" GRADIENT-LINEAR
                                REPEAT-SAWTOOTH FALSE 100 LAYER-MODE-NORMAL-LEGACY 
                                (/ ImageWidth 7)
                                (/ ImageWidth 7)
                                (* ImageWidth 0.8)
                                (* ImageWidth 0.8)
                                FALSE)
                            (gimp-displays-flush)
                        )
                    )
                )
            )
        )
        (gimp-message "line 730")
        
        (gimp-layer-set-mode (car (gimp-image-get-layer-by-name img "contrast-gradient")) LAYER-MODE-OVERLAY) ; was contrat
        (gimp-layer-set-opacity (car (gimp-image-get-layer-by-name img "contrast-gradient")) 90) ; was contrast
        (gimp-displays-flush)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img clouds-layer 0 5)
        (set! seed (if (number? seed) seed (realtime)))
        (if (< seed 1)
            (set! seed (srand 9500))
        )
        ; plasma has a bug - result is displaced 
        ;(plug-in-plasma RUN-NONINTERACTIVE img clouds-layer seed 3)
        (plug-in-solid-noise 1 img clouds-layer FALSE TRUE seed 3 3.5 2.26)
        (gimp-drawable-desaturate clouds-layer DESATURATE-LIGHTNESS)
        (gimp-displays-flush)
        (gimp-message "line 748")
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-context-set-foreground '(128 128 128))
        (gimp-drawable-fill grain-layer FILL-FOREGROUND)
        (gimp-drawable-fill grain-shadow-layer FILL-FOREGROUND)
        (gimp-image-insert-layer img grain-layer 0 5)
        (gimp-image-insert-layer img grain-shadow-layer 0 5)
        (plug-in-solid-noise 1 img grain-layer 1 1 3141598 8 4 4)
        (plug-in-hsv-noise 1 img grain-layer 2 0 0 150)
        (gimp-message "line 759")
        (gimp-displays-flush)
        (gimp-message "quit 762")
        ;(quit)
        
        
        (gimp-context-set-antialias TRUE)
        (gimp-context-set-feather TRUE)
        (gimp-context-set-feather-radius 4 3)
        (gimp-context-set-sample-merged TRUE)
        (gimp-image-select-color img CHANNEL-OP-REPLACE (car (gimp-image-get-layer-by-name img "main light")) '(35 35 35))
        (gimp-displays-flush)
        (plug-in-hsv-noise 1 img grain-shadow-layer 2 5 5 100) ; was 2 1 0 100
        (gimp-selection-none img)
        (plug-in-colortoalpha RUN-NONINTERACTIVE img grain-shadow-layer '(128 128 128))
        (plug-in-gauss 1 img grain-shadow-layer 3 3 1)
        (gimp-message "line 842")
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img noise-layer 0 5)
        (gimp-context-set-background inBgColor)
        (gimp-edit-fill noise-layer FILL-BACKGROUND)
        (plug-in-solid-noise 1 img noise-layer 0 1 (rand 314159) 3 2 2)
        (plug-in-solid-noise 1 img grain-layer 1 0 314159 3 2 2)
        ;(plug-in-rgb-noise 1 img noise-layer FALSE FALSE 0.03 0.03 0.03 0.03)
        (plug-in-rgb-noise 1 img noise-layer TRUE FALSE 0.3 0.3 0.3 0.01)
        (gimp-layer-set-mode noise-layer LAYER-MODE-GRAIN-MERGE)
        (gimp-message "line 854")
        (gimp-displays-flush)
        
        ;(quit)
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-insert-layer img color-layer 0 4)
        (gimp-context-set-background '(216 124 124))
        (gimp-edit-fill color-layer FILL-BACKGROUND)
        ;(python-layerfx-color-overlay (if (= inRunMode TRUE) (begin RUN-INTERACTIVE) RUN-NONINTERACTIVE) img color-layer '(255 230 230) 100 LAYER-MODE-MULTIPLY-LEGACY FALSE)
        (script-fu-layerfx-color-overlay img color-layer '(255 230 230) 100 LAYER-MODE-MULTIPLY-LEGACY FALSE)
        (gimp-message "line 866")
        (gimp-displays-flush)
        
        
        (gimp-layer-set-name (car (gimp-image-get-layer-by-name img "color-color")) "Red Color (option)")
        (gimp-layer-set-mode (car (gimp-image-get-layer-by-name img "Red Color (option)")) LAYER-MODE-SOFTLIGHT)
        
        (gimp-displays-flush)
        ;(quit)
        
        ;; 
        ;; ************************************************************************************************************************************
        (set! object-layer-high-pass (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "faded off object")) 0)))
        (gimp-image-insert-layer img object-layer-high-pass (car (gimp-image-get-layer-by-name img "Main object group")) 0) ; was main object -group
        (gimp-layer-set-opacity object-layer-high-pass 100)
        (gimp-message "line 881")
        (gimp-displays-flush)
        (gimp-layer-set-name object-layer-high-pass "object (highPass)")
        (set! object-layer-high-pass-dupl (car (gimp-layer-copy (car (gimp-image-get-layer-by-name img "object (highPass)")) 0)))
        (gimp-image-insert-layer img object-layer-high-pass-dupl (car (gimp-image-get-layer-by-name img "Main object group")) 0) ; was main object -group
        (gimp-drawable-invert object-layer-high-pass-dupl FALSE)
        (gimp-message "line 887")
        
        (plug-in-gauss (if (= inRunMode TRUE) (begin RUN-INTERACTIVE) RUN-NONINTERACTIVE) img object-layer-high-pass-dupl (/ ImageWidth 80) (/ ImageWidth 80) 1)
        (gimp-layer-set-opacity object-layer-high-pass-dupl 50)
        (gimp-displays-flush)
        
        (gimp-image-merge-down img object-layer-high-pass-dupl CLIP-TO-BOTTOM-LAYER)
        (gimp-brightness-contrast (car (gimp-image-get-layer-by-name img "object (highPass)")) 0 90)
        (gimp-drawable-desaturate (car (gimp-image-get-layer-by-name img "object (highPass)")) DESATURATE-AVERAGE)
        (gimp-layer-set-mode (car (gimp-image-get-layer-by-name img "object (highPass)")) LAYER-MODE-OVERLAY)
        (gimp-message "line 897")
        
        ;;
        ;; ************************************************************************************************************************************
        (gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "light streaks -copy (tmp)")))
        (gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "Light map (tmp)")))
        (gimp-message "line 829")
        (gimp-displays-flush)
        (gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "marker (tmp)")))
        (gimp-image-remove-layer img (car (gimp-image-get-layer-by-name img "light source (tmp)")))
        (gimp-layer-set-visible (car (gimp-image-get-layer-by-name img "light streaks")) TRUE)
        (plug-in-gauss 1 img main-light-layer-mask 20 20 1)
        (plug-in-gauss 1 img object-fade-off-layer-mask 30 30 1)
        (gimp-layer-set-opacity (car (gimp-image-get-layer-by-name img "faded off object")) 90)
        (gimp-message "line 911")
         
        ;;
        (gimp-selection-none img)
        
        ;;
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
        (gimp-image-set-active-layer img (car (gimp-image-get-layer-by-name img "Red Color (option)")))
        (gimp-message "Good finish OK")
        (gimp-context-pop)
        (gimp-image-undo-group-end img)
        
        (gimp-displays-flush)
        (gc) ; garbage collect
    )
)


(script-fu-register
    "script-fu-mythical"                                                ;func name
    "Mystical ..."                                                      ;menu label
    "Create awesome atmospheric lighting and Mystical effect. Eg light through a doorway type effect. \nRequires layers named background and brush-mask (Brush mask must contain transparent areas) \nfile:mystical_v0_03.scm"          ;desc
    "Stephan W."
    "Stephan Wittling; (c) 2016, 3d-hobby-art.de"                       ;copyright notice
    "Mai 30, 2016"                                                      ;date created
    "RGBA , RGB"                                                        ;image type that the script works on
    SF-IMAGE        "Image"                     0
    SF-DRAWABLE     "The layer"                 0
    SF-COLOR        "Add background"           '(0 0 0)
    SF-ADJUSTMENT   "InLightWidth"       '(2500 0 8000 10 100 0 0)
    SF-OPTION       "Gradient"                 '("FG to BG (RGB) (en)" "Incandescent" "Horizon 1" "Nauseating Headache")
    SF-OPTION       "Direction"                '("Top Right to Bottom Left" "Top Left to Bottom Right")
    SF-TOGGLE       "Run Interactive Mode?"     FALSE
)

(script-fu-menu-register "script-fu-mythical" "<Image>/Script-Fu/Effects")
;end of script 