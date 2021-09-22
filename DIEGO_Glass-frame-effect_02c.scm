; This script takes origin from the Gimp-chat topic KDE4/Aero Blur Effect.
; The main and simple purpose of this script is:
; - let the user perform his/her selection(s) (if not, a default selection is applied by the script)
; - create on the remaining part of the image a glass effect, with different glass aspects
; - leave selection(s) untouched, so that they look like through "holes" in the glass
;
; Flow implemented to get the final result:
; 
; 1. check whether the user did a selection: if not make one (a rounded rectangle)
; 2. duplicate the input layer
; 3. apply the chosen effect on the non-selected part of the image
; 4. duplicate again the original layer
; 5. apply on the non-selected part of the image a colour derived from the average (RobA script)
; 6. apply a bevelling effect (if so chosen by the user)
; 7. flatten the image (if so chosen by the user) 
; ------------------------------------------------------------------
; current version: 1.1 no more dependency on Layer-fx python plug-in
; thanks to Graechan - October 5, 2014
; ------------------------------------------------------------------
; 
; This program is free software; you can redistribute it and/or modify
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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; edited to remove dependancy on python-layerfx-bevel-emboss 3-10-2014 by Graechan
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(define (script-fu-glass-frame-effect
            theOldImage
            theOldLayer
            GlassEffect
            doBevelEmboss
            doFlatten
   )
    
    ; Initialize an undo, so the process can be undone with a single undo
    (gimp-image-undo-group-start theOldImage)
    
    ; Variable init
    (let* (
        (imageWidth (car (gimp-image-width theOldImage)))
        (imageHeight (car (gimp-image-height theOldImage)))
        (theDuplicatedLayer 0)
        (theNewLayer 0)
        (theTopLayer 0)
        (theMergedLayer 0)
        (theConteLayer 0)
        (theConteOut 0)
        (OldLayerPosition 0)
        (selectionWidth 0)
        (selectionHeight 0)
        (cornerRadiusX 0)
        (cornerRadiusY 0)
        (no-sel 0)          ;selection Flag
        (sel-x1 0) 
        (sel-y1 0)
        (sel-x2 0) 
        (sel-y2 0)
        (selectionWidth 0)
        (selectionHeight 0)
        (avg-colour 0)
        (bevelDepth 0)
        (bevelSize 0)
     )
        
        (gimp-message-set-handler 2)
        
        ; check whether there is a selection, if not a selection is forced (the central part)  
        (gimp-message "check selection")
        (set! no-sel (car (gimp-selection-is-empty theOldImage)))
        (if (= no-sel TRUE)
            (begin
                (set! selectionWidth (/ (* imageWidth 8) 10))
                (set! selectionHeight (/ (* imageHeight 8) 10))
                (set! cornerRadiusX (/ imageWidth 20))
                (set! cornerRadiusY (/ imageHeight 20))
                (gimp-image-select-round-rectangle theOldImage CHANNEL-OP-REPLACE
                    (/ (- imageWidth selectionWidth) 2)
                    (/ (- imageHeight selectionHeight) 2)
                    selectionWidth selectionHeight
                    cornerRadiusX cornerRadiusY)
            )
            
            (begin ; there is a selection
                ; save dimensions and position of the selecion
                (set! sel-x1 (cadr (gimp-selection-bounds theOldImage)))
                (set! sel-y1 (caddr (gimp-selection-bounds theOldImage)))
                (set! sel-x2 (cadddr (gimp-selection-bounds theOldImage)))
                (set! sel-y2 (car (cddddr (gimp-selection-bounds theOldImage))))
                (set! selectionWidth (- sel-x2 sel-x1))
                (set! selectionHeight (- sel-y2 sel-y1))
            )
        )
               
        (gimp-selection-invert theOldImage)
        
        (set! theDuplicatedLayer (car (gimp-layer-new-from-drawable theOldLayer theOldImage )))
        
        (gimp-image-insert-layer theOldImage theDuplicatedLayer 0 0)
        (gimp-item-set-name theDuplicatedLayer "Frame Layer")
        (gimp-layer-add-alpha theDuplicatedLayer)
        
        (gimp-message "choosen effect: ")
        (gimp-message (number->string GlassEffect))
        (if (= GlassEffect 0) ; GlassTile+Blur
            (begin
                (plug-in-glasstile 1 theOldImage theDuplicatedLayer 50 50)
                (plug-in-gauss 1 theOldImage theDuplicatedLayer
                    (/ imageWidth 200) (/ imageHeight 200) 0 )
                
            )
        )
        (if (= GlassEffect 1) ; Cartoons+Luce
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                        "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-cartoon 3,100,20"
                    )
                )
            )
        )
        (if (= GlassEffect 2) ; Textured Glass (G'MIC)
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_textured_glass 40,40,1,1,0,2,0"
                    )
                )
            )
        )
        (if (= GlassEffect 3) ; Fractalize
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                        "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                        "-fractalize "
                    )
                )
            )
        )
        (if (= GlassEffect 4) ; Gradient Flare
            (begin
                (plug-in-gflare 1 theOldImage theDuplicatedLayer "Distant_Sun"
                    (+ (/ selectionWidth 2) sel-x1) (+ (/ selectionHeight 2) sel-y1) (/ imageWidth 2)
                    0 0 30 1000 0 3 0.20)
                (plug-in-nova 1 theOldImage theDuplicatedLayer 
                    (+ (/ selectionWidth 2) sel-x1) (+ (/ selectionHeight 2) sel-y1)
                    '(200 200 0)100 1024 360)
            )
        )
        (if (= GlassEffect 5) ; Polygonize
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-polygonize 300,10,10,10,10"
                    )
                )
            )
        )
        (if (= GlassEffect 6) ; Stained Glass
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_stained_glass 35.5,0.10,1,0,1.20,0,1.25,25" ; was 100,0.10,1,0,1.20,0,1.25,25
                    )
                )
            )
        )
        (if (= GlassEffect 7) ; Psychedelic Glass
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_psyglass 0,90,0.10,1,0,3,0.9,1,1,0,1,0,0,0,0,0"
                    )
                )
            )
        )
        (if (= GlassEffect 8) ; Etching
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_gcd_etch 125,153,171,185,0.1,50,80,50,10,15,12,20,0,1,0.3,1,0"
                    )
                )
            )
        )
        (if (= GlassEffect 9) ; Charcoal
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_charcoal 145,70,170,1,1,1,128,128,255,255,255,0,0,0,0"
                    )
                )
            )
        )
        (if (= GlassEffect 10) ; Colored Pencil
            (begin
                (plug-in-gmic-qt 1 theOldImage theDuplicatedLayer 1 0 ; 1=input only active layer
                    (string-append
                    "-v - " ; silent output. Remove it to display errors from the G'MIC interpreter on stderr.
                    "-fx_cpencil 0.30,10,24,0,0,0.80"
                    )
                )
            )
        )
        
        (gimp-selection-invert theOldImage)  ; user selection restored
        
        (set! theNewLayer (car (gimp-layer-new-from-drawable theOldLayer theOldImage )))
        
        (gimp-image-insert-layer theOldImage theNewLayer 0 0)
        (gimp-item-set-name theNewLayer "Glass Layer")
        (gimp-layer-add-alpha theNewLayer)
        
        (gimp-edit-cut theNewLayer)   ; keep only the outside part of the image
        (gimp-selection-invert theOldImage)  ; glass only on outside part
        
        ;;;; use of average colour -----------------------------------------------------
        
        (gfe_sample_avg_colour theOldImage theNewLayer 0 1)
        (set! avg-colour (car (gimp-context-get-foreground)))
        (gimp-edit-fill theNewLayer FILL-FOREGROUND)
        (gimp-hue-saturation theNewLayer 0 0 0 100)
        
        ;;;; end average colour -----------------------------------------------------
        
        (gimp-layer-set-mode theNewLayer LAYER-MODE-OVERLAY)
        (gimp-layer-set-opacity theNewLayer 90)
        
        (if (= doBevelEmboss TRUE)
            (begin 
                ;(gimp-message "you selected Bevel")
                (set! theTopLayer (car (gimp-layer-new-from-drawable theOldLayer theOldImage )))
                
                (set! bevelDepth (/ imageWidth 50))
                (if (> bevelDepth 64)	; added check to prevent plug-in error
                    (set! bevelDepth 64)
                )
                (set! bevelSize (/ imageWidth 100))
                        
                (gimp-image-insert-layer theOldImage theTopLayer 0 0)
                (gimp-item-set-name theTopLayer "Emboss Layer")
                (gimp-layer-add-alpha theTopLayer)
                (gimp-edit-cut theTopLayer)
                (gfe-layerfx-bevel-emboss theOldImage theTopLayer 
                                           1               ;Style 
                                 bevelDepth       ;Depth 
                                 1               ;Direction 
                                 bevelSize       ;Size 
                                 2               ;Soften 
                                 120               ;Angle 
                                 30               ;Altitude 
                                 0               ;Gloss Contour 
                                 '(223 255 239)   ;Highlight Color 
                                 11               ;Highlight Mode 
                                 100               ;Highlight Opacity 
                                 '(04 16 8)       ;Shadow Color 
                                 11               ;Shadow Mode 
                                 100               ;Shadow Opacity 
                                 0               ;Surface Contour 
                                 FALSE           ;Invert 
                                 FALSE)           ;Merge with layer
                (gimp-item-set-visible theTopLayer FALSE)   
                
            )
            (gimp-message "you did not select Bevel") 
        )
        (gimp-message (number->string theDuplicatedLayer))
        (gimp-message (number->string theNewLayer))   
        (gimp-message (number->string selectionWidth))
        (gimp-message (number->string selectionHeight))   
        
        (gimp-message "near end")
        (if (= doFlatten TRUE)
            (begin
                (gimp-image-flatten theOldImage)
            )
        ) ;endif
        (if (= no-sel TRUE)
        '   (begin
                (gimp-selection-none theOldImage)
            ) ;delete the created selection
            (begin
                (gimp-selection-invert theOldImage)) ;present the original selection
        ) ;endif
        
        (gimp-message "Done!")
        ;Ensure the updated image is displayed now
        (gimp-displays-flush)
        
        (gimp-image-undo-group-end theOldImage)
        
    ) ; end let* 
) ; end define

(script-fu-register "script-fu-glass-frame-effect"
            "<Image>/Script-Fu/Frames/Creates a glass frame around an image-v.1.1"
            "Creates a frame around an image with a simple or sophisticated glass effect-v.1.1 \nfile:DIEGO_Glass-frame-effect_02c.scm"
            "Diego Nassetti"
            "Diego Nassetti"
            "September 2014"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-OPTION       "Effect on Glass Layer"
                            '(
                              "GlassTile&Blur"
                              "Cartoons"
                              "Textured Glass"
                              "Fractalize (random)"
                              "Gradient Flare"
                              "Polygonize"
                              "Stained Glass"
                              "Psychedelic Glass"
                              "Etching"
                              "Charcoal"
                              "Coloured Pencil"
                             )
            SF-TOGGLE       "Bevel/Emboss Selections"   TRUE
            SF-TOGGLE       "Flatten Image"   FALSE
)
;-----------------------------------------------------------------------------------------Sample average colours from RobA

(define (gfe_sample_avg_colour img inLayer inMerged InMode)
  (let*
    (
        (img (car (gimp-image-duplicate img)))
        (inLayer (car (gimp-image-get-active-layer img)))
        (savedsel 0)
        (handler (car (gimp-message-get-handler)))
        (palette (car (gimp-context-get-palette)))
        (colour 0)
    )
    ;  it begins here
   
     (unless (= inLayer -1) 
      (gimp-message-set-handler MESSAGE-BOX)
        
      (if (equal? (car (gimp-selection-is-empty img)) TRUE) 
            (gimp-selection-all img)
      )
      (set! savedsel (car (gimp-selection-save img)))
        
      (if (equal? (car (gimp-image-base-type img)) INDEXED)
            (gimp-image-convert-rgb img)
      )
        
      (if (equal? inMerged TRUE)
        (set! inLayer (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))
       )
        
      (gimp-selection-layer-alpha inLayer)
      (gimp-channel-combine-masks (car (gimp-image-get-selection img)) savedsel CHANNEL-OP-INTERSECT 0 0)
        
      (set! colour 
        (if (equal? (car (gimp-image-base-type img)) RGB)
          (list (round (car (gimp-histogram inLayer HISTOGRAM-RED 0 255))) 
                (round (car (gimp-histogram inLayer HISTOGRAM-GREEN 0 255))) 
                (round (car (gimp-histogram inLayer HISTOGRAM-BLUE 0 255))))
          (let ((greyval (round (car (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)))))
            (list greyval greyval greyval))
        )
      )
            
      (cond 
        ((and (equal? InMode 0) (equal? (car (gimp-image-base-type img)) RGB))
          (gimp-message (string-append "Average Colour"
                                       " R:" (number->string (car colour))
                                       " G:" (number->string (cadr colour))
                                       " B:" (number->string (caddr colour)))))
                                        
        ((and (equal? InMode 0) (equal? (car (gimp-image-base-type img)) GRAY))
          (gimp-message (string-append "Average Value: " (number->string (caddr colour)))))
            
        ((equal? InMode 1) 
          (gimp-context-set-foreground colour))
            
        ((equal? InMode 2)
          (gimp-context-set-background colour))
        
        ((and (equal? InMode 3) (equal? (car (gimp-palette-is-editable palette)) TRUE))
          (gimp-palette-add-entry palette "Untitled" colour)))
   
      (gimp-message-set-handler handler)
    )
        
    ;done
   (gimp-image-delete img)
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-blending-mode mode)
  (let* ((modenumbers #(0 1 3 15 4 5 16 17 18 19 20 21 6 7 8 9 10 11 12 13 14)))
    (vector-ref modenumbers mode)
  )
)

(define (math-round input)
  (floor (+ input 0.5))
)

(define (math-ceil input)
  (if (= input (floor input))
    input
    (+ (floor input) 1)
  )
)

(define (get-layer-pos img layer)
  (let* ((layerdata (gimp-image-get-layers img))
    (numlayers (car layerdata))
    (layerarray (cadr layerdata))
    (i 0)
    (pos -1)
   )
    (while (< i numlayers)
      (if (= layer (vector-ref layerarray i))
   (begin
     (set! pos i)
     (set! i numlayers)
   )
   (set! i (+ i 1))
      )
    )
    pos
  )
)

(define (add-under-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (+ (get-layer-pos img oldlayer) 1))
)

(define (add-over-layer img newlayer oldlayer)
    (gimp-image-add-layer img newlayer (get-layer-pos img oldlayer))
)

(define (draw-blurshape img drawable size initgrowth sel invert)
  (let* (
            (k initgrowth)
            (currshade 0)
            (i 0)
        )
    (while (< i size)
      (if (> k 0)
        (gimp-selection-grow img k)
        (if (< k 0)
            (gimp-selection-shrink img (abs k))
        )
      )
      (if (= invert 1)
        (begin
            (set! currshade (math-round (* (/ (- size (+ i 1)) size) 255)))
        )
        (begin
            (set! currshade (math-round (* (/ (+ i 1) size) 255)))
        )
      )
      (gimp-context-set-foreground (list currshade currshade currshade))
      (if (= (car (gimp-selection-is-empty img)) 0)
            (gimp-edit-fill drawable FILL-FOREGROUND)
      )
      (gimp-selection-load sel)
      (set! k (- k 1))
      (set! i (+ i 1))
    )
  )
)


(define (gfe-layerfx-bevel-emboss img
               drawable
               style
               depth
               direction
               size
               soften
               angle
               altitude
               glosscontour
               highlightcolor
               highlightmode
               highlightopacity
               shadowcolor
               shadowmode
               shadowopacity
               surfacecontour
               invert
               merge)
  (gimp-image-undo-group-start img)
  (let* (
        (origfgcolor (car (gimp-palette-get-foreground)))
        (origselection (car (gimp-selection-save img)))
        (drwwidth (car (gimp-drawable-width drawable)))
        (drwheight (car (gimp-drawable-height drawable)))
        (drwoffsets (gimp-drawable-offsets drawable))
        (layername (car (gimp-drawable-get-name drawable)))
        (imgtype (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)))
        (lyrgrowamt (math-round (* size 1.2)))
        (bumpmaplayer 0)
        (highlightlayer 0)
        (highlightmask 0)
        (shadowlayer 0)
        (shadowmask 0)
        (layersize 0)
        (alphaSel 0)
        (halfsizef 0)
        (halfsizec 0)
        (origmask 0)
        (alphamask 0)
    )
    (cond
      ((= style 0)
        (begin
            (set! layersize (list
                (+ drwwidth (* lyrgrowamt 2))
                (+ drwheight (* lyrgrowamt 2))
                (- (car drwoffsets) lyrgrowamt)
                (- (cadr drwoffsets) lyrgrowamt)
                )
            )
        )
      )
      ((= style 1)
        (begin
            (set! layersize (list
                drwwidth
                drwheight
                (car drwoffsets)
                (cadr drwoffsets)
                )
            )
        )
      )
      ((= style 2)
        (begin
            (set! layersize (list
                (+ drwwidth lyrgrowamt)
                (+ drwheight lyrgrowamt)
                (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
                (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
                )
            )
        )
      )
      (
        (begin
            (set! layersize (list
                (+ drwwidth lyrgrowamt)
                (+ drwheight lyrgrowamt)
                (- (car drwoffsets) (floor (/ lyrgrowamt 2)))
                (- (cadr drwoffsets) (floor (/ lyrgrowamt 2)))
                )
            )
        )
      )
    )
    (set! bumpmaplayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-bumpmap") 100 LAYER-MODE-NORMAL)))
    (set! highlightlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-highlight") highlightopacity (get-blending-mode highlightmode))))
    (set! shadowlayer (car (gimp-layer-new img (car layersize) (cadr layersize) imgtype (string-append layername "-shadow") shadowopacity (get-blending-mode shadowmode))))
    (add-over-layer img bumpmaplayer drawable)
    (add-over-layer img shadowlayer bumpmaplayer)
    (add-over-layer img highlightlayer shadowlayer)
    (gimp-layer-set-offsets bumpmaplayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets shadowlayer (caddr layersize) (cadddr layersize))
    (gimp-layer-set-offsets highlightlayer (caddr layersize) (cadddr layersize))
    
    (gimp-message "reached 588")
    (gimp-selection-all img)
    ;(gimp-image-select-color img CHANNEL-OP-REPLACE drawable '(254 254 254))
    
    (gimp-context-set-foreground highlightcolor)
    (gimp-edit-fill highlightlayer FILL-FOREGROUND)
    (gimp-context-set-foreground shadowcolor)
    (gimp-edit-fill shadowlayer FILL-FOREGROUND)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill bumpmaplayer FILL-FOREGROUND)
    (set! highlightmask (car (gimp-layer-create-mask highlightlayer ADD-MASK-COPY))) ; was 1=black
    (set! shadowmask (car (gimp-layer-create-mask shadowlayer ADD-MASK-BLACK)))
    (gimp-layer-add-mask highlightlayer highlightmask)
    (gimp-layer-add-mask shadowlayer shadowmask)
    (gimp-selection-layer-alpha drawable)
    ;(gimp-image-select-item img drawable CHANNEL-OP-REPLACE)
    (if (> (car (gimp-layer-get-mask drawable)) -1)
       (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
       ;(gimp-image-select-item img drawable CHANNEL-OP-INTERSECT))
    )
    (gimp-message "reached 608")
    (set! alphaSel (car (gimp-selection-save img)))
    (cond
      ((= style 0)
        (draw-blurshape img bumpmaplayer size size alphaSel 0)
      )
      ((= style 1)
        (draw-blurshape img bumpmaplayer size 0 alphaSel 0)
      )
      ((= style 2)
        (begin
            (set! halfsizec (math-ceil (/ size 2)))
            (draw-blurshape img bumpmaplayer size halfsizec alphaSel 0)
        )
      )
      (
        (begin
            (set! halfsizef (floor (/ size 2)))
            (set! halfsizec (- size halfsizef))
            (gimp-selection-all img)
            (gimp-context-set-foreground '(255 255 255))
            (gimp-edit-fill bumpmaplayer FILL-FOREGROUND)
            (draw-blurshape img bumpmaplayer halfsizec halfsizec alphaSel 1)
            (draw-blurshape img bumpmaplayer halfsizef 0 alphaSel 0)
        )
      )
    )
    (gimp-selection-all img)
    (gimp-context-set-foreground '(127 127 127))
    (gimp-edit-fill highlightmask FILL-FOREGROUND)
    (gimp-selection-none img)
    (if (> surfacecontour 0)
        ;(apply-contour bumpmaplayer 0 surfacecontour)
        (python-fu-colorcontour 1 img bumpmaplayer surfacecontour)
    )
    (if (< angle 0)
      (set! angle (+ angle 360))
    )
    
    (plug-in-bump-map 1 img highlightmask bumpmaplayer angle altitude depth 0 0 0 0 1 direction 0)
    
    (if (> glosscontour 0)
      ;(apply-contour highlightmask 0 glosscontour)
      (python-fu-colorcontour 1 img highlightmask glosscontour)
    )
    (if (> soften 0)
      (plug-in-gauss-rle 1 img highlightmask soften 1 1)
    )
    (if (> invert 0)
      (gimp-drawable-invert highlightmask FALSE)
    )
    (gimp-channel-combine-masks shadowmask highlightmask 2 0 0)
    (gimp-levels highlightmask 0 127 255 1.0 0 255)
    ;(gimp-levels shadowmask 0 0 127 1.0 255 0)
    (gimp-drawable-levels shadowmask HISTOGRAM-VALUE 0.0 0.5 1 1.0 1.0 0.0 1)
    (gimp-selection-load alphaSel)
    (if (= style 0)
        (gimp-selection-grow img size)
        (if (or (= style 2) (= style 3))
            (gimp-selection-grow img halfsizec)
        )
    )
    (gimp-selection-invert img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill shadowmask 0)
    (gimp-selection-none img)
    (gimp-image-remove-layer img bumpmaplayer)
    (if (= merge 1)
      (if (= style 1)
        (begin
            (set! origmask (car (gimp-layer-get-mask drawable)))
            (if (> origmask -1)
                (begin
                    (set! origmask (car (gimp-channel-copy origmask)))
                    (gimp-layer-remove-mask drawable 1)
                )
            )
            (set! alphamask (car (gimp-layer-create-mask drawable 3)))
            (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
            (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
            (gimp-drawable-set-name highlightlayer layername)
            (gimp-layer-add-mask highlightlayer alphamask)
            (gimp-layer-remove-mask highlightlayer 0)
            (if (> origmask -1)
                (gimp-layer-add-mask highlightlayer origmask)
            )
        )
        (begin
            (set! origmask (car (gimp-layer-get-mask drawable)))
            (if (> origmask -1)
                (gimp-layer-remove-mask drawable 0)
            )
            (set! shadowlayer (car (gimp-image-merge-down img shadowlayer 0)))
            (set! highlightlayer (car (gimp-image-merge-down img highlightlayer 0)))
            (gimp-drawable-set-name highlightlayer layername)
        )
      )
    )
    (gimp-context-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaSel)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

;end of script 