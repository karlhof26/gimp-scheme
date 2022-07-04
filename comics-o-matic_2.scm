; Copyright (C) 2013 Stefano Guidoni 

; This program is free software: you can redistribute it and/or modify
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
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;version 0.4 - December 2013


; inch/mm
(define comicsomatic-ratio (/ 10 254))


; a poor man swap function
(define (comicsomatic-choose-wisely inA inB inChoice)
    (if (= inChoice TRUE) inB inA)
)


; returns the ID of the guide at the given position or 0 if there's none
(define (comicsomatic-find-guide image inPosition inOrientation inId)
    (if (= inId 0)
        0
        (let* ( (orientation (car (gimp-image-get-guide-orientation image inId)))
                (position (car (gimp-image-get-guide-position image inId)))
              )
            (if (and (= orientation inOrientation) (= position inPosition))
                inId
                (let* ((guide-id (car (gimp-image-find-next-guide image inId))))
                      (comicsomatic-find-guide image inPosition inOrientation guide-id)
                )
            )
        )
    )
)


; adds a hguide at the given position only if there isn't already one there
(define (comicsomatic-add-hguide image inPosition)
    (let* (
            (guide-id (car (gimp-image-find-next-guide image 0)))
            (guide-id (comicsomatic-find-guide image (truncate inPosition) ORIENTATION-HORIZONTAL guide-id)))
        (if (= guide-id 0)
            (gimp-image-add-hguide image inPosition)
        )  
    )
)


; adds a vguide at the given position only if there isn't already one there
(define (comicsomatic-add-vguide image inPosition)
    (let* (
            (guide-id (car (gimp-image-find-next-guide image 0)))
            (guide-id (comicsomatic-find-guide image (truncate inPosition) ORIENTATION-VERTICAL guide-id))
          )
        (if (= guide-id 0)
            (gimp-image-add-vguide image inPosition)
        )  
    )
)


; comicsomatic-new-canvas
; creates a new canvas
(define (comicsomatic-new-canvas inFormat inDPI)
  (let* ((canvas-dpi inDPI)
         (canvas-width (* canvas-dpi (cond
                                     ((= inFormat 11) 215.9) ; US Legal
                                     ((= inFormat 10) 215.9) ; US Letter
                                     ((= inFormat 9) 257) ; JIS B4
                                     ((= inFormat 8) 182) ; JIS B5
                                     ((= inFormat 7) 128) ; JIS B6
                                     ((= inFormat 6) 250) ; B4
                                     ((= inFormat 5) 176) ; B5
                                     ((= inFormat 4) 125) ; B6
                                     ((= inFormat 3) 160) ; 16x21
                                     ((= inFormat 2) 297) ; A3
                                     ((= inFormat 1) 210) ; A4
                                     (else 148)) comicsomatic-ratio))  ; A5
         (canvas-height (* canvas-dpi (cond
                                      ((= inFormat 11) 355.6) ; US Legal
                                      ((= inFormat 10) 279.4) ; US Letter
                                      ((= inFormat 9) 364) ; JIS B4
                                      ((= inFormat 8) 257) ; JIS B5
                                      ((= inFormat 7) 182) ; JIS B6
                                      ((= inFormat 6) 353) ; B4
                                      ((= inFormat 5) 250) ; B5
                                      ((= inFormat 4) 176) ; B6
                                      ((= inFormat 3) 210) ; 16x21
                                      ((= inFormat 2) 420) ; A3
                                      ((= inFormat 1) 297) ; A4
                                      (else 210)) comicsomatic-ratio))  ; A5
         (image (car (gimp-image-new canvas-width canvas-height RGB)))
         (gimp-image-undo-disable image)
         (bglayer (car (gimp-layer-new image canvas-width canvas-height RGBA-IMAGE "Canvas" 100 LAYER-MODE-NORMAL)))
         (fglayer (car (gimp-layer-new image canvas-width canvas-height RGBA-IMAGE "Panels" 100 LAYER-MODE-NORMAL)))
        )
        
        (gimp-image-set-resolution image canvas-dpi canvas-dpi)
        (gimp-drawable-fill bglayer FILL-WHITE)
        (gimp-image-insert-layer image bglayer 0 -1)
        (gimp-drawable-fill fglayer FILL-TRANSPARENT)
        (gimp-image-insert-layer image fglayer 0 -1)
        (gimp-display-new image)
        (gimp-image-undo-enable image)
        (gimp-displays-flush)
        
        image)
)

(script-fu-register
    "comicsomatic-new-canvas"
    "Create Empty Comic Canvas"
    "Create a new canvas with two layers, one for the drawing, the other for the panels.  \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    ""
    SF-OPTION       "Size"  '("A5" "A4" "A3" "16x21cm" "B6" "B5" "B4" "JIS B6" "JIS B5" "JIS B4" "US Letter" "US Legal")
    SF-ADJUSTMENT   "DPI"   '(300 300 1200 300 300 0 1)
    ;  SF-OPTION _"Units" `("mm" "1/6 in")
)

(script-fu-menu-register "comicsomatic-new-canvas" "<Image>/Script-Fu3/Comics-o-matic/New Canvas/")


; comicsomatic-new-canvas-with-guides
; creates a new canvas and calls comicsomatic-add-framing-guides
(define (comicsomatic-new-canvas-with-guides inFormat inDPI inHDiv inVDiv inUnits inHSpace inVSpace inTMargin inBMargin inLMargin inRMargin inSwap)
  (let* (
            (image (comicsomatic-new-canvas inFormat inDPI))
        )
        (comicsomatic-add-framing-guides image inHDiv inVDiv inUnits inHSpace inVSpace inTMargin inBMargin inLMargin inRMargin inSwap))
)

(script-fu-register
    "comicsomatic-new-canvas-with-guides"
    "Create Canvas with Guides"
    "Create a new canvas with two layers, one for the drawing, the other for the panels, and add a grid of guides to it. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    ""
    SF-OPTION       "Size"      '("A5" "A4" "A3" "16x21cm" "B6" "B5" "B4" "JIS B6" "JIS B5" "JIS B4" "US Letter" "US Legal")
    SF-ADJUSTMENT   "DPI"       '(300 300 1200 300 300 0 1)
    SF-ADJUSTMENT   "Columns"   '(3 1 12 1 1 0 1)
    SF-ADJUSTMENT   "Rows"      '(3 1 12 1 1 0 1)
    SF-OPTION       "Units"                 '("mm" "1/32 in")
    SF-ADJUSTMENT   "Horizontal spacing"    '(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Vertical spacing"      '(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Top margin"            '(8 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Bottom margin"         '(12 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Left margin"           '(3.5 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Right margin"          '(10 0 50 0.5 10 1 0)
    SF-TOGGLE       "Swap left and right margin"    FALSE
)

(script-fu-menu-register "comicsomatic-new-canvas-with-guides" "<Image>/Script-Fu3/Comics-o-matic/New Canvas/")


; comicsomatic-draw-panel
; outlines the current selection with the foreground colour
(define (comicsomatic-draw-panel image inLayer inUnits inLine)
  (let* (
         (ratio (cond 
                ((= inUnits 1) (/ 1 32))
                (else comicsomatic-ratio)))
         (canvas-dpi (car (gimp-image-get-resolution image)))
        )
        (if (= (car (gimp-selection-is-empty image)) TRUE)
            (error _"No selection found. To draw a panel you have to select the panel area."))
        (gimp-image-undo-group-start image)
        (gimp-selection-border image (* ratio canvas-dpi inLine))
        (gimp-selection-sharpen image)
        (gimp-selection-feather image 2)
        (gimp-edit-fill inLayer FILL-FOREGROUND)
        (gimp-selection-none image)
        (gimp-image-undo-group-end image)
        (gimp-displays-flush))
)

(script-fu-register
    "comicsomatic-draw-panel"
    "Draw Panel"
    "Outline the current selection with the foreground color to draw a panel. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE        "Image" 0
    SF-DRAWABLE     "Current Layer" 0
    SF-OPTION       "Units" `("mm" "1/32 in")
    SF-ADJUSTMENT   "Thickness" `(0.5 0.1 5.0 0.1 1 1 0)
)

(script-fu-menu-register "comicsomatic-draw-panel" "<Image>/Script-Fu3/Comics-o-matic")


; comicsomatic-add-framing-guides
; adds guides to the canvas, including those delimiting the drawing area
(define (comicsomatic-add-framing-guides image inHDiv inVDiv inUnits inHSpace inVSpace inTMargin inBMargin inLMargin inRMargin inSwap)
  (let* ((ratio (cond 
                ((= inUnits 1) (/ 1 32))
                (else comicsomatic-ratio)))
         (canvas-dpi (car (gimp-image-get-resolution image)))
         (width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
        )
        
        (gimp-image-undo-group-start image)
        ;  (gimp-image-add-hguide image (* inTMargin canvas-dpi ratio))
        ;  (gimp-image-add-hguide image (- height (* inBMargin canvas-dpi ratio)))
        ;  (gimp-image-add-vguide image (* (comicsomatic-choose-wisely inLMargin inRMargin inSwap) canvas-dpi ratio))
        ;  (gimp-image-add-vguide image (- width (* (comicsomatic-choose-wisely inRMargin inLMargin inSwap) canvas-dpi ratio)))
        (comicsomatic-add-hguide image (* inTMargin canvas-dpi ratio))
        (comicsomatic-add-hguide image (- height (* inBMargin canvas-dpi ratio)))
        (comicsomatic-add-vguide image (* (comicsomatic-choose-wisely inLMargin inRMargin inSwap) canvas-dpi ratio))
        (comicsomatic-add-vguide image (- width (* (comicsomatic-choose-wisely inRMargin inLMargin inSwap) canvas-dpi ratio)))
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (comicsomatic-add-guides image inHDiv inVDiv inUnits inHSpace inVSpace inTMargin inBMargin inLMargin inRMargin inSwap)
  )
)

(script-fu-register
    "comicsomatic-add-framing-guides"
    "Add Guides (framing)"
    "Add a grid of guides at specified positions including framing. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE        "Image" 0
    SF-ADJUSTMENT   "Columns" `(3 1 12 1 1 0 1)
    SF-ADJUSTMENT   "Rows" `(3 1 12 1 1 0 1)
    SF-OPTION       "Units" `("mm" "1/32 in")
    SF-ADJUSTMENT   "Horizontal spacing" `(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Vertical spacing" `(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Top margin" `(8 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Bottom margin" `(12 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Left margin" `(3.5 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Right margin" `(10 0 50 0.5 10 1 0)
    SF-TOGGLE       "Swap left and right margin" FALSE
)

(script-fu-menu-register "comicsomatic-add-framing-guides" "<Image>/Script-Fu3/Comics-o-matic/Guides/") 


; comicsomatic-add-guides
; adds more guides
(define (comicsomatic-add-guides image inHDiv inVDiv inUnits inHSpace inVSpace inTMargin inBMargin inLMargin inRMargin inSwap)
  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (canvas-dpi (car (gimp-image-get-resolution image)))
         (h-lines inVDiv)
         (v-lines inHDiv)
         (h-gap (/ inHSpace 2))
         (v-gap (/ inVSpace 2))
         (iter (- h-lines 1))
         (ratio (cond 
                ((= inUnits 1) (/ 1 32))
                (else comicsomatic-ratio)))
         (inner-width (- width (* (+ (comicsomatic-choose-wisely inLMargin inRMargin inSwap) (comicsomatic-choose-wisely inRMargin inLMargin inSwap)) canvas-dpi ratio)))
         (inner-height (- height (* (+ inTMargin inBMargin) canvas-dpi ratio))))
        
        (gimp-image-undo-group-start image)
        (while (> iter 0)
            (let* ((position (+ (* inTMargin canvas-dpi ratio) (* iter (/ inner-height h-lines)))))
                ;         (gimp-image-add-hguide image (- position (* canvas-dpi ratio v-gap)))
                ;         (gimp-image-add-hguide image (+ position (* canvas-dpi ratio v-gap))))
                (comicsomatic-add-hguide image (- position (* canvas-dpi ratio v-gap)))
                (comicsomatic-add-hguide image (+ position (* canvas-dpi ratio v-gap)))
            )
            (set! iter (- iter 1))
        )
        
        (set! iter (- v-lines 1))
        (while (> iter 0)
            (let* ((position (+ (* (comicsomatic-choose-wisely inLMargin inRMargin inSwap) canvas-dpi ratio) (* iter (/ inner-width v-lines)))))
                ;         (gimp-image-add-vguide image (- position (* canvas-dpi ratio h-gap)))
                ;         (gimp-image-add-vguide image (+ position (* canvas-dpi ratio h-gap))))
                (comicsomatic-add-vguide image (- position (* canvas-dpi ratio h-gap)))
                (comicsomatic-add-vguide image (+ position (* canvas-dpi ratio h-gap)))
            )
            (set! iter (- iter 1))
        )
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
  )
)

(script-fu-register
    "comicsomatic-add-guides"
    "Add Guides (no framing)"
    "Add a grid of guides to the canvas at specified positions \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE        "Image"    0
    SF-ADJUSTMENT   "Columns"           '(3 1 12 1 1 0 1)
    SF-ADJUSTMENT   "Rows"              '(3 1 12 1 1 0 1)
    SF-OPTION       "Units"             '("mm" "1/32 in")
    SF-ADJUSTMENT   "Horizontal spacing"    '(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Vertical spacing"      '(3 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Top margin"            '(8 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Bottom margin"         '(12 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Left margin"           '(3.5 0 50 0.5 10 1 0)
    SF-ADJUSTMENT   "Right margin"          '(10 0 50 0.5 10 1 0)
    SF-TOGGLE       "Swap left and right margin"    FALSE
)

(script-fu-menu-register "comicsomatic-add-guides" "<Image>/Script-Fu3/Comics-o-matic/Guides/")


; comicsomatic-divide-panel
; divides a panel using guides
(define (comicsomatic-divide-panel image inOrientation inUnits inSize inUnits2 inGap inOption)
    (if (= (car (gimp-selection-is-empty image)) TRUE)
        (error _"No selection found. To divide a panel you have to select the panel area.")
    )
  (let* ((rect-x1 (car (cdr (gimp-selection-bounds image))))
         (rect-y1 (car (cdr (cdr (gimp-selection-bounds image)))))
         (rect-x2 (car (cdr (cdr (cdr (gimp-selection-bounds image))))))
         (rect-y2 (car (cdr (cdr (cdr (cdr (gimp-selection-bounds image)))))))
         (canvas-dpi (car (gimp-image-get-resolution image)))
         (size (if (or (= inOrientation 0) (= inOrientation 2))
                   (cond
                   ((= inUnits 2) (* (/ inSize 100) (- rect-y2 rect-y1)))
                   ((= inUnits 1) (* canvas-dpi (/ 1 32) inSize))
                   (else (* canvas-dpi comicsomatic-ratio inSize)))
                   (cond
                   ((= inUnits 2) (* (/ inSize 100) (- rect-x2 rect-x1)))
                   ((= inUnits 1) (* canvas-dpi (/ 1 32) inSize))
                   (else (* canvas-dpi comicsomatic-ratio inSize)))))
         (gap (if (or (= inOrientation 0) (= inOrientation 2))
                   (cond
                   ((= inUnits2 2) (* (/ inSize 100) (- rect-y2 rect-y1)))
                   ((= inUnits2 1) (* canvas-dpi (/ 1 32) inGap))
                   (else (* canvas-dpi comicsomatic-ratio inGap)))
                   (cond
                   ((= inUnits2 2) (* (/ inSize 100) (- rect-x2 rect-x1)))
                   ((= inUnits2 1) (* canvas-dpi (/ 1 32) inGap))
                   (else (* canvas-dpi comicsomatic-ratio inGap)))))
         (gap2 0)
        )
        
        (gimp-image-undo-group-start image)  
        (if (= inOption TRUE)
            (begin
                (set! gap (/ gap 2))
                (set! gap2 gap)
            )
        )
        
        (if (or (= inOrientation 0) (= inOrientation 2))
            (begin
                (comicsomatic-add-hguide image (if (= inOrientation 0) 
                                                      (- (+ rect-y1 size) gap2)
                                                      (- rect-y2 (- size gap2))))
                (comicsomatic-add-hguide image (if (= inOrientation 0) 
                                                      (+ rect-y1 size gap)
                                                      (- rect-y2 (- size gap))))
            )
            (begin
                (comicsomatic-add-vguide image (if (= inOrientation 1) 
                                                      (- (+ rect-x1 size) gap2)
                                                      (- rect-x2 (- size gap2))))
                (comicsomatic-add-vguide image (if (= inOrientation 1) 
                                                      (+ rect-x1 (+ size gap))
                                                      (- rect-x2 (- size gap))))
            )
        )
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
  )
    
)

(script-fu-register "comicsomatic-divide-panel"
    "Divide Panel"
    "Add guides to divide a panel, the total length of the panel is \ndivided between first half, gap and second half,\n
 unless you choose to divide and then subtract the gap length. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE "Image" 0
    SF-OPTION       "Orientation" `("Vertical (Top-Down)" "Horizontal (Left-Right)" "Vertical (Down-Top)" "Horizontal (Right-Left)")
    SF-OPTION       "Units" `("mm" "1/32 in" "%")
    SF-ADJUSTMENT   "Size of the top or left half" `(3 0 100 1 10 1 0)
    SF-OPTION       "Units" `("mm" "1/32 in" "%")
    SF-ADJUSTMENT   "Gap" `(0 0 100 1 10 1 0)
    SF-TOGGLE       "Subtract the gap after splitting the panel" FALSE
)

(script-fu-menu-register "comicsomatic-divide-panel" "<Image>/Script-Fu3/Comics-o-matic/Guides/")


; comicsomatic-add-guide-relative
; adds a guide at a relative position
(define (comicsomatic-add-guide-relative image inPosition inUnits inDistance)
    (if (= (car (gimp-selection-is-empty image)) TRUE)
        (error _"No selection found. To use this function you have to make a selection."))
  (let* ((rect-x1 (car (cdr (gimp-selection-bounds image))))
         (rect-y1 (car (cdr (cdr (gimp-selection-bounds image)))))
         (rect-x2 (car (cdr (cdr (cdr (gimp-selection-bounds image))))))
         (rect-y2 (car (cdr (cdr (cdr (cdr (gimp-selection-bounds image)))))))
         (canvas-dpi (car (gimp-image-get-resolution image)))
         (orientation (if (< inPosition 2)
                          0
                          1))
         (ratio (cond 
                ((= inUnits 1) (/ 1 32))
                (else comicsomatic-ratio)))
         (position (cond
                   ((= inPosition 3) (+ rect-x2 (* canvas-dpi (* ratio inDistance))))
                   ((= inPosition 2) (- rect-x1 (* canvas-dpi (* ratio inDistance))))
                   ((= inPosition 1) (+ rect-y2 (* canvas-dpi (* ratio inDistance))))
                   (else (- rect-y1 (* canvas-dpi ratio inDistance))))
         )
        )
        
        (gimp-image-undo-group-start image)  
        (if (= orientation 0)
            (comicsomatic-add-hguide image position)
            (comicsomatic-add-vguide image position)
        )
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
  )
    
)

(script-fu-register "comicsomatic-add-guide-relative"
    "Add Guide at a Relative Position"
    "Add a guide at a given position, relative to a selection. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE "Image" 0
    SF-OPTION _"Position" `(_"Above" _"Beneath" _"Left" _"Right")
    SF-OPTION _"Units" `("mm" "1/32 in")
    SF-ADJUSTMENT _"Distance" `(3 0.1 100 1 10 1 0)
)

(script-fu-menu-register "comicsomatic-add-guide-relative" "<Image>/Script-Fu3/Comics-o-matic/Guides/")

; comicsomatic-add-graph-paper-layer
; adds a layer with a graph paper
(define (comicsomatic-add-graph-paper-layer image inColour inStyle)
  (let* (
         
         (ratio (cond 
                ((= inStyle 6) (/ 38 40))
                ((= inStyle 5) (/ 3 4))
                ((= inStyle 4) (/ 1 2))
                ((= inStyle 3) (/ 1 3))
                ((= inStyle 2) (/ 1 6))
                ((= inStyle 1) (* 10 comicsomatic-ratio))
                (else comicsomatic-ratio)))
         (div1  (cond 
                ((= inStyle 6) 6)
                ((= inStyle 5) 10)
                ((= inStyle 4) 20)
                ((= inStyle 3) 12)
                ((= inStyle 2) 20)
                ((= inStyle 1) 15); was 1
                (else 75)))
         (div2  (cond 
                ((= inStyle 6) 3)
                ((= inStyle 5) 5)
                ((= inStyle 4) 10)
                ((= inStyle 3) 6)
                ((= inStyle 2) 10)
                ((= inStyle 1) 5); was 10
                (else 15)))
         (canvas-dpi (car (gimp-image-get-resolution image)))
         (width (car (gimp-image-width image)))
         (height (car (gimp-image-height image)))
         (colour (car (gimp-context-get-foreground)))
         (position (* ratio canvas-dpi))
         (index 1)
         ;(gimp-image-undo-disable image)
         (pglayer (car (gimp-layer-new image width height RGBA-IMAGE _"Graph Paper" 100 LAYER-MODE-NORMAL)))
        )
        
        (gimp-image-undo-group-start image)
        
        (gimp-drawable-fill pglayer FILL-TRANSPARENT)
        (gimp-image-insert-layer image pglayer 0 -1)
        
        (gimp-context-set-foreground inColour)
        (set! position (* ratio canvas-dpi))
        (while (< position width)
            (let* ((thickness (if (= 0 (modulo index div1))    ;(modulo index div1))
                                     5 
                              (if (= 0 (modulo index div2))    ;(modulo index div2))
                                         3 
                                         1))) ; was 5 3 2
                  )
                  
                  (if (= thickness 5)
                        (begin
                            ;(gimp-message "77777777777777777777777777777777")
                            ;(quit)
                        )
                        (begin
                            (if (= thickness 3)
                                (begin
                                    ;(gimp-message "2TWO2TWO2TWO2TWO2TWO2TWO2TWO")
                                )
                            )
                        )
                  )
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE (- position 1) 0 thickness height)
                (set! index (+ 1 index))
                (set! position (* index (* ratio canvas-dpi)))
                (gimp-edit-fill pglayer FILL-FOREGROUND)
            )
        )
        (set! index 1)
        (set! position (* ratio canvas-dpi))
        (while (< position height)
            (let* ((thickness (if (= 0 (modulo index div1)) 
                                     5
                              (if (= 0 (modulo index div2)) 
                                         3 
                                         1)))
                  )
                (gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 (- position 1) width thickness)
                (set! index (+ 1 index))
                (set! position (* index (* ratio canvas-dpi)))
                (gimp-edit-fill pglayer FILL-FOREGROUND)
            )
        )
        (gimp-context-set-foreground colour)
        (gimp-selection-none image)
        
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
    )
)

(script-fu-register  "comicsomatic-add-graph-paper-layer"
    "Add Graph Paper Layer"
    "Add a graph paper layer. \nfile:comics-o-matic_2.scm"
    "Stefano Guidoni"
    "Stefano Guidoni"
    "November 2013"
    "RGB* RGBA*"
    SF-IMAGE    "Image"         0
    SF-COLOR    "Color"         '(230 158 20)
    SF-OPTION   "Style"         '("mm" "cm" "1/6 in" "1/3 in" "1/2 in" "0.75 in" "1.0 in")
)

(script-fu-menu-register "comicsomatic-add-graph-paper-layer" "<Image>/Script-Fu3/Comics-o-matic/Guides")

;end of script