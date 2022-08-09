; RMA_selection_blend.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.1 (20120215)

; Description 
;
; Fills a selection with a gradient, following the selection boundaries, horizontally or vertically.
; Updated with a gradient picker and a "reverse" direction option.
;

; Changelog
;
; v1.1 - Added gradient picker and reverse option with some performance enhancements.

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (RMA_selection_blend img inLayer inGrad inRev inMethod)
  (let* (
            (width (car (gimp-image-width img)))
            (height (car (gimp-image-height img)))
            (selbounds (gimp-selection-bounds img))
            (inGrad (if (equal? inGrad "") (car (gimp-context-get-gradient)) inGrad))
            (origimg img)
            (origlayer inLayer)
            (img (car (gimp-image-duplicate img)))
            (inLayer (car (gimp-image-get-active-layer img)))
            (selchannel 0)
            (counter 0)
            (countto 0)
            (selsize 0)
            (slice 0)
            (errhandler (car (gimp-message-get-handler)))
        )
    
    (gimp-message-set-handler CONSOLE)
    
    (gimp-image-undo-group-start origimg)
    (gimp-image-undo-freeze img)
    (gimp-context-push)
    (gimp-context-set-gradient inGrad)
    
    (if (zero? (car selbounds)) (gimp-selection-all img))
    
    (set! selchannel (car (gimp-selection-save img)))
    
    (cond
      ((= inMethod 0) ; Horizontal
            (set! counter (caddr selbounds))
            (set! countto (cadddr (cdr selbounds)))
            (set! selsize (- (cadddr selbounds) (cadr selbounds)))
            (while (< counter countto)
                (gimp-rect-select img (cadr selbounds) counter selsize 1 CHANNEL-OP-REPLACE FALSE 0)
                (gimp-selection-combine selchannel CHANNEL-OP-INTERSECT)
                (set! slice (cdr (gimp-selection-bounds img)))
                (if (zero? inRev)
                    (gimp-edit-blend inLayer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE 
                        (car slice) (cadr slice) (caddr slice) (cadr slice))
                    (gimp-edit-blend inLayer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE 
                        (caddr slice) (cadr slice) (car slice) (cadr slice))
                )
                (set! counter (+ counter 1))
            )
      )
      ((= inMethod 1) ; Vartical
            (set! counter (cadr selbounds))
            (set! countto (cadddr selbounds))
            (set! selsize (- (cadddr (cdr selbounds)) (caddr selbounds)))
            (while (< counter countto)
                (gimp-rect-select img counter (list-ref selbounds 2) 1 selsize CHANNEL-OP-REPLACE FALSE 0)
                (gimp-selection-combine selchannel CHANNEL-OP-INTERSECT)
                (set! slice (cdr (gimp-selection-bounds img)))
                (if (zero? inRev)
                    (gimp-edit-blend inLayer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE 
                        (car slice) (cadr slice) (car slice) (cadddr slice))
                    (gimp-edit-blend inLayer BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE 
                        (car slice) (cadddr slice) (car slice) (cadr slice 2))
                )              
                (set! counter (+ counter 1))
            )
      )
    )
        
        (if (zero? (car selbounds))
            (gimp-selection-none img)
            (gimp-selection-load selchannel)
        )
        
        (gimp-edit-copy inLayer)
        (gimp-floating-sel-anchor (car (gimp-edit-paste origlayer TRUE)))
        
        (gimp-displays-flush)
        (gimp-context-pop)
        (gimp-image-undo-thaw img)
        (gimp-image-delete img)
        
        (gimp-image-undo-group-end origimg)
        
        (gimp-message-set-handler errhandler)
        
  )
)

(script-fu-register "RMA_selection_blend"
                    "Selection Blend. Fill w Gradient"
                    "Fills a selection with a gradient, following the selection boundaries, horizontally or vertically. \nfile:RMA_selection_blend.scm"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Feb 2012"
                    "RGB*, GREY*"
                    SF-IMAGE      "image"              0
                    SF-DRAWABLE   "Layer"              0
                    SF-GRADIENT   "Gradient"           ""
                    SF-TOGGLE     "Reversed"           FALSE
                    SF-OPTION     "Method"            (list "Horizontal" "Vertical")
)

(script-fu-menu-register "RMA_selection_blend"
                         "<Toolbox>/Script-Fu2/Select")
                         
;end of script