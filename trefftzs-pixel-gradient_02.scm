;  
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; pixel gradient script  for GIMP 2.10.22
; Original author: Jeff Trefftzs <trefftzs@tcsn.net>
;
; Tags: effect
;
; Author statement:
;
;
; --------------------------------------------------------------------
; Distributed by Gimp FX Foundry project
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define the function:

(define (script-fu-pixelgradient inImage inLayer minsize maxsize nsteps)
    ;; If there isn't already a selection, select the whole thing
    (let* (
            (noselection 0)
        )
        (if (car (gimp-selection-bounds inImage))
            (begin
                (set! noselection FALSE)
                ;(gimp-message "selection true")
            )
            (begin
                (gimp-selection-all inImage)
                ;(gimp-message "selection false")
                (set! noselection TRUE)
            )
        )
        
        (let* (
                (selchannel (car (gimp-selection-save inImage)))
                (selstuff  (gimp-selection-bounds inImage))
                (width
                    (cond ((car selstuff)
                            (- (nth 3 selstuff) (nth 1 selstuff)))
                        (t (car (gimp-image-width inImage)))
                    )
                )
                (height
                    (cond ((car selstuff)
                            (- (nth 4 selstuff) (nth 2 selstuff)))
                        (t (car (gimp-image-height inImage)))
                    )
                )
                (x0
                    (cond ((car selstuff)
                            (nth 1 selstuff))
                        (t 0)
                    )
                )
                (y0
                    (cond ((car selstuff)
                            (nth 2 selstuff))
                        (t 0)
                    )
                )
                (x1 width)
                (y1 height)
                (stepwidth 0)
                (pixstep 0)
                (startx x0)
                (startsize minsize)
                
                (karltemp 5.123)
                (sel-x1 0)
                (sel-y1 0)
                (sel-x2 0)
                (sel-y2 0)
                (selectionWidth 0)
                (selectionHeight 0)
                (endnow 1)
              )
            
            (gimp-image-undo-group-start inImage)
            
            (gimp-progress-init "Pixelise" -1)
            ;; Step across the selection (or image), pixelizing as we go
            ;; next line added by karlhof26 to ensure no early stop of the while loop
            (set! x1 (+ x1 x0))
            (set! x1 (+ x1 1))
            
            ;(gimp-message "debug start")
            (set! karltemp 4.234)
            ;(gimp-message (number->string karltemp))
            ;(gimp-message (number->string x1))
            ;save dimensions and position of the selection 
            ;(gimp-message "debug start2")
            
            ; rounding added by karlhof26 to ensure no rounding errors
            (set! stepwidth (round (/ width nsteps)))
            (set! pixstep (round (/ (- maxsize minsize) nsteps)))
            
            (if (= noselection FALSE)
                (begin
                    ;(gimp-message "through to here - not needed now")
                    (set! sel-x1 (cadr (gimp-selection-bounds inImage)))
                    (set! sel-y1 (caddr (gimp-selection-bounds inImage)))
                    (set! sel-x2 (cadddr (gimp-selection-bounds inImage)))
                    (set! sel-y2 (car (cddddr (gimp-selection-bounds inImage))))
                    (set! selectionWidth (- sel-x2 sel-x1))
                    (set! selectionHeight (- sel-y2 sel-y1))
                )
            )
            
            ;(gimp-message "debug start the while")
            ;(gimp-message (number->string width)) 
            ;(gimp-message (number->string x1)) 
            ;(gimp-message (number->string x0)) 
            (gimp-message (number->string stepwidth)) 
            ;(gimp-message (number->string startx)) 
            (gimp-message (number->string pixstep)) 
            ;(gimp-message (number->string karltemp)) 
            
            (gimp-progress-pulse)
            
            (while (and (< startx x1) (= endnow 1)) 
                (begin
                    ;; (gimp-selection-load selchannel)
                    (gimp-image-select-item inImage CHANNEL-OP-REPLACE selchannel)
                    (gimp-image-select-rectangle inImage 
                        CHANNEL-OP-INTERSECT 
                        startx y0 
                        stepwidth height
                    )
                    
                    (plug-in-pixelize TRUE inImage inLayer startsize)
                    
                    (gimp-progress-update (/ startx x1))
                    
                    (set! startx (+ startx stepwidth))
                    (set! startsize (+ startsize pixstep))
                    (if (> startx x1)
                        (begin
                            ;(gimp-message "ready to end already")
                            (set! endnow 2)
                        )
                    )
                    
                    ;(gimp-message "update crash")
                    (gimp-displays-flush)
                    ;(quit)
                    
                )
            )
            
            (if (equal? TRUE noselection)
                (gimp-selection-none inImage)
                (gimp-selection-load selchannel)
            )
            
            (gimp-message "Good finish OK")
            (gimp-image-set-active-layer inImage inLayer)
            (gimp-image-undo-group-end inImage)
            (gimp-displays-flush)
            (gc) ; garbage collect
        )
    )
)

(script-fu-register
    "script-fu-pixelgradient"
    "<Toolbox>/Script-Fu2/Selection Effects/Pixel Gradient"
    "Pixelizes a selection (or layer) from left to right with increasing pixel sizes. \nfile:trefftzs-pixel-gradient_02.scm"
    "Jeff Trefftzs"
    "Copyright 2003, Jeff Trefftzs"
    "November 17, 2003"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE      "The Image"       0
    SF-DRAWABLE   "The Layer"       0
    SF-ADJUSTMENT "Minimum Pixel Size"  '(5 1 256 1 5 0 1)
    SF-ADJUSTMENT "Maximum Pixel Size"  '(64 1 256 1 5 0 1)
    SF-ADJUSTMENT "Number of Steps"     '(7 1 256 1 5 0 1)
)

; end of script