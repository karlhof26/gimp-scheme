; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2010 James Huang <elastic192@gmail.com>
; http://tw.myblog.yahoo.com/jw!5nbbef.RAx8aIbO00RkKIw--
; idPhoto.scm ver 1.0 大頭照排版

(define (script-fu-idPhoto inImage inLayer outDPI outMode outWD outHT inMode inWD inHT wLeft
                    wTop wRight wBottom vframe? flatten?)
  (let* (
            (theImage inImage)
            (outImage)
            (outWidth)
            (outHeight)
            (inWidth)
            (inHeight)
            (frameWidth)
            (frameHeight)
            (frameLayer)
            (bgLayer)
            (newLayer)
            (cpLayer)
            (selRect)
            (selX)
            (selY)
            (selNX)
            (selNY)
            (selWD)
            (selHT)
            (tmpV)
            (col)
            (row)
            (sW)
            (sH)
            (sX)
            (sY)
            (offX)
            (offY)
            (cntC)
            (cntR)
          )
        
        (cond
            ((= outMode 0)      ; 4x6
                (set! outWidth (* outDPI 6))
                (set! outHeight (* outDPI 4)))
            ((= outMode 1)      ; 3.5x5
                (set! outWidth (* outDPI 5))
                (set! outHeight (* outDPI 3.5)))
            ((= outMode 2)      ; custom
                (set! outWidth outWD)
                (set! outHeight outHT))
        ) ; end of cond
        
        (cond
            ((= inMode 0)       ; 2" 3.50x4.50cm
                (set! inWidth (* outDPI 1.377))
                (set! inHeight (* outDPI 1.771)))
            ((= inMode 1)       ; 1" 2.75x3.50cm
                (set! inWidth (* outDPI 1.082))
                (set! inHeight (* outDPI 1.377)))
            ((= inMode 2)       ; (US) 2" 5.0x5.0cm
                (set! inWidth (* outDPI 1.967))
                (set! inHeight (* outDPI 1.967)))
            ((= inMode 3)       ; custom
                (set! inWidth (* outDPI (/ inWD 2.54))) 
                (set! inHeight (* outDPI (/ inHT 2.54))))
        ) ; end of cond
        
        (set! frameWidth (+ inWidth wLeft))
        (set! frameWidth (+ frameWidth wRight))
        (set! frameHeight (+ inHeight wTop))
        (set! frameHeight (+ frameHeight wBottom))
        
        (set! col (truncate (/ outWidth frameWidth)))
        (set! sW (- outWidth (* frameWidth col)))
        (set! sW (truncate (/ sW (+ col 1))))
        
        (set! row (truncate (/ outHeight frameHeight)))
        (set! sH (- outHeight (* frameHeight row)))
        (set! sH (truncate (/ sH (+ row 1))))
        
        (gimp-context-push)
        
        (set! outImage (car (gimp-image-new outWidth outHeight RGB)))
        (gimp-image-undo-disable outImage)
        
        (if (= 1 (car (gimp-selection-is-empty theImage)))  
            (gimp-selection-all theImage)
        )
        (set! selRect (gimp-selection-bounds theImage))
        
        (set! selX (cadr selRect))
        (set! selY (caddr selRect))
        (set! selWD (- (cadr (cddr selRect)) selX))
        (set! selHT (- (caddr (cddr selRect)) selY))
        (set! selNX selX)
        (set! selNY selY)
        
        (if (>= selWD selHT)
            (begin
                (set! tmpV (/ (* inHeight selWD) inWidth))
                (set! selNY (- selNY (/ (- tmpV selHT) 2)))
                (if (< selNY 0)
                    (set! selNY 0)
                )
                (set! selHT tmpV)
            )
            (begin
                (set! tmpV (/ (* inWidth selHT) inHeight))
                (set! selNX (- selNX (/ (- tmpV selWD) 2)))
                (if (< selNX 0)
                    (set! selNX 0)
                )
                (set! selWD tmpV)
            )
        )
        
        (gimp-image-select-rectangle theImage CHANNEL-OP-REPLACE selNX selNY selWD selHT)
        ;(gimp-rect-select theImage selNX selNY selWD selHT REPLACE 0 0)
        
        (gimp-edit-copy inLayer)
        
        (set! bgLayer (car (gimp-layer-new outImage outWidth outHeight RGBA-IMAGE "Background" 100 0)))
        (gimp-image-add-layer outImage bgLayer 0)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-fill bgLayer FILL-FOREGROUND)
        
        (set! frameLayer (car (gimp-layer-new outImage outWidth outHeight RGBA-IMAGE "Frame" 20 0)))
        (gimp-image-insert-layer outImage frameLayer 0 0)
        
        (set! cpLayer (car (gimp-edit-paste frameLayer TRUE)))
        (gimp-floating-sel-to-layer cpLayer)
        (gimp-layer-scale-full cpLayer inWidth inHeight TRUE INTERPOLATION-LANCZOS)
        
        (gimp-context-set-foreground '(255 0 0))
        ;(gimp-brushes-set-brush "Circle (01)")
        
        (set! cntR 0)
        (set! sY sH)
        (while (< cntR row)
            (set! cntC 0)
            (set! sX sW)
            (while (< cntC col)
                ;(gimp-rect-select outImage sX sY frameWidth frameHeight REPLACE 0 0)
                (gimp-image-select-rectangle outImage CHANNEL-OP-REPLACE sX sY frameWidth frameHeight)
                (gimp-edit-fill frameLayer FILL-FOREGROUND)
                (gimp-selection-shrink outImage 1)
                (gimp-edit-clear frameLayer)
                ;(gimp-edit-stroke frameLayer)
                (gimp-selection-none outImage)
                
                (set! newLayer (car (gimp-layer-copy cpLayer TRUE)))
                (gimp-image-insert-layer outImage newLayer 0 0)
                (set! offX (+ sX wLeft))
                (set! offY (+ sY wTop))
                (gimp-layer-set-offsets newLayer offX offY)
                
                (set! sX (+ sX (+ frameWidth sW)))
                (set! cntC (+ cntC 1))
            )
            (set! sY (+ sY (+ frameHeight sH)))
            (set! cntR (+ cntR 1))
        )
        (gimp-image-remove-layer outImage cpLayer)
        (gimp-image-set-active-layer outImage frameLayer) 
        
        (if (= vframe? FALSE)
            (gimp-layer-set-visible frameLayer FALSE)
        )
        
        (if (= flatten? TRUE)
            (gimp-image-flatten outImage)
        )
        
        (gimp-image-undo-enable outImage)
        (gimp-display-new outImage)
        
        (gimp-context-pop)
  )
)

(script-fu-register "script-fu-idPhoto"
            "IDPhoto..."
            "Created an IdPhoto from an image. The layer is duplicated in a grid so that idPhoto's can be produced. \nfile:idPhoto.scm"
            "JamesH"
            "JamesH"
            "2010/06/09"
            "RGB*"
            SF-IMAGE        "SF-IMAGE" 0
            SF-DRAWABLE     "SF-DRAWABLE" 0
            
            SF-ADJUSTMENT   "DPI"               '(300 10 1000 10 100 0 0)
            
            SF-OPTION       "Print paper size"    '("4x6 in" "3.5x5 in" "Custom")
            SF-ADJUSTMENT   "Width(pixels)" '(1800 100 6000 10 100 0 0)
            SF-ADJUSTMENT   "Height(pixels)" '(1200 100 6000 10 100 0 0)
            
            SF-OPTION       "idPhoto size"   '("2in 3.50x4.50cm" "1in 2.75x3.50cm" "2in(US) 2 5.0x5.0cm" "Custom")
            SF-ADJUSTMENT   "idPhoto width(cm)" '(3.50 1.00 10.00 0.01 0.50 1 0)
            SF-ADJUSTMENT   "idPhoto height(cm)" '(4.50 1.00 10.00 0.01 0.50 1 0)
            
            SF-ADJUSTMENT   "Border left(pixels)" '(8 0 200 1 10 0 0)
            SF-ADJUSTMENT   "Border top(pixels)" '(8 0 200 1 10 0 0)
            SF-ADJUSTMENT   "Border right(pixels)" '(8 0 200 1 10 0 0)
            SF-ADJUSTMENT   "Border bottom(pixels)" '(8 0 200 1 10 0 0)
            
            SF-TOGGLE       "Display border"    FALSE
            SF-TOGGLE       "Flatten"           TRUE
)

(script-fu-menu-register "script-fu-idPhoto"
            "<Image>/Script-Fu/Edges")
    
;end of script