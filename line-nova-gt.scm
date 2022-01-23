;;; line-nova-gt.scm for gimp-2.x -*-scheme-*-
;;; Time-stamp: <1998/11/25 13:26:44 narazaki@gimp.org>
;;; Author Shuji Narazaki <narazaki@gimp.org>
;;; Version 0.8  - Updated By GnuTux - 10-2014 - http://gimpchat.com
;;; Version 0.9  - Updated By Graechan 10-2014 - Path Select
;;; Version 0.91 - Updated By Graechan 10-2014 - Gradient Nova
;;; Version 0.92 - Updated By Graechan 10-2014 - Bug Fix
;;; Version 0.93 - Updated by karlhof26 01-03-2020 - Fixed for Gimp 2.10.18
; line-nova-gt.scm  
; version 2.10.18 [gimphelp.org]
; modified/tested by Graechan
; 10/2014 on GIMP-2.8.10
; last modified/tested by karlhof26
; 01/03/2020 on GIMP-2.10.18
;
;==============================================================
;
; Installation: 
; This script should be placed in the user or system-wide script folder.
;
;	Windows Vista/7/8)
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Users\YOUR-NAME\.gimp-2.8\scripts
;	
;	Windows XP
;	C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;	or
;	C:\Documents and Settings\yourname\.gimp-2.8\scripts   
;    
;	Linux
;	/home/yourname/.gimp-2.8/scripts  
;	or
;	Linux system-wide
;	/usr/share/gimp/2.0/scripts
;
;==============================================================
;
; LICENSE
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
;==============================================================
; Original information 
;   - Changelog -
; version 0.1  2001/04/15 iccii <iccii@hotmail.com>
;     - Initial relased
; version 0.1a 2001/07/20 iccii <iccii@hotmail.com>
;     - more simple
; version 0.1b Receved as completely broken, doing just gausian blur. Fixed to 
; do something that may have been the authors intent.
;==============================================================

(define (script-fu-line-nova-gt img lyr fill-type nova-color nova-gradient num-of-lines corn-deg offset variation feather invert ellipse)
    
    (gimp-image-undo-group-start img) ;Graechan   
    
    (let* (         
            (copylyr 0)
            (layer 0)
            (inVectors 0) ;find the active path
            (num-strokes 0) ;get the number of strokes
            (stroke-list 0)
            (stroke-id 0) ;get the stroke id
            (num-points 0)
            (control-points 0)
            (anchor-point1x 0)
            (anchor-point1y 0)
            (anchor-point2x 0)
            (anchor-point2y 0)
            (co-ordCounter 0) ;prepare the co-ordinate counter for anchor-point routines
            (f1 FALSE)
            (lenx 0)
            (leny 0)
            (drw 0)
            (saved-selection (car (gimp-selection-save img)))
            (clearance-selection 0)
            (layer-name 0)      
        )
        
        (set! copylyr (car (gimp-layer-copy lyr TRUE)))
        (gimp-image-insert-layer img copylyr 0 (+ (car (gimp-image-get-layer-position img lyr)) 1))
        (set! layer copylyr) 
        (gimp-layer-resize-to-image-size lyr)
        
        (if (not (= (car (gimp-image-get-active-vectors img)) -1))
            (begin
                (set! f1 TRUE)
                (set! layer lyr)
                (set! inVectors (car (gimp-image-get-active-vectors img))) ;find the active path
                (set! num-strokes (car (gimp-vectors-get-strokes inVectors))) ;get the number of strokes
                (set! stroke-list (vector->list (cadr (gimp-vectors-get-strokes inVectors))))
                (set! stroke-id (car stroke-list)) ;get the stroke id
                (set! num-points (cadr (gimp-vectors-stroke-get-points inVectors stroke-id)))
                (set! control-points (vector->list (caddr (gimp-vectors-stroke-get-points inVectors stroke-id))))         
                
                (set! co-ordCounter 2)
                (while (> co-ordCounter 0)
                    (set! anchor-point1x (car control-points))
                    (set! control-points (cdr control-points))
                    (set! anchor-point1y (car control-points))
                    (set! control-points (cdr control-points))
                    (set! co-ordCounter (- co-ordCounter 1))
                )
                (set! control-points  (vector->list (caddr (gimp-vectors-stroke-get-points inVectors stroke-id))))   
                
                (set! co-ordCounter 5)
                (while (> co-ordCounter 0)
                    (set! anchor-point2x (car control-points))
                    (set! control-points (cdr control-points))
                    (set! anchor-point2y (car control-points))
                    (set! control-points (cdr control-points))
                    (set! co-ordCounter (- co-ordCounter 1))
                )
                (set! control-points  (vector->list (caddr (gimp-vectors-stroke-get-points inVectors stroke-id))))
                
                (set! lenx (- (max anchor-point1x anchor-point2x) (min anchor-point1x anchor-point2x)))
                (set! leny (- (max anchor-point1y anchor-point2y) (min anchor-point1y anchor-point2y)))
                (set! offset (max lenx leny))   
            )
        )   
        
        (set! drw (car (gimp-layer-new img (car (gimp-drawable-width layer)) (car (gimp-drawable-height layer)) (car (gimp-drawable-type-with-alpha lyr)) "Line Nova" 100 NORMAL-MODE))) ;Graechan
        (gimp-image-insert-layer img drw 0 (+ (car (gimp-image-get-layer-position img lyr)) 0))
        (gimp-layer-set-offsets drw (car (gimp-drawable-offsets layer)) (cadr (gimp-drawable-offsets layer))) ;Graechan
        (if (= f1 FALSE)
            (set! anchor-point1x (/ (car (gimp-drawable-width drw)) 2))
        ) ;Graechan
        (if (= f1 FALSE) (set! anchor-point1y (/ (car (gimp-drawable-height drw)) 2))) ;Graechan
        
        (gimp-layer-translate drw (- anchor-point1x (/ (car (gimp-drawable-width drw)) 2))   (- anchor-point1y (/ (car (gimp-drawable-height drw)) 2))) ;Graechan
        ;(gimp-message "line 143")
        (gimp-image-select-ellipse img CHANNEL-OP-REPLACE
            (car (gimp-drawable-offsets copylyr)) ;x
            (cadr (gimp-drawable-offsets copylyr)) ;y
            (car (gimp-drawable-width copylyr)) ;width
            (car (gimp-drawable-height copylyr)) ;height
            ;2 ;operation
            ;TRUE ;antialias
            ;TRUE ;feather
            ;feather
          ) ;feather-radius
        
        (gimp-selection-invert img)
        (set! clearance-selection (car (gimp-selection-save img)))
        (gimp-selection-none img)   
        ;(gimp-message "line 158")
        (gimp-selection-load saved-selection)
        (gimp-image-remove-channel img saved-selection)
        
        (let* (
                (*points* (cons-array (* 3 2) 'double))
                (modulo fmod)                        ; in R4RS way
                (pi/2 (/ *pi* 2))
                (pi/4 (/ *pi* 4))
                (pi3/4 (* 3 pi/4))
                (pi5/4 (* 5 pi/4))
                (pi3/2 (* 3 pi/2))
                (pi7/4 (* 7 pi/4))
                (2pi (* 2 *pi*))
                (rad/deg (/ 2pi 360))
                (variation/2 (/ variation 2))
                (drw-width (car (gimp-drawable-width drw)))
                (drw-height (car (gimp-drawable-height drw)))
                (drw-offsets (gimp-drawable-offsets drw))
                (old-selection 0)
                (f2 FALSE)
                (radius (max drw-height drw-width))
                (index 0)
                (dir-deg/line (/ 360 num-of-lines))      
              )
            
            (gimp-context-push)
            (gimp-context-set-foreground nova-color)
            (gimp-context-set-gradient nova-gradient)
            (gimp-layer-resize-to-image-size drw) ;Graechan
            ;(gimp-message "line 188")
            
            (define (draw-vector beg-x beg-y direction)
            
                (define (set-point! index x y)
                    (aset *points* (* 2 index) x)
                    (aset *points* (+ (* 2 index) 1) y)
                )
                (define (deg->rad rad)
                    (* (modulo rad 360) rad/deg)
                )
                (define (set-marginal-point beg-x beg-y direction)
                    (let (
                            (dir1 (deg->rad (+ direction corn-deg)))
                            (dir2 (deg->rad (- direction corn-deg)))
                          )
                        ;(gimp-message "line 204")
                        
                        (define (aux dir index)
                            (set-point! index
                                (+ beg-x (* (cos dir) radius))
                                (+ beg-y (* (sin dir) radius)))
                        )
                        
                        (aux dir1 1)
                        (aux dir2 2)
                    )
                )
                
                (let (
                        (dir0 (deg->rad direction))
                        (off (+ offset (- (modulo (rand) variation) variation/2)))
                     )
                    ;(gimp-message "line 221")
                    
                    (set-point! 0
                        (+ beg-x (* off (cos dir0)))
                        (+ beg-y (* off (sin dir0)))
                    )
                    (set-marginal-point beg-x beg-y direction)
                    (gimp-image-select-polygon img CHANNEL-OP-ADD 6 *points*)
                )
            )
            
            
            ;(gimp-message "line 233")
            
            (if (= (car (gimp-selection-is-empty img)) FALSE)
                (begin
                    (set! old-selection (car (gimp-selection-save img)))
                    (set! f2 TRUE)
                )
            )   
            
            (gimp-selection-none img)
            (srand (realtime))
            (while (< index num-of-lines)
                (draw-vector (+ (nth 0 drw-offsets) (/ drw-width 2))
                    (+ (nth 1 drw-offsets) (/ drw-height 2))
                    (* index dir-deg/line)
                )
                (set! index (+ index 1))
            )
            ;(gimp-message "line 251")
            
            (if (= invert FALSE)
                (gimp-selection-invert img)
            )
            
            (gimp-edit-bucket-fill drw 0 0 100 0 FALSE 0 0)
            
            (if (= fill-type 1)
                (begin
                    (let* (
                            (x1 anchor-point1x)
                            (y1 anchor-point1y)
                            (x2 0)
                            (y2 0)
                          )   
                        
                        (set! x2 (cond ((> anchor-point1x (- (car (gimp-drawable-width layer)) anchor-point1x)) 
                            (car (gimp-drawable-offsets layer)))
                            (else 
                                (car (gimp-drawable-width layer)))
                                )
                        )
                        (set! y2 (cond ((> anchor-point1y (- (car (gimp-drawable-height layer)) anchor-point1y))
                            (cadr (gimp-drawable-offsets layer)))
                            (else (car (gimp-drawable-height layer)))))
                        
                        (gimp-edit-blend drw BLEND-CUSTOM LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE
                            FALSE 3 0.2 TRUE x1 y1 x2 y2)
                    ) ;end gradient fill   
                )
            ) ;endif
            
            (gimp-image-select-rectangle img CHANNEL-OP-REPLACE
                  (car (gimp-drawable-offsets copylyr))
                  (cadr (gimp-drawable-offsets copylyr))
                  (car (gimp-drawable-width copylyr)) ;width
                  (car (gimp-drawable-height copylyr)) ;height
                   ;2 ;operation{ CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) }
                  ;FALSE ;feather
                  ;0
                )
            
            (let ((pasted 0))
                (gimp-edit-cut drw)
                (gimp-image-remove-layer img drw)
                (set! drw (car (gimp-layer-new img (car (gimp-drawable-width copylyr)) (car (gimp-drawable-height copylyr)) (car (gimp-drawable-type-with-alpha lyr)) "Line Nova" 100 NORMAL-MODE))) ;Graechan
                (gimp-image-insert-layer img drw 0 -1) ;Graechan
                (gimp-layer-set-offsets drw (car (gimp-drawable-offsets copylyr)) (cadr (gimp-drawable-offsets copylyr))) ;Graechan
                (set! pasted (car (gimp-edit-paste drw FALSE)))
                (gimp-floating-sel-anchor pasted)
            )
            (set! layer-name (car (gimp-drawable-get-name lyr)))
            (set! lyr (car (gimp-image-merge-down img lyr CLIP-TO-BOTTOM-LAYER)))
            (gimp-drawable-set-name lyr layer-name)
            (gimp-image-set-active-layer img drw)
            
            (if (or (= ellipse TRUE) (= f1 TRUE))
                (begin
                    (gimp-selection-load clearance-selection)
                    (gimp-edit-clear drw)
                )
            )
            (gimp-image-remove-channel img clearance-selection)
            (gimp-selection-none img)
            
            (if (= f2 TRUE)
                (begin
                    (gimp-image-select-item img CHANNEL-OP-REPLACE old-selection)
                    (gimp-image-remove-channel img old-selection)
                )
            )
            
            (gimp-image-undo-group-end img)
            (gimp-displays-flush)
            (gimp-context-pop)
        ) ;end script variables    
    ); end path variables
) ;end procedure


(script-fu-register "script-fu-line-nova-gt"
    "Line Nova GT..."
    "Fill a layer with rays emanating outward from its center using selected color. \nfile:line-nova-gt.scm"
    "Shuji Narazaki <narazaki@gimp.org>"
    "Shuji Narazaki - GnuTux - Graechan - karlhof26"
    "1997,1998,2014,2020"
    "*"
    SF-IMAGE        "Image"                 0
    SF-DRAWABLE     "Drawable"              0
    SF-OPTION       "Nova Fill Type"        '("Color" "Gradient")
    SF-COLOR        "Nova Color"            '(255 231 72)
    SF-GRADIENT     "Nova Gradient"         "Abstract 3"
    SF-ADJUSTMENT   "Number of lines"       '(90 10 1000 1 1 0 1)
    SF-ADJUSTMENT   "Sharpness (gaps between)"   '(1.0 0.0 10.0 0.1 1 1 1)
    SF-ADJUSTMENT   "Inner radius Offset"         '(40 0 2000 1 1 0 1)
    SF-ADJUSTMENT   "Randomness"            '(30 1 2000 1 1 0 1)
    SF-ADJUSTMENT   "Ellipse Feather Value" '(15 0 30 1 5 0 1)
    SF-TOGGLE       "Invert Nova"           FALSE
    SF-TOGGLE       "Ellipse Default:(only if path = null)"         FALSE
    
)

(script-fu-menu-register "script-fu-line-nova-gt"
                         "<Image>/Script-Fu2/Render")

;end of script