;;  ***************************************************************************
;;  *   Copyright (C) 2011 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *              Edited to create Octagons instead of circles               *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************

(define (script-fu-octagongrid-kh
    gType
    circDiam
    xCirc
    yCirc
    oBorder
    gapSpace
    gridColor
    bgColor
    )
    
    (let* (
            (dummy_kh 1)
            (s3 (sqrt 3))
            (circRad (/ circDiam 2.0))
            (tBorder (* 2 oBorder))
        
            (xGap 0)
            (yGap 0)
            (inWidth 0)
            (inHeight 0)
            (theImage 0)
            (baseLayer 0)
            
            (xFlag 0)
            (yFlag 0)
            (xStart 0.0)
            (yStart 0.0)
            (rowCheck 0.0)
        )
        ;(gimp-message "started OK")
        (gimp-context-push)
        (cond
            ((= gType 0)
                (set! inWidth (+ tBorder (+ (* circDiam xCirc) (* gapSpace (- xCirc 1)))))
                (set! inHeight (+ tBorder (+ (* circDiam yCirc) (* gapSpace (- yCirc 1)))))
            )
            ((= gType 1)
                (set! xGap (* s3 (+ circRad (* 0.5 gapSpace))))
                (set! yGap (* gapSpace (- yCirc 1)))
                (set! inWidth (+ (+ tBorder circDiam) (* (- xCirc 1) xGap)))
                (set! inHeight (+ (+ (+ tBorder circRad) yGap) (+ (* gapSpace 0.5) (* circDiam yCirc))))
            )
        )
        
        (set! theImage (car (gimp-image-new inWidth inHeight RGB)))
        (set! baseLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Circles in the Sand" 100 LAYER-MODE-NORMAL)))
        
        (gimp-image-insert-layer theImage baseLayer 0 0)
        
        (gimp-context-set-background bgColor)
        (gimp-context-set-foreground gridColor)
        
        (gimp-drawable-fill baseLayer FILL-BACKGROUND)
        
        (while (< xFlag xCirc)
            (cond
                ((= gType 0)
                    (set! xStart (+ oBorder (* xFlag (+ circDiam gapSpace))))
                )
                ((= gType 1)
                    (if (= (fmod xFlag 2) 1)
                        (set! rowCheck (+ (* gapSpace 0.5) circRad))
                    )
                    (set! xStart (+ oBorder (* s3 (+ circRad (* 0.5 gapSpace)) xFlag)))
                )
            )
            
            (while (< yFlag yCirc)
                (set! yStart (+ rowCheck (+ oBorder (+ (* gapSpace yFlag) (* circDiam yFlag)))))
                
                (gimp-free-select theImage 16
                    (vector (+ xStart (* circDiam 0.2929)) yStart
                        (+ xStart (* circDiam 0.7071)) yStart
                        (+ xStart circDiam) (+ yStart (* circDiam 0.2929))
                        (+ xStart circDiam) (+ yStart (* circDiam 0.7071))
                        (+ xStart (* circDiam 0.7071)) (+ yStart circDiam)
                        (+ xStart (* circDiam 0.2929)) (+ yStart circDiam)
                        xStart (+ yStart (* circDiam 0.7071))
                        xStart (+ yStart (* circDiam 0.2929))
                    )
                    CHANNEL-OP-REPLACE TRUE FALSE 0)
                (gimp-edit-bucket-fill baseLayer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 255 0 0 0)
                (set! yFlag (+ 1 yFlag))
            )
            
            (set! xFlag (+ xFlag 1))
            (set! yFlag 0)
            (set! rowCheck 0.0)
        )
        
        (gimp-selection-none theImage)
        (gimp-context-pop)
        (gimp-display-new theImage)
        (gimp-context-set-dynamics "Dynamics Off")
        (gc) ; garbage collect 
    )
)

(script-fu-register
    "script-fu-octagongrid-kh"
    "Grid - Octagons..."
    "Creates a grid of X by Y octagons. either in rectangular or hexagonal packing. - \nfile:JMS-Grid_Create_Octagons.scm"
    "James Sambrook"
    "5 April 2011"
    "James Sambrook.  King George, VA, USA"
    ""
    SF-OPTION       "Grid Type"                      '("Rectangular"
                                                        "Hexagonal")
    SF-ADJUSTMENT   "Circular Diameter"              '(40 10 200 1 5 0 0)
    SF-ADJUSTMENT   "Circles in X Direction"         '(10 1 100 1 5 0 0)
    SF-ADJUSTMENT   "Circles in y Direction"         '(10 1 100 1 5 0 0)
    SF-ADJUSTMENT   "Outer Border Around Circles"    '(10 1 100 1 5 0 0)
    SF-ADJUSTMENT   "Gap Between the Circles"        '(10 1 100 1 5 0 0)
    SF-COLOR        "Color for grid lines"           '(0 0 0)
    SF-COLOR        "Color for rectangle interior"   '(255 255 255)
)

(script-fu-menu-register "script-fu-octagongrid-kh"
    "<Toolbox>/Script-Fu/Render/Pattern/")
    
; end of script