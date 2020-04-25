;;; Studio Tecnico Arch. Giuseppe Conte  
;;; via Roma, 28 
;;; 72026 - San Pancrazio Salentino (BR) - Italy
;;;
;;; Plugin  : draw-polygon.scm
;;; Author  : Arch. Giuseppe Conte 
;;; Date    : 08 maggio 2002 -  Florence - Italy
;;; Revision: 19 giugno 2004
;;; 					13 dicembre 2007 update to TinyScheme
;;;						20 marzo 2008
;;; Version : 2.4.1
;;; Last version at: http://xoomer.alice.it/lwcon/gimp/script-fu/draw-polygon.htm
;;; Help guide at  : http://xoomer.alice.it/lwcon/gimp/script-fu/draw-polygon.htm
;;; Required : Gimp 2.4.1 or last
;;;
;;; Description: 
;;; Draw all poligonos at center point specified with the brush and color active.
;;; -----------------------------------------------------------------------------
;;;
;;; License:
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Define the function:

(define (script-fu-draw-polygon inImage inLayer dx dy radius lati)
    
    (let* (	    
            ;;(segment)
                    (segment (make-vector 4 ))   
                    (stepx dx)
                    (stepy dy)
                    (raggio radius)
                    (nlati lati)
                    (angolo)
                    (arco)
                    (beta)
                    (gamma)
                    (gcount)
                    (npoint)
          )
        
        (set! angolo (/ 360 lati))
        (set! arco (* angolo (/ 3.1415 180)))
        (set! beta 0)
        (set! gamma 0)
        (set! gcount 1)
        (set! npoint 4)
        
        ;inizio delle operazioni che potranno essere annullate con un solo undo
        (gimp-undo-push-group-start inImage)
        
        
        (while (<= gcount nlati) 
            (begin
                (set! beta (+ beta arco))
                (set! gamma (- beta arco))
                (vector-set! segment 0 (round(+ (* raggio (cos gamma)) stepx)))
                (vector-set! segment 1 (round(+ (- (* raggio (sin gamma))) stepy)))
                (vector-set! segment 2 (round(+ (* raggio (cos beta)) stepx)))
                (vector-set! segment 3 (round(+ (- (* raggio (sin beta))) stepy)))
                
                (gimp-pencil inLayer npoint segment)
                (set! gcount (+ gcount 1))
                
            );end begin
        );end while
        
        ;fine delle operazioni che potranno essere annullate con un solo undo
        (gimp-undo-push-group-end inImage) 
        
        (gimp-displays-flush)
        
 );;let
) ;;def

(script-fu-register
    "script-fu-draw-polygon"
    "<Toolbox>/Script-Fu/Draw/Draw Polygon"
    "Draws regular polygons. Input: center, radius, number segment. Uses the settings for the pencil. \n file:draw-polygon_02.scm"
    "Arch. Giuseppe Conte <http://space.tin.it/edicola/lwcon/>"
    "2008, Giuseppe Conte"
    "20 marzo 2008 - San Pancrazio Salentino (BR) - Italy"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE "The Image" 0
    SF-DRAWABLE "The Layer" 0
    SF-ADJUSTMENT "X center" '(200 0 9999 1 10 0 1)
    SF-ADJUSTMENT "Y center" '(200 0 9999 1 10 0 1)
    SF-ADJUSTMENT "radius" '(10 0 9999 1 10 0 1)
    SF-ADJUSTMENT "Numero di lati" '(3 0 9999 1 10 0 1)
)

; end of script