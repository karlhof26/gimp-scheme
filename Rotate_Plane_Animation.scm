; create animation layers for rotating plane
; found at link: http://gimpchat.com/viewtopic.php?f=23&t=594
; author: Tin Tran 
; date: 2014
;
;; Tin Tran
;
;; This script was tested with Gimp 2.10.22
;;
;;
;;
;;This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses>.
;;
; ;Define the function

(define (script-fu-rotate-plane image layer
                                frames-per-layer
                                rotation-type
         )
    (define (get-visibles image)
    (let loop ((layers (vector->list (cadr (gimp-image-get-layers image))))
               (visibles '()) )
      (if (null? layers)
        visibles
        (loop (cdr layers) 
              (if (zero? (car (gimp-drawable-get-visible (car layers))))
                visibles
                (cons (car layers) visibles) ) ) ) ) )
    (let* 
        (
            (visibles (get-visibles image))
            (current-layer -1)
            (copied-layer -1)
            (horizontal-rotation 0)
            (vertical-rotation 0)
            (new-name "")
            (frame 0)
            (original-layers visibles)
        )
    
    (gimp-image-undo-group-start image)                   ;undo-group in one step
    
    (while (not (null? visibles))
        (set! current-layer (car visibles))
        (gimp-layer-add-alpha current-layer)
        (set! frame 0)
        (while (< frame frames-per-layer)
            (set! copied-layer (car (gimp-layer-copy current-layer TRUE)))
            (gimp-image-insert-layer image copied-layer (car (gimp-item-get-parent current-layer)) 0) ; Add it
            
            (if (= rotation-type 0);;Horizontal rotation
                (begin
                    (set! horizontal-rotation (round (+ -90 (* (/ 180.0 frames-per-layer) frame))))
                    (set! vertical-rotation 0.0)
                )
            )
            (if (= rotation-type 1);;Vertical rotation
                (begin
                    (set! horizontal-rotation 0.0)
                    (set! vertical-rotation (round (+ -90 (* (/ 180.0 frames-per-layer) frame))))
                )
            )
            (if (= rotation-type 2);;Both rotations (horizontal and vertical)
                (begin
                    (set! horizontal-rotation (round (+ -90 (* (/ 180.0 frames-per-layer) frame))))
                    (set! vertical-rotation (round (+ -90 (* (/ 180.0 frames-per-layer) frame))))
                )
            )
            (set! new-name (string-append (car (gimp-item-get-name current-layer)) "_"
                                            (number->string horizontal-rotation) "_"
                                            (number->string vertical-rotation) " (replace)"))
            (gimp-item-set-name copied-layer new-name)
            (plug-in-map-object 1 image copied-layer 
                                        0 ;0-plane 1-sphere 2-box 3-cylinder
                                        .5 ;viewpoint-x
                                        .5 ;viewpoint-y
                                        2 ;viewpoint-z
                                        .5 ; position-x
                                        .5 ; position-y
                                        0  ; position-z
                                        1.0  ;first axis x
                                        0  ;first axis y
                                        0  ;first axis z
                                        0  ;second axis x
                                        1.0  ;second axis y
                                        0  ;second axis z
                                        0  ;rotate -x
                                        horizontal-rotation ;rotate y
                                        vertical-rotation ;rotate z
                                        0 ; 0-point-light 1-directional 2-none
                                        '(255 255 255) ;light color
                                        -0.50 ;light position x
                                        -0.50 ;light position y
                                        2.00  ;light position z
                                        0 ;light direction x
                                        0 ;light direction y
                                        0 ;light direction z
                                        .30  ;ambient-intensity
                                        1.00 ;diffuse-intensity
                                        .50  ;diffuse-reflectivity
                                        .50  ;specular-reflectivity
                                        27.00 ; highlight
                                        TRUE ;antialiasing
                                        FALSE ;tiled
                                        FALSE ;newimage
                                        TRUE ;transparent background
                                        0 ; radius
                                        0 ;x-scale
                                        0 ;y-scale
                                        0 ;z-scale
                                        0 ;cylinder-length
                                        -1 ;box front drarwable
                                        -1 ;box back drawable
                                        -1 ;box top drawable
                                        -1 ;box bottom drawable
                                        -1 ;box left drawable
                                        -1 ;box right drawable
                                        -1 ;cyl-top-drawable
                                        -1 ;cyl-bottom-drawable
            )
            (set! frame (+ frame 1))
        ) ;end inner-while
            ;(gimp-item-set-visible current-layer FALSE) ;maybe deletet layer instead.
            
        (set! visibles (cdr visibles))
    ) ;end while
    (map (lambda (x) (gimp-image-remove-layer image x)) original-layers)
    
    (gimp-image-undo-group-end image)        ;undo-group in one step
    (gimp-displays-flush)
    ) ;end of let
)     ;end of define

(script-fu-register
  "script-fu-rotate-plane"                  ;function name
  "<Toolbox>/Script-Fu2/Create from Image/Rotate Plane Animation..."    ;menu register
  "Create animation layers for rotating plane animation. \nfile:Rotate_Plane_Animation.scm"       ;description
  "Tin Tran"                                ;author name
  "copyright info and description"          ;copyright info or description
  "2014"                                    ;date
  "RGB*, GRAY*"                             ;mode
  SF-IMAGE      "Image"                 0                   
  SF-DRAWABLE   "Layer"                 0
  SF-ADJUSTMENT "Frames per layer"      '(12 1 30 1 10 0 1)
  SF-OPTION     "Rotation Type"         '("Horizontal" "Vertical" "Both")
)

;end of script