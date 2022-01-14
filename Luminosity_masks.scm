; following steps in tutorial by Pat David 
; found at link: http://www.gimp.org/tutorials/Luminosity_Masks/
; This script will generate 9 extra layers named "L","LL","LLL","D","DD","DDD","M","MM","MMM"
; where L stands for Light-tones, D stands for Dark-tones, M stands for Mid-tones
; these layers will be masked as instructed in the tutorial
; so that the user can manipulate these layers at free will. 
; Note: not mentioned in tutorial is how to create MM and MMM layers without them having black masks
; according to saul goode's script found at http://chiselapp.com/user/saulgoode/repository/script-fu/artifact/636ac4e7aa1a57e21e04f507b5db9399c37cda66
; MM is created by inverting LL then SUBTRACT DD
; MMM is created by inverting LLL then subtract DDD
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;
;
; author: Tin Tran
; date: 2014

(define (script-fu-luminosity-masks image layer 
        )
    (let* (
            (dummy-variable-to-make-let-happy 0)
            (i 0)               ;just a counter for loops
            ;all below variables for saving visibility states for hiding all layers and showing them later when DONE
            (get-layers-returned-values)
            (layers-visibility-array)     ;array of bytes to store visibility flags
            (layers-name-vector)          ;vector to store layers' names
            (layers-count)
            (layers-array)
            (current-layer)
            (current-layer-name)
            (visibility-flag)
            
            
            (L-channel)
            (LL-channel)
            (LLL-channel)
            (D-channel)
            (DD-channel)
            (DDD-channel)
            (M-channel)
            (MM-channel)
            (MMM-channel)
            (L-layer)
            (LL-layer)
            (LLL-layer)
            (D-layer)
            (DD-layer)
            (DDD-layer)
            (M-layer)
            (MM-layer)
            (MMM-layer)
            (channel1)
            (channel2)
            (new-mask)  ;we need to create a mask before adding it to layer
            (copied-layer)    ;this layer will be desaturated to create Light,Dark and Mid channels
          )
        (gimp-image-undo-group-start image)                   ;undo-group in one step
        ;GIMP 2.6 backward compatibility check---------------------------------
        (if (not (defined? 'gimp-item-get-name)) ;Check for 2.8 proc
            (begin
                (define (gimp-item-get-name layer) ;Define it
                    ;(gimp-layer-get-name layer)    ;Call 2.6 proc
                    (gimp-drawable-get-tattoo layer)
                )
            )
        )
        (if (not (defined? 'gimp-item-get-visible)) ;Check for 2.8 proc
            (begin
                (define (gimp-item-get-visible layer) ;Define it
                    (gimp-drawable-get-visible layer)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-item-set-visible)) ;Check for 2.8 proc
            (begin
                (define (gimp-item-set-visible layer visible) ;Define it
                    (gimp-drawable-set-visible layer visible)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-image-get-item-position)) ;Check for 2.8 proc
            (begin
                (define (gimp-image-get-item-position image layer) ;Define it
                    (gimp-image-get-layer-position image layer)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-item-delete)) ;Check for 2.8 proc
            (begin
                (define (gimp-item-delete drawable) ;Define it
                    (gimp-drawable-delete drawable)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-item-set-name)) ;Check for 2.8 proc
            (begin
                (define (gimp-item-set-name drawable name) ;Define it
                    (gimp-drawable-set-name drawable name)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-image-insert-layer)) ;Check for 2.8 proc
            (begin
                (define (gimp-image-insert-layer image layer parent position) ;Define it
                    (gimp-image-add-layer image layer position)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-image-insert-channel)) ;Check for 2.8 proc
            (begin
                (define (gimp-image-insert-channel image channel parent position) ;Define it
                    (gimp-image-add-channel image channel position)    ;Call 2.6 proc
                )
            )
        )
        (if (not (defined? 'gimp-image-get-layer-by-name)) ;Check for 2.8 proc
            (begin
                (define (gimp-image-get-layer-by-name image tattoo) ;Define it
                    (gimp-image-get-layer-by-tattoo image tattoo)    ;Call 2.6 proc
                )
            )
        )
        
        ;-----------------------------------------------------------------
        (set! get-layers-returned-values (gimp-image-get-layers image))
        (set! layers-count (car get-layers-returned-values))
        (set! layers-array (cadr get-layers-returned-values))
        ;gets layers visibility and names and save it before hiding them all so that we can set it back later
        (set! layers-visibility-array (cons-array layers-count 'byte))
        (set! layers-name-vector (make-vector layers-count 'string))
        (set! i 0)
        (while (< i layers-count)
            (set! current-layer (aref layers-array i))                ;set the current layer we're looking at
            (vector-set! layers-name-vector i (car (gimp-item-get-name current-layer))) ;saves current the layer's name
            (aset layers-visibility-array i (car (gimp-item-get-visible current-layer))) ;saves the visibility of current layer
            (set! i (+ i 1))
        )
        ;hide all layers
        (set! i 0)
        (while (< i layers-count)
            (gimp-item-set-visible (aref layers-array i) FALSE)
            (set! i (+ i 1))
        )
        ;-------------------------------------------------------------------------
        (gimp-layer-resize-to-image-size layer)               ;layer to image size
        (gimp-selection-none image)                           ;selects none
        
        ;make a copy then desaturate it
        (set! copied-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image copied-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image copied-layer)
        (gimp-drawable-desaturate copied-layer DESATURATE-LUMINANCE) ;{ DESATURATE-LIGHTNESS (0), DESATURATE-LUMINOSITY (1), DESATURATE-AVERAGE (2) }
        (gimp-item-set-visible copied-layer TRUE)
        ;creates L channel from any channel since we desaturated it
        (set! L-channel (car (gimp-channel-new-from-component image BLUE-CHANNEL "L")))
        (gimp-image-insert-channel image L-channel 0 0)
        ;creates D channel
        (set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel2 (car (gimp-channel-copy L-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! D-channel channel1)     ;save D-channel
        (gimp-image-insert-channel image D-channel 0 0)
        ;creates DD-channel
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy D-channel)))
        (set! channel2 (car (gimp-channel-copy L-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        ;(gimp-item-delete channel2)
        (set! DD-channel channel1)     ;save DD-channel
        (gimp-image-insert-channel image DD-channel 0 0)
        (gimp-item-set-name DD-channel "DD")
        ;creates DDD-channel by subtracting L from D twice
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy D-channel)))
        (set! channel2 (car (gimp-channel-copy L-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! DDD-channel channel1)     ;save DD-channel
        (gimp-image-insert-channel image DDD-channel 0 0)
        (gimp-item-set-name DDD-channel "DDD")
        
        
        ;creates LL-channel by subtracting D from L 
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy L-channel)))
        (set! channel2 (car (gimp-channel-copy D-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! LL-channel channel1)     ;save DD-channel
        (gimp-image-insert-channel image LL-channel 0 0)
        (gimp-item-set-name LL-channel "LL")
        
        ;creates LLL-channel by subtracting D from L twice
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy L-channel)))
        (set! channel2 (car (gimp-channel-copy D-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! LLL-channel channel1)     ;save DD-channel
        (gimp-image-insert-channel image LLL-channel 0 0)
        (gimp-item-set-name LLL-channel "LLL")
        
        
        ;creates M-channel by intersecting D from L 
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy L-channel)))
        (set! channel2 (car (gimp-channel-copy D-channel)))
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-INTERSECT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! M-channel channel1)     ;save DD-channel
        (gimp-image-insert-channel image M-channel 0 0)
        (gimp-item-set-name M-channel "M")
        
        
        ;creates MM-channel by intersecting DD from LL 
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy LL-channel)))
        (set! channel2 (car (gimp-channel-copy DD-channel)))
        (gimp-image-insert-channel image channel1 0 0)
        (gimp-invert channel1)
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! MM-channel channel1)     ;save DD-channel
        ;(gimp-image-insert-channel image MM-channel 0 0)
        (gimp-item-set-name MM-channel "MM")
        
        ;creates MMM-channel by intersecting DDD from LLL 
        ;(set! channel1 (car (gimp-channel-new-from-component image ALPHA-CHANNEL "D")))
        (set! channel1 (car (gimp-channel-copy LLL-channel)))
        (set! channel2 (car (gimp-channel-copy DDD-channel)))
        
        (gimp-image-insert-channel image channel1 0 0)
        (gimp-invert channel1)
        (gimp-channel-combine-masks channel1 channel2
                                    CHANNEL-OP-SUBTRACT ;CHANNEL-OP-ADD (0), CHANNEL-OP-SUBTRACT (1), CHANNEL-OP-REPLACE (2), CHANNEL-OP-INTERSECT (3) 
                                    0 ;offset x
                                    0 ;offset y
                    )
        ;result is stored in channel1 so we delete channel2 as we don't need it anymore
        (gimp-item-delete channel2)
        (set! MMM-channel channel1)     ;save DD-channel
        ;(gimp-image-insert-channel image MMM-channel 0 0)
        (gimp-item-set-name MMM-channel "MMM")
        
        ;now we add layers with masks
        ;L-layer
        (set! L-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image L-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image L-layer)
        (gimp-image-set-active-channel image L-channel)
        (set! new-mask (car (gimp-layer-create-mask L-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask L-layer new-mask)
        (gimp-item-set-name L-layer "L")
        ;LL-layer
        (set! LL-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image LL-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image LL-layer)
        (gimp-image-set-active-channel image LL-channel)
        (set! new-mask (car (gimp-layer-create-mask LL-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask LL-layer new-mask)
        (gimp-item-set-name LL-layer "LL")
        ;LLL-layer
        (set! LLL-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image LLL-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image LLL-layer)
        (gimp-image-set-active-channel image LLL-channel)
        (set! new-mask (car (gimp-layer-create-mask LLL-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask LLL-layer new-mask)
        (gimp-item-set-name LLL-layer "LLL")
        
        ;D-layer
        (set! D-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image D-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image D-layer)
        (gimp-image-set-active-channel image D-channel)
        (set! new-mask (car (gimp-layer-create-mask D-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask D-layer new-mask)
        (gimp-item-set-name D-layer "D")
        
        ;DD-layer
        (set! DD-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image DD-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image DD-layer)
        (gimp-image-set-active-channel image DD-channel)
        (set! new-mask (car (gimp-layer-create-mask DD-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask DD-layer new-mask)
        (gimp-item-set-name DD-layer "DD")
        
        ;DDD-layer
        (set! DDD-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image DDD-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image DDD-layer)
        (gimp-image-set-active-channel image DDD-channel)
        (set! new-mask (car (gimp-layer-create-mask DDD-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask DDD-layer new-mask)
        (gimp-item-set-name DDD-layer "DDD")
        
        ;M-layer
        (set! M-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image M-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image M-layer)
        (gimp-image-set-active-channel image M-channel)
        (set! new-mask (car (gimp-layer-create-mask M-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask M-layer new-mask)
        (gimp-item-set-name M-layer "M")
        
        ;MM-layer
        (set! MM-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image MM-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image MM-layer)
        (gimp-image-set-active-channel image MM-channel)
        (set! new-mask (car (gimp-layer-create-mask MM-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask MM-layer new-mask)
        (gimp-item-set-name MM-layer "MM")
        
        
        ;MMM-layer
        (set! MMM-layer (car(gimp-layer-copy layer TRUE)))    ;copies the active layer
        (gimp-image-insert-layer image MMM-layer 0 (car (gimp-image-get-item-position image layer)))      ;insert the copied layer above active layer
        (gimp-image-set-active-layer image MMM-layer)
        (gimp-image-set-active-channel image MMM-channel)
        (set! new-mask (car (gimp-layer-create-mask MMM-layer ADD-MASK-CHANNEL))) ;creates the mask before adding it
        (gimp-layer-add-mask MMM-layer new-mask)
        (gimp-item-set-name MMM-layer "MMM")
        
        
        (gimp-image-remove-layer image copied-layer) ;deletes the desaturated layer
        (gimp-image-remove-channel image L-channel)
        (gimp-image-remove-channel image LL-channel)
        (gimp-image-remove-channel image LLL-channel)
        (gimp-image-remove-channel image D-channel)
        (gimp-image-remove-channel image DD-channel)
        (gimp-image-remove-channel image DDD-channel)
        (gimp-image-remove-channel image M-channel)
        (gimp-image-remove-channel image MM-channel)
        (gimp-image-remove-channel image MMM-channel)
        
        
        ;DONE -----------------------------------------------------------------
        ;set visibility back to how it was before started running script
        (set! i 0)
        (while (< i layers-count)
            (set! current-layer-name (vector-ref layers-name-vector i)) ;grabs current-layer-name as loop runs through saved vector
            (set! current-layer (car(gimp-image-get-layer-by-name image current-layer-name))) ;sets current-layer by names saved in vectors
            (set! visibility-flag (aref layers-visibility-array i)) ;grabs the saved visibility flag
            (gimp-item-set-visible current-layer visibility-flag)         ;sets visibility to originally saved values
            (set! i (+ i 1))
        )
        (gimp-image-undo-group-end image)                     ;undo group in one step
        (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-luminosity-masks"         ;function name
    "<Image>/Script-Fu/Layer/Luminosity Masks"    ;menu register
    "Generates 9 layers with different luminosity masks. This script will generate 9 extra layers named L,LL,LLL,D,DD,DDD,M,MM,MMM
    where L stands for Light-tones, D stands for Dark-tones, M stands for Mid-tones. 
    These layers will be masked so that the user can manipulate these layers at will. \nfile:Luminosity_masks.scm"       ;description
    "Tin Tran"                              ;author name
    "copyright info and description"        ;copyright info or description
    "2014"                                  ;date
    "RGB*, GRAY*"                           ;mode
    SF-IMAGE      "Image"       0                   
    SF-DRAWABLE   "Layer"       0
)

;end of script