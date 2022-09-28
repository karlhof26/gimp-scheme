; Pin-Stripe Border.scm
;
; Adds a dark and light stripe border to the current layer
;  described by Oregonian on GIMP Chat
;
; Version 1.0       July 2011       Brian Hahn 
; Version 1.1       July 2020       karlhof26
;
; License:  GNU Public License, version 2 or later
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; edited for 2.8.15 Graechan 26/11/14
;
;;;; define new brush for drawing operation
(define (build-brush-circle brushName hardness spacing radius brush-color)
    
    (gimp-brush-new brushName)
    (gimp-brush-set-shape brushName BRUSH-GENERATED-CIRCLE) ;{ BRUSH-GENERATED-CIRCLE (0), BRUSH-GENERATED-SQUARE (1), BRUSH-GENERATED-DIAMOND (2) }    
    (gimp-brush-set-spikes brushName 2)
    (gimp-brush-set-hardness brushName hardness)                   
    (gimp-brush-set-aspect-ratio brushName 1.0)
    (gimp-brush-set-angle brushName 0.0)                       
    (gimp-brush-set-spacing brushName spacing)
    (gimp-brush-set-radius brushName radius)            
    (gimp-context-set-brush brushName)
    (if (defined? 'gimp-context-set-brush-default-size) 
        (gimp-context-set-brush-default-size)
    )
    (gimp-context-set-foreground brush-color)
)
    
;;Rem (gimp-brush-delete "outlineBrush") ;delete the created brush

(define (script-fu-pin-stripe-border img drw border-width color1 color2 merge-down)
    (let* (
            (width (car (gimp-drawable-width drw)))
            (height (car (gimp-drawable-height drw)))
            (layer-x-offset (car (gimp-drawable-offsets drw)))
            (layer-y-offset (cadr (gimp-drawable-offsets drw)))
            (border-layer 0)
            (brushName 0)
        )
        
        (if (= (car (gimp-drawable-is-layer drw)) FALSE)
            (begin
                (gimp-message "Drawable is not a layer.\nSelect a layer and try again.")
                (quit)
            )
        )
        
        (gimp-context-push)
        (gimp-image-undo-group-start img)
        
        (gimp-drawable-set-visible drw TRUE)                    ;make sure calling layer is visible
        (set! border-layer (car (gimp-layer-copy drw TRUE)))    ;copy layer, adding alpha if necessary
        
        (gimp-drawable-fill border-layer FILL-TRANSPARENT)      ;clear the border layer
        (gimp-image-add-layer img border-layer -1)              ;add to image
        (gimp-drawable-set-name border-layer "Border")
        (gimp-layer-set-mode border-layer LAYER-MODE-OVERLAY)
        
        
        
        (gimp-context-set-paint-mode  LAYER-MODE-NORMAL)        ;set paint modes just to be safe
        (gimp-context-set-opacity 100)
        (gimp-context-set-paint-method "gimp-paintbrush")
        
        ;use rectangle select because the brush stroke 
        ; is offset by one half pixel to the right and bottom.
        ; the brush width is compensated to give sharp edges
        
        (gimp-rect-select img layer-x-offset layer-y-offset (- width 1) (- height 1) CHANNEL-OP-REPLACE FALSE 0)
        
        ;first stroke at full width
        (build-brush-circle "outlineBrush" 1 10 (- border-width 0.5) color1)
        (gimp-edit-stroke border-layer)
        (gimp-brush-delete "outlineBrush")
        
        ;pin-stripe color at 80% width
        (build-brush-circle "outlineBrush" 1 10 (- (* border-width 0.8) 0.5) color2)
        (gimp-edit-stroke border-layer)
        (gimp-brush-delete "outlineBrush")
        
        ;outer stripe at 50% width
        (build-brush-circle "outlineBrush" 1 10 (- (* border-width 0.5) 0.5) color1)
        (gimp-edit-stroke border-layer)
        (gimp-brush-delete "outlineBrush")
        
        (gimp-selection-none img)
        (if (= merge-down TRUE)
            (gimp-image-merge-down img border-layer 1)
        )                   ;merge border down
        
        ;done, final clean-up
        (gimp-image-undo-group-end img)
        (gimp-context-pop)
        (gimp-displays-flush)
    )
)

(script-fu-register
    "script-fu-pin-stripe-border"
    "<Image>/Script-Fu/Edges/Pin-Stripe Border..."
    "Adds a pin-stripe border to the current layer \nfile:Pin-Stripe Border.scm"
    "Brian Hahn"
    "Brian Hahn"
    "July 2011"
    "GRAY* RGB*"
    SF-IMAGE        "image"                 0
    SF-DRAWABLE     "drawable"              0
    SF-ADJUSTMENT   "Border width"          '(5 1 100 1 2 0 1)
    SF-COLOR        "Primary color"         "black"
    SF-COLOR        "Accent strip color"    "white"
    SF-TOGGLE       "Merge border down?"    FALSE
)

;;