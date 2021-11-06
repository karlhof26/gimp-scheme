;
; "Graynite" slab (Granite? I don't know...)
;
; Based on "Graynite button" script. This uses BumpMaps (Read: Much cooler!)
;
; Copyright (C) 8.3.1998 by Urpo Lankinen.
; Original version (granitebutton.scm):
;     Copyright (C) 15.12.1997 by Urpo Lankinen.
;  Distributed under GPL. Permission granted to distribute this script
;  with anything that has something to do with The GIMP.
;
; RCS: $Id: graniteslab.scm,v 1.1 1998/05/10 06:25:46 urpo Exp urpo $ 
;
; Changed on June 15, 2000 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 1.1.26
;
; Changed on December 8, 2003 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 1.2
;
; Changed on January 29, 2004 by Kevin Cozens <kcozens@interlog.com>
; Updated for GIMP 2.0pre3
;
; Changed on November 11, 2021 by Karlhof26
; Updated for GIMP 2.10.24

(define (script-fu-granite-slab text font size slabedge slabdepth textdepth carvetog graniteshade)
  (let* (
            (img (car (gimp-image-new 256 256 RGB))) ; was gray
            (bg-layer (car (gimp-text-fontname img -1 0 0 text slabedge TRUE size PIXELS font)))
            (text-layer (car (gimp-text-fontname img -1 0 0 text slabedge TRUE size PIXELS font)))
            (width (car (gimp-drawable-width text-layer)))
            (height (car (gimp-drawable-height text-layer)))
            (white-layer (car (gimp-text-fontname img -1 0 0 text slabedge TRUE size PIXELS font)))
            (brd-layer
                (car (gimp-layer-new img width height
                    RGB-IMAGE "border" 100 LAYER-MODE-NORMAL)) ; was GrayA
            )
            
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            (grancol 0)
            (brdwidth)
            (brdheight)
        )
        
        (gimp-image-undo-disable img)
        (gimp-drawable-set-name text-layer "Text")
        (gimp-drawable-set-name white-layer "White")
        (gimp-image-lower-layer img white-layer)
        (gimp-drawable-set-name bg-layer "Bg")
        (gimp-drawable-set-name brd-layer "Brd layer")
        (gimp-image-resize img width height 0 0)
        
        ;    (set! amount (* height (/ 1 16)))
        
        ; =========================================================
        (gimp-image-set-active-layer img text-layer)
        (gimp-context-set-foreground '(0 0 0))
        ;(gimp-message "line 59")
        
        ; Add the border layer
        (gimp-image-insert-layer img brd-layer 0 2)
        ;(gimp-image-set-active-layer img brd-layer)
        ;(gimp-edit-clear brd-layer)
        
        ; do the plasma and make it gray
        (gimp-image-set-active-layer img bg-layer)
        (plug-in-plasma 1 img bg-layer 42 3.2)
        (gimp-drawable-brightness-contrast bg-layer -0.235 -0.431) ; was -60 -110
        
        ;(gimp-message "line 71")
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        ; Do the borders
        (gimp-image-set-active-layer img brd-layer)
        (gimp-selection-all img)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-bucket-fill brd-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 0 0 0)
        
        
        ;(gimp-message "line 76")
        (gimp-selection-all img)
        (set! brdwidth (car (gimp-drawable-width brd-layer)))
        (set! brdheight (car (gimp-drawable-height brd-layer)))
        ;(gimp-message (number->string brdwidth))
        
        ;(gimp-image-select-rectangle img CHANNEL-OP-REPLACE 20 20 (- brdwidth 20) (- brdheight 20))
        ;(gimp-selection-border img (* slabedge 2))
        (gimp-selection-shrink img (round (* slabedge 0.3)))
        
        
        
        (set! grancol (* graniteshade 10))
        ;(gimp-message (number->string grancol))
        (gimp-context-set-foreground (list grancol grancol grancol)); was 0 0 0
        ;(gimp-context-set-foreground '(80 80 80))
        
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        (gimp-edit-bucket-fill brd-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 0 0 0)
        
        
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit) ;for debugging return exit stop
        
        (gimp-selection-none img)
        
        ;(gimp-message "line 102")
        (gimp-drawable-set-visible bg-layer 0)
        (gimp-drawable-set-visible brd-layer TRUE) ; was 1
        (gimp-drawable-set-visible text-layer 0)
        (gimp-drawable-set-visible white-layer 0)
        
        ; OK, the borders are in order. Bump-map? Just say YES!
        (gimp-drawable-set-visible bg-layer TRUE)
        (gimp-image-set-active-layer img brd-layer)
        
        ;(gimp-message "line 123 - Not Crashing")
        ;(gimp-message "line 124")
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(gimp-message (number->string (* slabdepth 3)))
        ;(quit)
        
        (plug-in-bump-map 1 img brd-layer bg-layer
                135 45 (* slabdepth 3) 0 0 0.00 0.00 FALSE FALSE 1)   ; was slabdepth
        ; (gimp-drawable-delete brd-layer)
         
        
        
        ;(gimp-message "line 136")
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        
        ; Do the "Black text on white bg" trick
        
        (gimp-drawable-set-visible bg-layer 0)
        (gimp-drawable-set-visible brd-layer 0)
        (gimp-drawable-set-visible text-layer 1)
        (gimp-drawable-set-visible white-layer 1)
        
        (gimp-image-set-active-layer img white-layer)
        (gimp-selection-all img)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-bucket-fill white-layer BUCKET-FILL-FG LAYER-MODE-NORMAL 100 0 0 0 0)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-selection-none img)
        (set! text-layer
            (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))
        (plug-in-blur 1 img text-layer)
        
        (gimp-drawable-set-visible brd-layer TRUE)
        (gimp-image-set-active-layer img brd-layer)
        (gimp-image-lower-item img text-layer)
        
        ;(gimp-message "line 163")
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        ;(gimp-message "line 168")
        ;;(plug-in-bump-map 1 img bg-layer text-layer 135 45 textdepth 0 0 0 0 1
        ;;        (if (eq? carvetog FALSE) 1 0) GRADIENT-LINEAR)
        
        (plug-in-bump-map 1 img brd-layer text-layer 135 45 textdepth 0 0 0.0 0.0 FALSE
                (if (eq? carvetog FALSE) 1 0) 1)
        
        ;(gimp-message "line 175")
        ;(gimp-displays-flush)
        ;(gimp-display-new img)
        ;(quit)
        
        
        ;(gimp-message "line 181")
        (gimp-drawable-set-visible bg-layer 1)
        (gimp-drawable-set-visible text-layer 1)
        
        ; End===================================
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
        (gimp-image-undo-enable img)
        (gimp-display-new img)
    )
)

;
; Hajaa-ho!
;
(script-fu-register
    "script-fu-granite-slab"
    "<Image>/Script-Fu/Logos/Granite slab..."
    "Makes a granite slab with a text. Set border to lowest value to eliminate.\nfile:graniteslab_02.scm"
    "Urpo Lankinen <wwwwolf@iki.fi>"
    "Urpo Lankinen <wwwwolf@iki.fi>"
    "1998"
    ""
    SF-STRING "Text String"     "Granite"
    SF-FONT  "Font"             "Courier New Bold"
    SF-ADJUSTMENT   "Font Size (pixels)"    '(150 2 1000 1 10 0 1)
    SF-ADJUSTMENT   "Edge of the slab"      '(45 1 2000 1 5 0 1)
    SF-ADJUSTMENT   "Depth of the slab"     '(8 1 21 1 5 0 1)
    SF-ADJUSTMENT   "Depth of the text"     '(32 1 64 1 5 0 1)
    SF-TOGGLE       "Carve"                 TRUE
    SF-ADJUSTMENT "Granite shade (1 darkest to 25 lightest)"   '(8 1 25 1 5 0 1)
)

; end of script