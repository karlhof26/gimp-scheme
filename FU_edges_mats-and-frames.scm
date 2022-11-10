; FU_edges_mats-and-frames.scm
; version 2.8 [gimphelp.org]
; last modified/tested by Paul Sherman
; 02/14/2014 on GIMP-2.8.10
; 03/09/2020 on GIMP-2.10.20 
;
; 02/14/2014 - convert to RGB if needed
;==============================================================
;
; Installation:
; This script should be placed in the user or system-wide script folder.
;   
;   Windows 10 & 11)
;   C:\Program Files\GIMP 2\share\gimp\2.0\scripts
;   or
;   C:\Users\YOUR-NAME\.gimp-2.8\scripts
;   C:\Users\YOUR-NAME\.gimp-2.10\scripts
;   C:\Users\YOUR-NAME\AppData\Roaming\GIMP\2.10\scripts
;   
;   
;   Linux
;   /home/yourname/.gimp-2.8/scripts  
;   or
;   Linux system-wide
;   /usr/share/gimp/2.0/scripts
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
;
; Copyright (C) 2003 Eric Jeschke (eric@redskiesatnight.com)
; Based on code by
; Copyright (C) 1997 Andy Thomas (alt@picnic.demon.co.uk)
; and
; Copyright (C) 1997 Andrew Donkin  (ard@cs.waikato.ac.nz)
;
;==============================================================

; gimp-scheme should add this...it's a scheme "standard"
(define (cadddr l) (caddr (cdr l)))

; ditto
; non-functional mapper (discards values, used for effect).
(define (for-each proc l)
  (if (not (null? l))
      (begin
        (proc (car l))
        (for-each proc (cdr l))
      )
  )
)

; Create an array from a list.
(define (list-to-array contents)
  (let* (   (alen (length contents))
            (n_array (cons-array alen 'double))
            (count 0)
        )
    (for-each (lambda (val)
                (aset n_array count val)
                (set! count (+ count 1)))
              contents
    )
    n_array
  )
)

; Takes a color (represented as a list of numbers: (R G B)) and a delta
; value and returns four new colors representing the left, top, right and
; bottom shades.  Used for coloring the mat bevel.
;
; returns (LC TC RC BC)
(define (getcolors color delta)
  (letrec ( (deltacolor (lambda (v d) (max (min (+ v d) 255) 0)))
            (adjcolor (lambda (cl d)
                       (mapcar (lambda (v) (deltacolor v d)) cl)))
          )
    (list
        (adjcolor color (- 0 (/ delta 2)))   ; left color
        (adjcolor color (- 0 delta))         ; top color
        (adjcolor color (/ delta 2))         ; right color
        (adjcolor color delta)               ; bottom color
     )
  )
)

; utility routine to decipher color choice option and return the
; appropriate fill: from dialog color picker, current fg, or current bg.
(define (get-color fillchoice colorchoice)
    (cond
        ((= fillchoice 0) colorchoice)
        ((= fillchoice 1) (car (gimp-context-get-foreground)))
        (TRUE (car (gimp-context-get-background))) 
    )
)

; Draw a border around an image.
;
; This is the workhorse function.  Draws a constant width border around
; an image with optional padding on each of the 4 sides.  If 'delta' > 0
; then each side will be colored so that it resembles a bevel (unless
; the fillchoice is a pattern).
(define (draw-border-matte drawable
            borderwidth
            lpad tpad rpad bpad   ; # pixels to pad on L, T, R, B
            fillchoice            ; type of fill
            fillcolor delta       ; fill parameters
            bumppattern           ; "texture" pattern
            leavebumpmap          ; flag: TRUE-->preserve bumpmap
            ibumpp                ; flag: TRUE-->bump interactively
            layername             ; border is drawn on a new layer
            leaveselectionp       ; if true, border becomes selection
        )
    ;(gimp-message "line 130")
    (let* (
            (img (car (gimp-item-get-image drawable)))
            (imgtype (car (gimp-drawable-type-with-alpha drawable)))
            (owidth (car (gimp-image-width img)))      ; current image w & h
            (oheight (car (gimp-image-height img)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            ; calculate new dimensions
            (width (+ owidth (* 2 borderwidth) lpad rpad))
            (height (+ oheight (* 2 borderwidth) tpad bpad))
            ; calculate coordinates for filling
            (ulimage_x (+ borderwidth lpad))
            (ulimage_y (+ borderwidth tpad))
            (llimage_x ulimage_x)
            (llimage_y (+ ulimage_y oheight))
            (lrimage_x (+ llimage_x owidth))
            (lrimage_y llimage_y)
            (urimage_x lrimage_x)
            (urimage_y ulimage_y)
            (ulborder_x lpad)
            (ulborder_y tpad)
            (llborder_x ulborder_x)
            (llborder_y (+ llimage_y borderwidth))
            (lrborder_x (+ lrimage_x borderwidth))
            (lrborder_y (+ lrimage_y borderwidth))
            (urborder_x lrborder_x)
            (urborder_y ulborder_y)
            ; get colors for each side (returns LC TC RC BC)
            (filltype BUCKET-FILL-BG)
            (color (get-color fillchoice fillcolor))
            (colors (getcolors color delta))
            ; create a new layer above current one
            (layer (car (gimp-layer-new img width height imgtype layername
                                     100 LAYER-MODE-NORMAL)))
            (ibumpflag (if (= ibumpp TRUE) 0 1))
        )
        
        ;(gimp-message "line 166")
        ; fill with transparent pixels and resize to new dimensions
        (gimp-drawable-fill layer FILL-TRANSPARENT)
        (gimp-image-resize img width height ulimage_x ulimage_y)
        
        ; add the layer
        (gimp-image-insert-layer img layer 0 0)
        
        ; set up for pattern fill if that was requested
        (if (> fillchoice 2)
            (begin
                ;(gimp-message "Pattern filling")
                ; uses the current pattern
                (set! filltype BUCKET-FILL-PATTERN)
            )
        )
        
        
        ; color the left polygon
        (gimp-context-set-background (car colors))
        (gimp-image-select-polygon img CHANNEL-OP-REPLACE
              10
              (list-to-array (list ulborder_x ulborder_y ulimage_x ulimage_y
                               llimage_x llimage_y llborder_x llborder_y
                               ulborder_x ulborder_y)))
        ;(gimp-message "line193")
        
        (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        
        ; color the left pad
        (if (> lpad 0)
            (begin
                ;(gimp-message "left pad")
                (gimp-context-set-background color)
                (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
                    (list-to-array (list 0 ulborder_y ulborder_x ulborder_y llborder_x
                        llborder_y 0 llborder_y 0 ulborder_y)))
                
                (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
            )
        )
        
        
        
        ; color the top polygon
        (gimp-context-set-background (cadr colors))
        (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
            (list-to-array (list ulborder_x ulborder_y ulimage_x ulimage_y
                               urimage_x urimage_y urborder_x urborder_y
                               ulborder_x ulborder_y)))
        
        (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        
        ; color the top pad
        (if (> tpad 0)
            (begin
                ;(gimp-message "top-pad")
                (gimp-context-set-background color)
                (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
                    (list-to-array (list 0 0 0 ulborder_y width urborder_y width 0 0 0)))
                
                (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
            )
        )
        
        
        
        ; color the right polygon
        (gimp-context-set-background (caddr colors))
        (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
            (list-to-array (list urborder_x urborder_y urimage_x urimage_y
                               lrimage_x lrimage_y lrborder_x lrborder_y
                               urborder_x urborder_y)))
        
        (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        ;(gimp-message "line 246")
        
        
        
        ; color the right pad
        (if (> rpad 0)
            (begin
                (gimp-context-set-background color)
                (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
                    (list-to-array (list width urborder_y urborder_x urborder_y
                               lrborder_x lrborder_y width lrborder_y
                               width urborder_y)))
                
                (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
            )
        )
        
        
        
        ; color the bottom polygon
        (gimp-context-set-background (cadddr colors))
        (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
            (list-to-array (list llborder_x llborder_y llimage_x llimage_y
                               lrimage_x lrimage_y lrborder_x lrborder_y
                               llborder_x llborder_y)))
        
        
        (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        ; color the bottom pad
        (if (> bpad 0)
            (begin
                ;(gimp-message "line 269 - bottom pad")
                (gimp-context-set-background color)
                (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
                    (list-to-array (list 0 height 0 llborder_y width lrborder_y
                               width height 0 height)))
                
                
                (gimp-edit-bucket-fill layer filltype LAYER-MODE-NORMAL 100 0 FALSE 0 0)
            )
        )
        
        
        
        ; if user wanted to texture the border, do a bump map
        (if (not (null? bumppattern))
            (begin
                (let* (     (old-pattern (car (gimp-context-get-pattern)))
                            (bumpmap (car (gimp-layer-new img width height imgtype
                                             "Texture Bump Map" 100 LAYER-MODE-NORMAL)))
                      )
                    ;(gimp-message "bumpmapping")
                    (gimp-image-insert-layer img bumpmap 0 -1)
                    (gimp-context-set-pattern bumppattern)
                    ; NOTE: if we try to do a bump map on only the selection we get
                    ; weird artifacts around the edges; as a kludge, we turn on
                    ; "preserve transparency" and bump map the entire thing.  It might
                    ; also be possible to just grow the selection, but I haven't tried
                    ; that yet.
                    (gimp-layer-set-preserve-trans layer 1)
                    (gimp-selection-all img)
                    
                    (gimp-edit-bucket-fill bumpmap BUCKET-FILL-PATTERN LAYER-MODE-NORMAL 100
                            0 FALSE 0 0)
                    (gimp-context-set-pattern old-pattern)
                    (plug-in-bump-map ibumpflag img layer bumpmap 125 45 5 0 0 0 0 ; was 125 45 3 0 0 0 0
                            TRUE FALSE 1)
                    (if (= leavebumpmap TRUE)
                        (gimp-item-set-visible bumpmap 0)
                        (gimp-image-remove-layer img bumpmap)
                    )
                    (gimp-layer-set-preserve-trans layer 0)
                )
            )
        )
        
        ;debug
        ;(gimp-message "Quit KH2")
        ;(gimp-displays-flush)
        ;(quit)
        
        ;(gimp-message "line 328")
        (gimp-displays-flush)
        ; clear selection
        (gimp-selection-none img)
        
        ; set selection to border if desired
        (if (= leaveselectionp TRUE)
            (begin
                (gimp-image-select-polygon img CHANNEL-OP-REPLACE 10
                    (list-to-array (list ulimage_x ulimage_y urimage_x urimage_y
                               lrimage_x lrimage_y llimage_x llimage_y
                               ulimage_x ulimage_y)))
                
                (gimp-selection-invert img)
            )
        )
        
        ; clean-up
        (gimp-context-set-background old-bg)
        
        ; return value
        (list layer)
    );end let*
)

; Script to add a mat around an image.
;
(define (FU-add-mat image drawable
            bevelwidth           ; mat bevel width in pixels
            beveledge            ; outer edge of the bevel
            bevelfillchoice      ; {0 (Choose) | 1 (FG color) | 2 (BG color)}
            bevelcolorchoice     ; (Choose) color from color selection dialog
            delta                ; amount to vary fill to create bevel effect
            matwidth             ; no comment here asks karlhof82
            matfillchoice        ; as above, but for the mat
            matcolorchoice3      ; karlhof26 now matcolorchoice3
            texturep             ; flag: TRUE-->bump map the mat
            mattexture           ; pattern for bump mapping mat
            leavebumpmapp        ; flag: TRUE-->preserve bump map
            ibumpp               ; flag: TRUE-->bump interactively
            lpad                 ; amount to pad the mat on the left
            tpad                 ; top
            rpad                 ; right
            bpad                 ; bottom
            layersp              ; flag: TRUE-->preserve new layers
            leaveselectionp      ; flag: TRUE-->mat is selected on exit
        )
        ;(gimp-message "line 375")
        (gimp-image-undo-group-start image)
        (if (not (= RGB (car (gimp-image-base-type image))))
            (gimp-image-convert-rgb image)
        )
        
    (let* (
            (img (car (gimp-item-get-image drawable)))
            (pattern (if (= texturep TRUE) mattexture '()))
        )
        
        ;(gimp-message "line 386")
        ; draw bevel
        (if (> bevelwidth 0)
            (begin
                ;(gimp-message "line 390 - Bevelling")
                ;(gimp-message "line 391")
                ;;(draw-border-matte drawable bevelwidth 0 0 0 0 bevelfillchoice
                ;;    bevelcolorchoice delta mattexture FALSE FALSE "Mat Bevel" FALSE)
                ;(gimp-message "line 394")
                
                (draw-border-matte drawable bevelwidth beveledge beveledge beveledge beveledge bevelfillchoice     ; was 0 0 0 0 instead of beveledgex4
                    bevelcolorchoice delta pattern FALSE FALSE "Mat Bevel" FALSE)
                ;(gimp-message "line398")
                
                ;;boken- empty list;;(draw-border-matte drawable bevelwidth 0 0 0 0 bevelfillchoice
                ;;    bevelcolorchoice delta '() FALSE FALSE "Mat Bevel" FALSE)
            )
        )
        
        
        ; draw mat
        (if (> matwidth 0)
            (begin
                ;(gimp-message "line 409")
                (draw-border-matte drawable matwidth lpad tpad rpad bpad
                    matfillchoice matcolorchoice3 0 pattern leavebumpmapp
                    ibumpp "Mat" leaveselectionp)
            )
        )
        ;(gimp-message "line 415")
        ; merge layers if user did not want them
        
        (gimp-displays-flush)
        ;(gimp-message "Quit Now")
        ;(quit)
        
        (if (= layersp FALSE)
            (begin
                (if (> matwidth 0)
                    (begin
                        (let* (
                            (layer (car (gimp-image-get-active-layer img)))
                            )
                            (gimp-image-merge-down img layer EXPAND-AS-NECESSARY)
                        )
                    )
                )
                (if (> bevelwidth 0)
                    (begin
                        (let* (
                            (layer (car (gimp-image-get-active-layer img)))
                            )
                            (gimp-image-merge-down img layer EXPAND-AS-NECESSARY)
                        )
                    )
                )
            )
        )
        
        ;(gimp-message "Good finish OK")
        ; th-th-th-that's all folks!
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
        (gc) ; garbage collection; memory cleanup
    )
)


; Script to add a frame around an image.  
;
(define (FU-add-frame-kh image drawable
            framewidth
            framefillchoice      ; as above, but for the mat
            framecolorchoice
            texturep             ; flag: TRUE-->bump map the mat
            frametexture         ; pattern for bump mapping mat
            leavebumpmapp        ; flag: TRUE-->preserve texture bump map
            ibumpp               ; flag: TRUE-->bump interactively
            bevelindex           ; "depth" of 3D effect
            ds-width             ; inner shadow width
            ds-opacity           ; inner shadow opacity
            layersp              ; flag: TRUE-->preserve new layers
            leaveselectionp      ; flag: TRUE-->mat is selected on exit
            leavebevelbumpmapp   ; flag: TRUE-->preserve bevel bump map
        )
        ;(gimp-message "line 467")
        (gimp-image-undo-group-start image)
        (if (not (= RGB (car (gimp-image-base-type image))))
            (gimp-image-convert-rgb image)
        )
        
  (let* (
         (img (car (gimp-item-get-image drawable)))
         (imgtype (car (gimp-drawable-type-with-alpha drawable)))
         (pattern (if (= texturep TRUE) frametexture '()))
         )
    
    ; parameter sanity checks
    (if (<= framewidth 0)
        (set! framewidth 10)
    )
    (if (> bevelindex (/ framewidth 2))
        (set! bevelindex (/ framewidth 2))
    )
    
    ; draw border
    (let* (
            (frame (car (draw-border-matte drawable framewidth 0 0 0 0
                                    framefillchoice framecolorchoice 0
                                    pattern leavebumpmapp ibumpp "Frame"
                                    TRUE)))
            (selection (car (gimp-selection-save img)))
            (width (car (gimp-image-width img)))      ; current image w & h
            (height (car (gimp-image-height img)))
            (old-bg (car (gimp-context-get-background)))
            (bumpmap (car (gimp-layer-new img width height imgtype
                                         "Frame Bevel Bump Map" 100 LAYER-MODE-NORMAL)))
            (index 1)
          )
        ;(gimp-message "line 501")
        ; Initialise our bumpmap
        (gimp-image-insert-layer img bumpmap 0 -1)
        (gimp-item-set-visible bumpmap FALSE)
        (gimp-context-set-background '(0 0 0))
        (gimp-drawable-fill bumpmap FILL-BACKGROUND)
        (gimp-image-select-item img CHANNEL-OP-REPLACE selection)
        
        ; Fill with a gradient of sorts
        ; TODO: there has got to be a more efficient way to do this
        ; (gradient fills?)
        ;(gimp-message "line 512")
        (set! index 0)
        (while (< index bevelindex)
            (let ((gv (/ (* index 255) bevelindex)))
                (begin
                    ;(gimp-message (string-append (number->string gv) ":" (number->string index)))
                    (gimp-context-set-background (list (round gv) (round gv) (round gv)))
                    (gimp-edit-bucket-fill bumpmap FILL-BACKGROUND LAYER-MODE-NORMAL 100 0
                                   FALSE 0 0)
                    (gimp-selection-shrink img 1)
                    (set! index (+ index 1))
                )
            )
        )
        
        ; Now the white interior of the bumpmap
        (gimp-context-set-background '(255 255 255))
        (gimp-edit-bucket-fill bumpmap FILL-BACKGROUND LAYER-MODE-NORMAL 100 0 FALSE 0 0)
        
        ; Do the bump map.
        (gimp-selection-none img)
        (gimp-displays-flush)
        (plug-in-bump-map 1 img frame bumpmap 125 45 bevelindex 0 0 0 0 TRUE FALSE 1) ; bump value was 3 now 5
        
        ;(gimp-message "line 536")
        ;(gimp-layer-set-mode bumpmap LAYER-MODE-ADDITION)
        (gimp-displays-flush)
        
        ; remove bump map if user did not want it
        (if (= leavebevelbumpmapp FALSE)
            (gimp-image-remove-layer img bumpmap)
        )
        
        ; Now for the inner shadow
        (if (> ds-width 0)
            (begin
                (let* (
                        (ds-color '(0 0 0))
                        ; create the shadow layer
                        (shadow (car (gimp-layer-new img width height imgtype
                                              "Shadow" ds-opacity LAYER-MODE-NORMAL)))
                      )
                    ;(gimp-message "line 554")
                    (gimp-image-insert-layer img shadow 0 0)
                    (gimp-selection-none img)
                    (gimp-edit-clear shadow)
                    (gimp-context-set-background ds-color)
                    (gimp-image-select-item img CHANNEL-OP-REPLACE selection)
                    (gimp-selection-feather img ds-width)
                    (gimp-edit-fill shadow FILL-BACKGROUND)
                    (gimp-image-select-item img CHANNEL-OP-REPLACE selection)
                    (gimp-edit-clear shadow)
                )
            )
        )
        
        (gimp-displays-flush)
        ; merge layers if user did not want them
        (if (= layersp FALSE)
            (begin
                ;(gimp-image-set-active-layer img frame)
                ;(gimp-image-merge-down img frame EXPAND-AS-NECESSARY)
                (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)
            )
        )
        
        ; leave the selection if the user requested it
        (if (= leaveselectionp TRUE)
            (gimp-image-select-item img CHANNEL-OP-REPLACE selection)
            (gimp-selection-none img)
        )
        
        ; clean up
        (gimp-image-remove-channel img selection)
        (gimp-context-set-background old-bg)
        
    );end let*
    
    ;(gimp-message "Good finish OK")
    ; th-th-th-that's all folks!
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gc) ; garbage collect
  )
)

; Obligatory script registrations...

(script-fu-register "FU-add-mat"
            "<Toolbox>/Script-Fu/Edges/Add Matte Frame"
            "Add a single mat around an image. Matte overwrites the bevel. \nfile:FU_edges_mats-and-frames.scm"
            "Eric Jeschke <eric@redskiesatnight.com>"
            "Eric Jeschke"
            "5/27/03"
            "*"
            SF-IMAGE        "Input Image"                   0
            SF-DRAWABLE     "Input Drawable"                0
            SF-ADJUSTMENT   "Bevel Width 0=no bevel"        '(50 0 250 1 10 0 1)
            SF-ADJUSTMENT   "Bevel Edge"                    '(10 0 250 1 10 0 1)
            SF-OPTION       "Bevel Fill"                    '("Color" "FG color" "BG color" "ActivePattern")
            SF-COLOR        "Bevel Fill Color"              '(221 221 221)
            SF-ADJUSTMENT   "Delta Value on Bevel Color"    '(55 1 255 1 10 0 1)
            SF-ADJUSTMENT   "Mat Width 0=no matte"          '(80 0 1000 1 10 0 1)
            SF-OPTION       "Mat Fill"                      '("Color" "FG color" "BG color" "ActivePattern")
            SF-COLOR        "Mat Fill Color"                '(128 128 128)
            SF-TOGGLE       "Texture Mat/Bevel"             FALSE
            SF-PATTERN      "Texture/Bump Pattern"          "Wood"
            SF-TOGGLE       "Leave Texture Bump Map"        FALSE
            SF-TOGGLE       "Bump Interactively"            FALSE
            SF-ADJUSTMENT   "Left Pad"                      '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT   "Top Pad"                       '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT   "Right Pad"                     '(0 0 1000 1 10 0 1)
            SF-ADJUSTMENT   "Bottom Pad"                    '(0 0 1000 1 10 0 1)
            SF-TOGGLE       "Use Layers"                    TRUE
            SF-TOGGLE       "Leave Selection"               FALSE
)

(script-fu-register "FU-add-frame-kh"
            "<Toolbox>/Script-Fu/Edges/Add Frame with Bevel"
            "Add a frame around an image, ability to adjust bevel and shadow. Uses current pattern. \nfile:FU_edges_mats-and-frames.scm"
            "Eric Jeschke <eric@redskiesatnight.com>"
            "Eric Jeschke"
            "5/27/03"
            "*"
            SF-IMAGE        "Input Image"                   0
            SF-DRAWABLE     "Input Drawable"                0
            SF-ADJUSTMENT   "Frame Width"                   '(35 0 1000 1 10 0 1)
            SF-OPTION       "Frame Fill"                    '("Color" "FG color" "BG color" "Pattern")
            SF-COLOR        "Frame Fill Color"              '(128 128 128)
            SF-TOGGLE       "Texture Frame"                 FALSE
            SF-PATTERN      "Texture Pattern"               "Wood"
            SF-TOGGLE       "Leave Texture Bump Map"        FALSE
            SF-TOGGLE       "Bump Interactively"            FALSE
            SF-ADJUSTMENT   "Beveling Index"                '(10 0 250 1 10 0 1)
            SF-ADJUSTMENT   "Inner Shadow Width"            '(8 0 100 1 10 0 1)
            SF-ADJUSTMENT   "Inner Shadow Opacity"          '(50 0 100 1 10 0 1)
            SF-TOGGLE       "Use Layers"                    FALSE
            SF-TOGGLE       "Leave Selection"               FALSE
            SF-TOGGLE       "Leave Bevel Bump Map"          FALSE
)

;end of script