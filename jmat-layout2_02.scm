; Scripts for combining images side-by-side or atop in given width.
; By James Waldby - j-waldby@pat7.com - 30 March 2003
; v0.1 - 04 April 2003
;
;GPLv3
;Modified by Karlhof26
; --------------------------------------------------------------------
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; version 0.2 Raymond Ostertag <r.ostertag@caramail.com>
;;;     - ported to Gimp 2.0
;
;script-fu-jmat-copy is a utility used by other routines to
;copy the active layer of an image into another image, at a
;given size and offset.  [Should check if image has active layer]
;
;script-fu-jmat-aspect is a utility used by other routines to
;compute the aspect ratio (width/height) of the active layer of 
;an image [Should check if image has active layer]
;
;script-fu-jmat2 combines 2 images side-by-side.  It scales
;the images to the same height and to a given total width
;without changing aspect ratio.
;
;script-fu-jmat3a combines 3 images side-by-side.  It scales
;the images to the same height and to a given total width
;without changing aspect ratio.  Equivalent result to jmat2 
;followed by jmat2 without extra scaling.
;
;script-fu-jmat11 combines 2 images one above the other.  
;It scales both images to the same width without changing 
;aspect ratio.
;
;script-fu-jmat111 combines 3 images in a vertical stack.  
;It scales all images to the same width without changing 
;aspect ratio.
;
;script-fu-jmat1b2 combines 3 images, one beside the other two.  
;Equivalent to jmat11 followed by jmat2.
;
;script-fu-jmat1a2 combines 3 images, one above the other two.  
;Equivalent to jmat2 followed by jmat11.
;
;script-fu-jmat2a2 combines 4 images, two above the other two.  
;Equivalent to 2 jmat2's followed by jmat11.
;
;script-fu-jmat1a3 combines 4 images, one above the other three.  
;Equivalent to jmat3 followed by jmat11.

;====================================================================
; jmat-copy copies scaled top layer of image imi to a new layer 
; in image imo.
; Parameters:  imi = image in, to be copied.
; imo = image out, to be copied to.
; wn, hn = width and height of new layer in imo.
; xoff = x offset of new layer position in imo,
; yoff = y offset of new layer position in imo,
(define (script-fu-jmat-copy imi imo wn hn xoff yoff)
    (let* (
            (layi (car (gimp-image-get-active-layer imi)))
            (layo (car (gimp-layer-new imo wn hn RGB-IMAGE "c" 100 0)))
          )
        (gimp-image-add-layer imo layo 0)   ; Add a layer to new image
        (gimp-selection-all imi)
        (gimp-edit-copy layi)
        (let (
                (t (car (gimp-edit-paste layo 0)))
             )
            (gimp-layer-set-offsets t 0 0)          ; Avoid an offset problem
            (gimp-layer-scale t wn hn 0)            ; Scale ma to fit in p*z
            (gimp-floating-sel-anchor t)
        )
        (gimp-layer-set-offsets layo xoff yoff)     ; Offset the layer
    )
) ; end script-fu-jmat-copy

;====================================================================
; jmat-aspect computes aspect ratio of active layer of image
; Parameters:  imi = image in, to be copied.
(define (script-fu-jmat-aspect imi)
    (let* (
            (L (car (gimp-image-get-active-layer imi)))
            (R (/ (car (gimp-drawable-width L)) (car (gimp-drawable-height L))))
          )
        R
    )
) ; end script-fu-jmat-aspect
;====================================================================
; Given 2 images, combine them into one row, w pixels wide.
(define (script-fu-jmat2 w ima imb)
    (let* (
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (h2 (/ w (+ ra rb)))
            (wa (* h2 ra))
            (wb (* h2 rb))
            (ni (car (gimp-image-new w h2 0))) ; make new RGB image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wa h2 0  0)     ; Copy ima to top left
        (script-fu-jmat-copy imb ni wb h2 wa 0)     ; Copy imb to top center
        (gimp-image-undo-enable ni)         ; Re-enable undo
        (gimp-display-new ni)               ; Show the new image
    )    ;end let
)    ;end script-fu-jmat2

;====================================================================
; Given 3 images, combine into one row, w pixels wide.
(define (script-fu-jmat3a w ima imb imc)
    (let* (
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (rc (script-fu-jmat-aspect imc))
            (h3 (/ w (+ ra rb rc)))
            (wa (* h3 ra))
            (wb (* h3 rb))
            (wc (* h3 rc))
            (ab (+ wa wb))
            (ni (car (gimp-image-new w h3 0)))  ; make new w*z RGB image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wa h3 0  0)     ; Copy ima to top left
        (script-fu-jmat-copy imb ni wb h3 wa 0)     ; Copy imb to top middle
        (script-fu-jmat-copy imc ni wc h3 ab 0)     ; Copy imc to top right
        (gimp-image-undo-enable ni)     ; Re-enable undo
        (gimp-display-new ni)           ; Show the new image
    )    ;end let
)    ;end script-fu-jmat3

;====================================================================
; Given images ima, imb; stack into one image w pixels wide.
(define (script-fu-jmat11 w ima imb)
    (let* (
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (ha  (/ w ra))
            (hb  (/ w rb))
            (ni (car (gimp-image-new w (+ ha hb) 0))) ; make new w*z RGB image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni w ha 0 0) ; Copy ima to top
        (script-fu-jmat-copy imb ni w hb 0 ha); Copy imb to bottom
        (gimp-image-undo-enable ni)		; Re-enable undo
        (gimp-display-new ni)		; Show the new image
    )    ;end let
)    ;end script-fu-jmat11

;====================================================================
; Given 3 images; stack vertically into one image w pixels wide.
(define (script-fu-jmat111 w ima imb imc)
    (let* (
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (rc (script-fu-jmat-aspect imc))
            (ha  (/ w ra))
            (hb  (/ w rb))
            (hc  (/ w rc))
            (ni (car (gimp-image-new w (+ ha hb hc) 0))) ; make new image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni w ha 0 0) ; Copy ima to top
        (script-fu-jmat-copy imb ni w hb 0 ha); Copy imb to middle
        (script-fu-jmat-copy imc ni w hc 0 (+ ha hb)); Copy imc to bottom
        (gimp-image-undo-enable ni)		; Re-enable undo
        (gimp-display-new ni)		; Show the new image
    )    ;end let
)    ;end script-fu-jmat111
;====================================================================
; Given 3 images; stack first one above 2-side-by-side
(define (script-fu-jmat1a2 w imf ima imb)
    (let* (
            (rf (script-fu-jmat-aspect imf))
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (h2 (/ w (+ ra rb)))
            (wa (* h2 ra))
            (wb (* h2 rb))
            (hf (/ w rf))
            (ni (car (gimp-image-new w (+ hf h2) 0)))   ; make new image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wa h2  0 hf)    ; Copy ima to lower left
        (script-fu-jmat-copy imb ni wb h2 wa hf)    ; Copy imb to low right
        (script-fu-jmat-copy imf ni w  hf  0 0)     ; Copy imf to top
        (gimp-image-undo-enable ni)     ; Re-enable undo
        (gimp-display-new ni)           ; Show the new image
    )    ;end let
)    ;end script-fu-jmat1a2
;====================================================================
; Given 4 images; stack in 2 rows of 2
(define (script-fu-jmat2a2 w ima imb imc imd)
    (let* (
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (rc (script-fu-jmat-aspect imc))
            (rd (script-fu-jmat-aspect imd))
            (h2 (/ w (+ ra rb)))
            (wa (* h2 ra))
            (wb (* h2 rb))
            (k2 (/ w (+ rc rd)))
            (wc (* k2 rc))
            (wd (* k2 rd))
            (ni (car (gimp-image-new w (+ h2 k2) 0)))   ; make new image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wa h2  0 0)
        (script-fu-jmat-copy imb ni wb h2 wa 0)
        (script-fu-jmat-copy imc ni wc k2  0 h2)
        (script-fu-jmat-copy imd ni wd k2 wc h2)
        (gimp-image-undo-enable ni)     ; Re-enable undo
        (gimp-display-new ni)           ; Show the new image
    )    ;end let
)    ;end script-fu-jmat2a2
;====================================================================
; Given 4 images; stack first one vertically above 3-side-by-side
(define (script-fu-jmat1a3 w imf ima imb imc)
    (let* (
            (rf (script-fu-jmat-aspect imf))
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (rc (script-fu-jmat-aspect imc))
            (h3 (/ w (+ ra rb rc)))
            (wa (* h3 ra))
            (wb (* h3 rb))
            (wc (* h3 rc))
            (ab  (+ wa wb))
            (hf (/ w rf))
            (ni (car (gimp-image-new w (+ hf h3) 0))) ; make new image
          )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wa h3  0 hf) ; Copy ima to low left
        (script-fu-jmat-copy imb ni wb h3 wa hf) ; Copy imb to low middle
        (script-fu-jmat-copy imc ni wc h3 ab hf) ; Copy imb to low right
        (script-fu-jmat-copy imf ni w  hf 0  0) ; Copy imf to top
        (gimp-image-undo-enable ni)		; Re-enable undo
        (gimp-display-new ni)		; Show the new image
    )    ;end let
)    ;end script-fu-jmat1a3
;====================================================================
; Given 3 images; place first one left of stack-of-2.
; Let rf = xf/yf = Aspect Ratio for image f.  
; Let rab = Aspect Ratio for stack of images a and b,
; and rfab = Aspect Ratio for image f next to a,b stack,
; We find rab = (ra*rb)/(ra+rb) and rfab = rf + rab.
(define (script-fu-jmat1b2 w imf ima imb)
    (let* (
            (rf (script-fu-jmat-aspect imf))
            (ra (script-fu-jmat-aspect ima))
            (rb (script-fu-jmat-aspect imb))
            (rab (/ (* ra rb) (+ ra rb)))	; rab = aspect ratio of 2-stack
            (rfab (+ rf rab))		; aspect ratio of whole new image
            (h   (/ w rfab))		; h = new image height
            (wf  (* h rf))			; wf = width of left image
            (wr  (* h rab))		; wr = width of right stack
            (ha  (/ wr ra))		; ha = height of top right
            (hb  (/ wr rb))		; hb = height of low right
            (ni (car (gimp-image-new w h 0))) ; make new image
         )
        (gimp-image-undo-disable ni)
        (script-fu-jmat-copy ima ni wr ha wf 0) ; Copy ima to upper right
        (script-fu-jmat-copy imb ni wr hb wf ha) ; Copy imb to low right
        (script-fu-jmat-copy imf ni wf h  0  0) ; Copy imf to left
        (gimp-image-undo-enable ni)		; Re-enable undo
        (gimp-display-new ni)		; Show the new image
    )    ;end let
)    ;end script-fu-jmat1b2
;====================================================================
(script-fu-register "script-fu-jmat2"
    "<Toolbox>/Script-Fu3/Misc/1 Join 2 pix in a row..." ;script location in gimp menu
    "Put two pictures side-by-side in a new picture of specified width.  Scale them to same height without changing their aspect ratios.  The images should be open when you call matt2. \nfile:jmat-layout2_02.scm" ;help
    "James I. Waldby"               ;author
    "2003 James I. Waldby"          ;copyright
    "31 March 2003"                 ;date
    ""
    SF-ADJUSTMENT   "Result Width"      '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Left Image"        0
    SF-IMAGE        "Right Image"       1
)
  
;====================================================================
(script-fu-register "script-fu-jmat3a"
    "<Toolbox>/Script-Fu3/Misc/2 Join 3 pix in a row..."  ;script location in gimp menu
    "Put three currently-open pictures side-by-side in a new picture of specified width.  Scale them to same height without changing their aspect ratios. \nfile:jmat-layout2_02.scm"   ;help
    "James I. Waldby"               ;author
    "2003 James I. Waldby"          ;copyright
    "02 April 2003"                 ;date
    ""
    SF-ADJUSTMENT   "Result Width"      '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Left Image"        0
    SF-IMAGE        "Middle Image"      1
    SF-IMAGE        "Right Image"       2
)

;====================================================================
(script-fu-register "script-fu-jmat11"
    "<Toolbox>/Script-Fu3/Misc/3 Join 2 pix in a column..." ;script location in gimp menu
    "Put 2 currently-open pictures in a vertical stack in a new picture of specified width.  Scale them without changing their aspect ratios. \nfile:jmat-layout2_02.scm"  ;help
    "James I. Waldby"               ;author
    "2003 James I. Waldby"          ;copyright
    "02 April 2003"                 ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Top Image"             0
    SF-IMAGE        "Bottom Image"          1
)
;====================================================================
(script-fu-register "script-fu-jmat111"
    "<Toolbox>/Script-Fu3/Misc/4 Join 3 pix in a column..." ;script location in gimp menu
    "Put 3 currently-open pictures in a vertical stack in a new picture of specified width.  Scale them without changing their aspect ratios. \nfile:jmat-layout2_02.scm" ;help
    "James I. Waldby"               ;author
    "2003 James I. Waldby"          ;copyright
    "02 April 2003"                 ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Top Image"             0
    SF-IMAGE        "Middle Image"          1
    SF-IMAGE        "Bottom Image"          2
)
;====================================================================
(script-fu-register "script-fu-jmat1a2"
    "<Toolbox>/Script-Fu3/Misc/5 Join 3 pix, 1 above row of 2..." ;script location in gimp menu
    "Arrange 3 currently-open pictures (scaled without changing aspect ratios) into a new picture of specified width.  Stack 1 picture above 2-side-by-side. \nfile:jmat-layout2_02.scm" ;help
    "James I. Waldby"               ;author
    "2003 James I. Waldby"          ;copyright
    "02 April 2003"                 ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Top Image"             0
    SF-IMAGE        "Bottom Left Image"     1
    SF-IMAGE        "Bottom Right Image"    2
  )
;====================================================================
(script-fu-register "script-fu-jmat2a2"
    "<Toolbox>/Script-Fu3/Misc/6 Join 4 pix in 2 rows of 2..." ;script location in gimp menu
    "Arrange 4 currently-open pictures (scaled without changing aspect ratios) into a new picture of specified width.  Stack 2 rows of 2 pictures each. \nfile:jmat-layout2_02.scm" ;help
    "James I. Waldby"            ;author
    "2003 James I. Waldby"       ;copyright
    "02 April 2003"              ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Top Left Image"        0
    SF-IMAGE        "Top Right Image"       1
    SF-IMAGE        "Bottom Left Image"     2
    SF-IMAGE        "Bottom Right Image"    3
)
;====================================================================
(script-fu-register "script-fu-jmat1a3"
    "<Toolbox>/Script-Fu3/Misc/7 Join 4 pix, 1 above 3..." ;script location in gimp menu
    "Arrange 4 currently-open pictures (scaled without changing aspect ratios) into a new picture of specified width.  Stack 1 picture above 3-side-by-side. \nfile:jmat-layout2_02.scm" ;help
    "James I. Waldby"           ;author
    "2003 James I. Waldby"      ;copyright
    "02 April 2003"             ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Top Image"             0
    SF-IMAGE        "Bottom Left Image"     1
    SF-IMAGE        "Bottom Middle Image"   2
    SF-IMAGE        "Bottom Right Image"    3
)
;====================================================================
(script-fu-register "script-fu-jmat1b2"
    "<Toolbox>/Script-Fu3/Misc/8 Join 3 pix, 1 beside column of 2..." ;script location in gimp menu
    "Arrange active layers of 3 currently-open pictures (scaled without changing aspect ratios) into a new picture of specified width.  Put 1 picture beside stack-of-2. \nfile:jmat-layout2_02.scm"  ;help
    "James I. Waldby"           ;author
    "2003 James I. Waldby"      ;copyright
    "03 April 2003"             ;date
    ""
    SF-ADJUSTMENT   "Result Width"          '(600 10 8000 10 10 0 1)
    SF-IMAGE        "Left Image"            0
    SF-IMAGE        "Top Right Image"       1
    SF-IMAGE        "Bottom Right Image"    2
)
;====================================================================

;   SF-IMAGE	 "Middle Right Image" 2
