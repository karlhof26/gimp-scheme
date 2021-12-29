;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; GIMP - The GNU Image Manipulation Program                               ;;;
;;; Copyright (C) 1995 Spencer Kimball and Peter Mattis                     ;;;
;;;                                                                         ;;;
;;; This program is free software: you can redistribute it and/or modify    ;;;
;;; it under the terms of the GNU General Public License as published by    ;;;
;;; the Free Software Foundation, either version 3 of the License, or       ;;;
;;; (at your option) any later version.                                     ;;;
;;;                                                                         ;;;
;;; This program is distributed in the hope that it will be useful,         ;;;
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of          ;;;
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ;;;
;;; GNU General Public License for more details.                            ;;;
;;;                                                                         ;;;
;;; You should have received a copy of the GNU General Public License       ;;;
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.   ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; Multi-Replicate                                                         ;;;
;;;                                                                         ;;;
;;; multi-replicate.scm - version 1.07                                      ;;;
;;; Copyright (C) 2010-2013 Gino D                                          ;;;
;;; https://sites.google.com/site/ginodonig/gimp-scripts                    ;;;
;;;                                                                         ;;;
;;; This script is a powerful tool designed to duplicate layers and         ;;;
;;; channels in advanced mode.                                              ;;;
;;;                                                                         ;;;
;;; ....................................................................... ;;;
;;;                                                                         ;;;
;;; VERSION HISTORY                                                         ;;;
;;;                                                                         ;;;
;;; 1.00 - January 2010                                                     ;;;
;;;  * First release (as "Drawable Multiplication").                        ;;;
;;;                                                                         ;;;
;;; 1.01 - January 2010                                                     ;;;
;;;  * Fixed a problem of alignment occurring with non-zero offset values   ;;;
;;;    when the current drawable is a linked layer.                         ;;;
;;;                                                                         ;;;
;;; 1.02 - July 2010                                                        ;;;
;;;  * Now the script can offset the duplicates even when the original      ;;;
;;;    drawable is a channel.                                               ;;;
;;;  * Forced the script to be inactive if there is a floating selection    ;;;
;;;    inside the image.                                                    ;;;
;;;  * Reshaped and cleaned up the code.                                    ;;;
;;;                                                                         ;;;
;;; 1.03 - January 2011                                                     ;;;
;;;  * Now it is possible to express the offsets even as a percentage with  ;;;
;;;    respect to the image extents.                                        ;;;
;;;  * Added the ability to merge the duplicates of a layer into a single   ;;;
;;;    layer.                                                               ;;;
;;;  * Added the ability to autocrop the duplicates.                        ;;;
;;;  * Improved the method of duplication.                                  ;;;
;;;  * Reshaped and cleaned up the code.                                    ;;;
;;;                                                                         ;;;
;;; 1.04 - August 2011                                                      ;;;
;;;  * Now you can also access the script through the Layer local pop-menu  ;;;
;;;    and the Channel context menu, by right-clicking on the thumbnail of  ;;;
;;;    the specified layer or channel in the relevant dialog.               ;;;
;;;  * Fixed a small flaw in the display of the progress bar.               ;;;
;;;  * Corrected the year range in my copyright notice.                     ;;;
;;;  * Other small improvements and some code cleanups were made.           ;;;
;;;                                                                         ;;;
;;; 1.05 - December 2012                                                    ;;;
;;;  * Renamed the script from "Drawable Multiplication" to                 ;;;
;;;    "Multi-Replicate".                                                   ;;;
;;;  * Made the script fully compatible with GIMP 2.8, while maintaining    ;;;
;;;    the backward compatibility with the previous versions beginning from ;;;
;;;    GIMP 2.6.10.                                                         ;;;
;;;  * Added the abilities to scale and rotate the duplicates, with the     ;;;
;;;    possibility of selecting the interpolation method.                   ;;;
;;;  * Added the option to order the duplicates in the stack either from    ;;;
;;;    bottom to top or from top to bottom.                                 ;;;
;;;  * Added the option to place the duplicates of a layer into a new layer ;;;
;;;    group.                                                               ;;;
;;;  * Now the vector returned by the script contains only the identifiers  ;;;
;;;    of the generated duplicates without the one of the original          ;;;
;;;    drawable.                                                            ;;;
;;;  * Optimized the method of duplication.                                 ;;;
;;;  * Lots of improvements, cleanups and minor bug fixes regarding both    ;;;
;;;    the code and the dialog window.                                      ;;;
;;;                                                                         ;;;
;;; 1.06 - November 2013                                                    ;;;
;;;  * Increased the maximum number of copies allowed from 64 to 512.       ;;;
;;;  * Decreased the minimum value allowed for the final scale factors from ;;;
;;;    0.25 to 0.00.                                                        ;;;
;;;  * Added the ability to start transforming the duplicates relative to   ;;;
;;;    one another beginning from any step of duplication.                  ;;;
;;;  * Renamed some settings in the dialog window and changed the default   ;;;
;;;    interpolation method to 'Cubic'.                                     ;;;
;;;  * Fixed some minor bugs; enhanced and cleaned up the code.             ;;;
;;;                                                                         ;;;
;;; 1.07 - November 2013                                                    ;;;
;;;  * Fixed an issue occurring when the active drawable is a layer group,  ;;;
;;;    causing the script to crash instead of displaying the proper warning ;;;
;;;    message about the inability to operate on layer groups.              ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *multi-replicate-2.8.0* 1)

(let* ((gimp-version-comparison
        (lambda (predicate version-a version-b)
          (letrec ((func
                    (lambda (string1 p q)
                      (cond ((= (string-length string1) p)
                             (* (expt 100 q) (string->atom string1)))
                            ((eqv? (string-ref string1 p)
                                   (integer->char 46))
                             (+ (* (expt 100 q)
                                   (string->atom (substring string1 0 p)))
                                (func (substring string1
                                                 (+ p 1)
                                                 (string-length string1))
                                      0
                                      (- q 1))))
                            (else (func string1 (+ p 1) q))))))
            (predicate (func version-a 0 2)
                       (func version-b 0 2))))))
  (cond ((gimp-version-comparison < (car (gimp-version)) "2.6.10")
         (quit))
        ((gimp-version-comparison < (car (gimp-version)) "2.8.0")
         (set! *multi-replicate-2.8.0* 0))))

(define (script-fu-multi-replicate img1
                                   img1-drw1
                                   num-copies
                                   autocrop-copies
                                   disp-x
                                   disp-y
                                   disp-unit
                                   final-scale-x
                                   final-scale-y
                                   start-rot-angle
                                   incrm-rot-angle
                                   rot-center-x
                                   rot-center-y
                                   coord-unit
                                   coord-origin
                                   shift-rot-center
                                   init-transf-step
                                   interp-method
                                   stack-duplicates
                                   group-copies
                                   merge-copies)
  (let* ((handler (car (gimp-message-get-handler))))
    (gimp-message-set-handler MESSAGE-BOX)
    (cond ((> (car (gimp-image-get-floating-sel img1)) -1)
           (gimp-message "The script does not operate on floating selections.")
           (gimp-message-set-handler handler)
           (quit)))
    (cond ((= *multi-replicate-2.8.0* 0)
           ())
          ((= (car (gimp-item-is-group img1-drw1)) TRUE)
           (gimp-message "The script does not operate on layer groups.")
           (gimp-message-set-handler handler)
           (quit)))
    (gimp-message-set-handler handler))
  (cond ((= *multi-replicate-2.8.0* 0)
         (define (gimp-item-is-layer item)
           (gimp-drawable-is-layer item))
         (define (gimp-item-is-layer-mask item)
           (gimp-drawable-is-layer-mask item))
         (define (gimp-item-is-channel item)
           (gimp-drawable-is-channel item))
         (define (gimp-item-get-parent item)
           (cons FALSE ()))
         (define (gimp-item-is-group item)
           (cons FALSE ()))
         (define (gimp-image-get-item-position image item)
           (cond ((= (car (gimp-drawable-is-layer item)) TRUE)
                  (gimp-image-get-layer-position image item))
                 ((= (car (gimp-drawable-is-channel item)) TRUE)
                  (gimp-image-get-channel-position image item))
                 ((= (car (gimp-vectors-is-valid item)) TRUE)
                  (gimp-image-get-vectors-position image item))))
         (define (gimp-item-get-lock-content item)
           (cons FALSE ()))
         (define (gimp-item-set-lock-content item lock-content))
         (define (gimp-item-get-linked item)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-get-linked item)
               (gimp-drawable-get-linked item)))
         (define (gimp-item-set-linked item linked)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-set-linked item linked)
               (gimp-drawable-set-linked item linked)))
         (define (gimp-item-get-visible item)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-get-visible item)
               (gimp-drawable-get-visible item)))
         (define (gimp-item-set-visible item visible)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-set-visible item visible)
               (gimp-drawable-set-visible item visible)))
         (define (gimp-item-get-name item)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-get-name item)
               (gimp-drawable-get-name item)))
         (define (gimp-item-set-name item name)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-set-name item name)
               (gimp-drawable-set-name item name)))
         (define (gimp-image-insert-layer image layer parent position)
           (gimp-image-add-layer image layer position))
         (define (gimp-image-insert-channel image channel parent position)
           (gimp-image-add-channel image channel position))
         (define (gimp-item-delete item)
           (gimp-drawable-delete item))
         (define (gimp-context-get-transform-direction))
         (define (gimp-context-set-transform-direction transform-direction)
           (set! gimp-context-get-transform-direction
                 (lambda () (cons transform-direction ()))))
         (define (gimp-context-get-interpolation))
         (define (gimp-context-set-interpolation interpolation)
           (set! gimp-context-get-interpolation
                 (lambda () (cons interpolation ()))))
         (define (gimp-context-get-transform-recursion))
         (define (gimp-context-set-transform-recursion transform-recursion)
           (set! gimp-context-get-transform-recursion
                 (lambda () (cons transform-recursion ()))))
         (define (gimp-context-get-transform-resize))
         (define (gimp-context-set-transform-resize transform-resize)
           (set! gimp-context-get-transform-resize
                 (lambda () (cons transform-resize ()))))
         (define (gimp-layer-scale layer new-width new-height local-origin)
           (gimp-layer-scale-full layer
                                  new-width
                                  new-height
                                  local-origin
                                  (car (gimp-context-get-interpolation))))
         (define (gimp-item-get-image item)
           (if (= (car (gimp-vectors-is-valid item)) TRUE)
               (gimp-vectors-get-image item)
               (gimp-drawable-get-image item)))
         (define (gimp-item-transform-rotate item
                                             angle
                                             auto-center
                                             center-x
                                             center-y)
           (gimp-drawable-transform-rotate
            item
            angle
            auto-center
            center-x
            center-y
            (car (gimp-context-get-transform-direction))
            (car (gimp-context-get-interpolation))
            TRUE
            (car (gimp-context-get-transform-recursion))
            (car (gimp-context-get-transform-resize))))
         (set! group-copies FALSE)))
  (define (round2 num int)
    (let* ((rnd (/ (truncate (+ (* (expt 10 int) num)
                                (if (< num 0) -0.5 0.5)))
                   (expt 10 int))))
      (if (= int 0)
          (inexact->exact rnd)
          rnd)))
  (cond ((and (= (car (gimp-item-is-layer img1-drw1)) FALSE)
              (= (car (gimp-item-is-layer-mask img1-drw1)) FALSE))
         (define (gimp-item-copy item add-alpha)
           (gimp-channel-copy item))
         (define (gimp-image-insert-item image item parent position)
           (gimp-image-insert-channel image item parent position))
         (define (gimp-item-t-s-r item
                                  center-x
                                  center-y
                                  scale-x
                                  scale-y
                                  angle
                                  offset-x
                                  offset-y
                                  num-x
                                  num-y)
           (gimp-drawable-offset item
                                 FALSE
                                 OFFSET-TRANSPARENT
                                 offset-x
                                 offset-y))
         (define (gimp-image-remove-item image item)
           (gimp-image-remove-channel image item)))
        ((or (= (car (gimp-item-is-layer img1-drw1)) TRUE)
             (= (car (gimp-item-is-layer-mask img1-drw1)) TRUE))
         (define (gimp-item-copy item add-alpha)
           (gimp-layer-copy item add-alpha))
         (define (gimp-image-insert-item image item parent position)
           (gimp-image-insert-layer image item parent position))
         (define (gimp-item-t-s-r item
                                  center-x
                                  center-y
                                  scale-x
                                  scale-y
                                  angle
                                  offset-x
                                  offset-y
                                  num-x
                                  num-y)
           (if (not (and (= offset-x 0) (= offset-y 0)))
               (gimp-layer-translate item offset-x offset-y))
           (cond ((not (and (= scale-x 1) (= scale-y 1)))
                  (let* ((width1
                          (* scale-x (car (gimp-drawable-width item))))
                         (height1
                          (* scale-y (car (gimp-drawable-height item))))
                         (width2 (cond ((< width1 1) 1)
                                       ((> width1 262144) 262144)
                                       (else width1)))
                         (height2 (cond ((< height1 1) 1)
                                        ((> height1 262144) 262144)
                                        (else height1))))
                    (gimp-layer-scale item width2 height2 TRUE))
                  (gimp-progress-init " " 0)))
           (if (integer? (/ angle 90))
               (let* ((rot (modulo (inexact->exact (/ angle 90)) 4)))
                 (cond ((not (= rot 0))
                        (plug-in-rotate
                         RUN-NONINTERACTIVE
                         (car (gimp-item-get-image item))
                         item
                         rot
                         FALSE)
                        (case rot
                          ((1) (gimp-layer-translate item
                                                     (+ num-x num-y)
                                                     (- (- num-x num-y))))
                          ((2) (gimp-layer-translate item
                                                     (* 2 num-x)
                                                     (* 2 num-y)))
                          ((3) (gimp-layer-translate item
                                                     (- num-x num-y)
                                                     (+ num-x num-y)))))))
               (begin (gimp-item-transform-rotate
                       item
                       (round2 (* (/ (atan 1) 45) angle) 4)
                       FALSE
                       center-x
                       center-y)
                      (gimp-progress-init " " 0))))
         (define (gimp-image-remove-item image item)
           (gimp-image-remove-layer image item))))
  (let* ((num-copies (round2 num-copies 0))
         (disp-x (round2 disp-x 2))
         (disp-y (round2 disp-y 2))
         (final-scale-x (round2 final-scale-x 2))
         (final-scale-y (round2 final-scale-y 2))
         (init-transf-step (round2 init-transf-step 0))
         (start-rot-angle (round2 start-rot-angle 2))
         (incrm-rot-angle (round2 incrm-rot-angle 2))
         (rot-center-x (round2 rot-center-x 2))
         (rot-center-y (round2 rot-center-y 2))
         (img1-cnl1 (car (gimp-image-get-selection img1)))
         (img1-drw1-i-l-m (car (gimp-item-is-layer-mask img1-drw1)))
         (img1-drw1 (if (= img1-drw1-i-l-m TRUE)
                        (car (gimp-layer-from-mask img1-drw1))
                        img1-drw1))
         (img1-drw1-i-c (car (gimp-item-is-channel img1-drw1)))
         (img1-drw1-parent (car (gimp-item-get-parent img1-drw1)))
         (img1-drw1-position
          (car (gimp-image-get-item-position img1 img1-drw1)))
         (img1-drw1-lock-content
          (car (gimp-item-get-lock-content img1-drw1)))
         (img1-drw1-linked (car (gimp-item-get-linked img1-drw1)))
         (img1-drw1-visible (car (gimp-item-get-visible img1-drw1)))
         (img1-drw1-name (car (gimp-item-get-name img1-drw1)))
         (img1-drw1-h-a (car (gimp-drawable-has-alpha img1-drw1)))
         (img1-drw1-width (car (gimp-drawable-width img1-drw1)))
         (img1-drw1-height (car (gimp-drawable-height img1-drw1)))
         (img1-width (car (gimp-image-width img1)))
         (img1-height (car (gimp-image-height img1)))
         (img2-type (truncate (/ (car (gimp-drawable-type img1-drw1)) 2)))
         (copy-ids (make-vector num-copies))
         (img1-cnl2)
         (img1-grp1)
         (img2)
         (drw0)
         (drw0-parent)
         (drw0-position)
         (drw0-width)
         (drw0-height)
         (drw0-ofs-x)
         (drw0-ofs-y)
         (drw0-mdl-x)
         (drw0-mdl-y)
         (img2-drw1)
         (img1-drw2)
        )
    (gimp-context-push)
    (gimp-context-set-transform-direction TRANSFORM-FORWARD)
    (gimp-context-set-interpolation interp-method)
    (gimp-context-set-transform-recursion 3)
    (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
    (gimp-image-undo-group-start img1)
    (if (= img1-drw1-i-c FALSE)
        (gimp-image-set-active-layer img1 img1-drw1))
    (set! img1-cnl2 (car (gimp-channel-copy img1-cnl1)))
    (gimp-selection-none img1)
    (cond ((and (= img1-drw1-i-c FALSE)
                (= group-copies TRUE))
           (set! img1-grp1 (car (gimp-layer-group-new img1)))
           (gimp-item-set-name img1-grp1
                               (string-append img1-drw1-name " copies"))
           (gimp-image-insert-layer img1
                                    img1-grp1
                                    img1-drw1-parent
                                    (+ img1-drw1-position
                                       stack-duplicates))))
    (cond ((and (= img1-drw1-i-c FALSE)
                (= merge-copies TRUE)
                (> num-copies 1))
           (set! img2
                 (car (gimp-image-new img1-drw1-width
                                      img1-drw1-height
                                      img2-type)))
           (set! drw0 (car (gimp-layer-new-from-drawable img1-drw1 img2)))
           (gimp-image-insert-layer img2 drw0 0 0)
           (set! drw0-parent 0)
           (set! drw0-position 0)
           (gimp-item-set-visible drw0 TRUE)
           (if (= img1-drw1-i-l-m TRUE)
               (gimp-layer-set-edit-mask drw0 FALSE)))
          (else
           (set! drw0 (car (gimp-item-copy img1-drw1 img1-drw1-h-a)))
           (gimp-item-set-name drw0 (string-append img1-drw1-name "z"))
           (gimp-image-insert-item img1
                                   drw0
                                   img1-drw1-parent
                                   (+ img1-drw1-position
                                      stack-duplicates))
           (if (and (= img1-drw1-i-c FALSE)
                    (= group-copies TRUE))
               (begin (set! drw0-parent img1-grp1)
                      (set! drw0-position 0))
               (begin (set! drw0-parent img1-drw1-parent)
                      (set! drw0-position
                            (+ img1-drw1-position stack-duplicates))))))
    (gimp-item-set-lock-content drw0 FALSE)
    (gimp-item-set-linked drw0 FALSE)
    (cond ((= img1-drw1-i-c TRUE)
           (if (eqv? (string->symbol img1-drw1-name) 'Qmask)
               (gimp-item-set-visible drw0 FALSE)))
          ((= autocrop-copies TRUE)
           (gimp-layer-add-alpha drw0)
           (gimp-layer-resize drw0
                              (+ img1-drw1-width 2)
                              (+ img1-drw1-height 2)
                              1
                              1)
           (plug-in-autocrop-layer RUN-NONINTERACTIVE
                                   (car (gimp-item-get-image drw0))
                                   drw0)
           (cond ((and (= start-rot-angle 0)
                       (= incrm-rot-angle 0)
                       (= final-scale-x 1)
                       (= final-scale-y 1))
                  (if (= img1-drw1-h-a FALSE)
                      (gimp-layer-flatten drw0)))
                 ((> (car (gimp-layer-get-mask drw0)) -1)
                  (gimp-layer-remove-mask drw0 MASK-APPLY)))))
    (set! drw0-width (car (gimp-drawable-width drw0)))
    (set! drw0-height (car (gimp-drawable-height drw0)))
    (set! drw0-ofs-x (car (gimp-drawable-offsets drw0)))
    (set! drw0-ofs-y (car (cdr (gimp-drawable-offsets drw0))))
    (set! drw0-mdl-x (+ drw0-ofs-x (truncate (/ drw0-width 2))))
    (set! drw0-mdl-y (+ drw0-ofs-y (truncate (/ drw0-height 2))))
    (case disp-unit
      ((0 1) (set! disp-x (* 0.01 disp-x drw0-width))
             (set! disp-y (* 0.01 disp-y drw0-height)))
      ((2)   (set! disp-x (* 0.01 disp-x img1-width))
             (set! disp-y (* 0.01 disp-y img1-height))))
    (case coord-unit
      ((0 1) (set! rot-center-x (* 0.01 rot-center-x drw0-width))
             (set! rot-center-y (* 0.01 rot-center-y drw0-height)))
      ((2)   (set! rot-center-x (* 0.01 rot-center-x img1-width))
             (set! rot-center-y (* 0.01 rot-center-y img1-height))))
    (gimp-progress-init " " 0)
    (gimp-progress-set-text "0 %")
    (let i-loop ((i 0)
                 (i-fcr ())
                 (i-scl-x ())
                 (i-scl-y ())
                 (i-par1 (if (and (= img1-drw1-i-c FALSE)
                                  (= disp-unit 1))
                             0
                             1))
                 (i-ofs-x 0)
                 (i-ofs-y 0)
                 (i-par2 (if (= coord-origin 1) 0 1))
                 (i-par3 (if (= coord-unit 1) 0 1))
                 (i-ctr-x ())
                 (i-ctr-y ())
                 (i-psn (+ drw0-position stack-duplicates)))
      (set! i-fcr (if (< i (- init-transf-step 1)) 0 1))
      (set! i-scl-x
            (+ 1
               (* i-fcr
                  (/ (- (+ i 1) (- init-transf-step 1))
                     (- num-copies (- init-transf-step 1)))
                  (- final-scale-x 1))))
      (set! i-scl-y
            (+ 1
               (* i-fcr
                  (/ (- (+ i 1) (- init-transf-step 1))
                     (- num-copies (- init-transf-step 1)))
                  (- final-scale-y 1))))
      (set! i-ofs-x
            (+ i-ofs-x
               (* i-fcr 
                  (truncate (* (+ i-scl-x (* i-par1 (- 1 i-scl-x)))
                               disp-x)))))
      (set! i-ofs-y
            (+ i-ofs-y
               (* i-fcr
                  (truncate (* (+ i-scl-y (* i-par1 (- 1 i-scl-y)))
                               disp-y)))))
      (set! i-ctr-x
            (+ (* i-par2 drw0-ofs-x)
               (truncate (* (+ i-scl-x (* i-par3 (- 1 i-scl-x)))
                            rot-center-x))
               (* i-fcr shift-rot-center i-ofs-x)))
      (set! i-ctr-y
            (+ (* i-par2 drw0-ofs-y)
               (truncate (* (+ i-scl-y (* i-par3 (- 1 i-scl-y)))
                            rot-center-y))
               (* i-fcr shift-rot-center i-ofs-y)))
      (vector-set! copy-ids
                   i
                   (car (gimp-item-copy drw0 img1-drw1-h-a)))
      (gimp-item-set-name (vector-ref copy-ids i) img1-drw1-name)
      (gimp-image-insert-item (car (gimp-item-get-image drw0))
                              (vector-ref copy-ids i)
                              drw0-parent
                              i-psn)
      (gimp-item-t-s-r (vector-ref copy-ids i)
                       i-ctr-x
                       i-ctr-y
                       i-scl-x
                       i-scl-y
                       (* i-fcr (+ start-rot-angle (* i incrm-rot-angle)))
                       i-ofs-x
                       i-ofs-y
                       (- i-ctr-x (+ drw0-mdl-x i-ofs-x))
                       (- i-ctr-y (+ drw0-mdl-y i-ofs-y)))
      (gimp-item-set-lock-content (vector-ref copy-ids i)
                                  img1-drw1-lock-content)
      (gimp-item-set-linked (vector-ref copy-ids i) img1-drw1-linked)
      (gimp-progress-set-text
       (string-append (atom->string
                       (round2 (* 100 (/ (+ i 1) num-copies)) 0))
                      " %"))
      ; (gimp-progress-update (/ (+ i 1) num-copies))
      (if (< (+ i 1) num-copies)
          (i-loop (+ i 1)
                  i-fcr
                  i-scl-x
                  i-scl-y
                  i-par1
                  i-ofs-x
                  i-ofs-y
                  i-par2
                  i-par3
                  i-ctr-x
                  i-ctr-y
                  (+ i-psn stack-duplicates))))
    (gimp-image-remove-item (car (gimp-item-get-image drw0)) drw0)
    (cond ((and (= img1-drw1-i-c FALSE)
                (= merge-copies TRUE)
                (> num-copies 1))
           (set! img2-drw1
                 (car (gimp-image-merge-visible-layers img2
                                                       EXPAND-AS-NECESSARY)))
           (set! img1-drw2
                 (car (gimp-layer-new-from-drawable img2-drw1 img1)))
           (gimp-item-set-name img1-drw2 img1-drw1-name)
           (cond ((= group-copies TRUE)
                  (gimp-image-insert-layer img1 img1-drw2 img1-grp1 0))
                 ((= group-copies FALSE)
                  (gimp-image-insert-layer img1
                                           img1-drw2
                                           img1-drw1-parent
                                           (+ img1-drw1-position
                                              stack-duplicates))))
           (gimp-item-set-visible img1-drw2 img1-drw1-visible)
           (gimp-item-set-lock-content img1-drw2 img1-drw1-lock-content)
           (gimp-item-set-linked img1-drw2 img1-drw1-linked)
           (gimp-layer-set-lock-alpha
            img1-drw2
            (car (gimp-layer-get-lock-alpha img1-drw1)))
           (gimp-image-delete img2)
           (set! copy-ids (vector img1-drw2))))
    (gimp-channel-combine-masks img1-cnl1 img1-cnl2 CHANNEL-OP-ADD 0 0)
    (gimp-item-delete img1-cnl2)
    (gimp-image-undo-group-end img1)
    (gimp-displays-flush)
    (gimp-context-pop)
    copy-ids))

(let* ((widget-a '(1 1 512 1 1 0 0))
       (widget-b '(0 -4096 4096 1 5 2 0))
       (widget-c '("% of duplicate extents"
                   "% of scaled duplicate extents"
                   "% of image extents"
                   "Pixel"))
       (widget-d '(1 0 4 0.01 0.01 2 1))
       (widget-e '(0 -180 180 0.10 7.50 2 0))
       (widget-f '(50 -262144 262144 1 5 2 0))
       (widget-g '("Upper left corner of original layer"
                   "Upper left corner of image"))
       (widget-h '("InterpolationType" "cubic"))
       (widget-i '("Above each other"
                   "Below each other")))
  ;;
  (script-fu-register
   "script-fu-multi-replicate"
   "Multi-Replicate..."
   "Duplicate layers and channels in advanced mode. \nfile:multi-replicate.scm"
   "Gino D <ginodonig@gmail.com>"
   "Gino D"
   "2010-2013"
   "*"
   SF-IMAGE       "Image"                                      0
   SF-DRAWABLE    "Drawable"                                   0
   SF-ADJUSTMENT _"Number of copies"                           widget-a
   SF-TOGGLE     _"Autocrop layer copies"                      TRUE
   SF-ADJUSTMENT _"Displacement in X direction"                widget-b
   SF-ADJUSTMENT _"Displacement in Y direction"                widget-b
   SF-OPTION     _"Unit of measure for displacements"          widget-c
   SF-ADJUSTMENT _"Final scale factor in X direction"          widget-d
   SF-ADJUSTMENT _"Final scale factor in Y direction"          widget-d
   SF-ADJUSTMENT _"Starting angle of rotation (in degrees)"    widget-e
   SF-ADJUSTMENT _"Incremental angle of rotation (in degrees)" widget-e
   SF-ADJUSTMENT _"X coordinate of rotation center"            widget-f
   SF-ADJUSTMENT _"Y coordinate of rotation center"            widget-f
   SF-OPTION     _"Unit of measure for coordinates"            widget-c
   SF-OPTION     _"Origin of coordinates"                      widget-g
   SF-TOGGLE     _"Shift rotation center with layer copies"    TRUE
   SF-ADJUSTMENT _"Step to start transforming from"            widget-a
   SF-ENUM       _"Interpolation method"                       widget-h
   SF-OPTION     _"Stack duplicates"                           widget-i
   SF-TOGGLE     _"Group layer copies"                         TRUE
   SF-TOGGLE     _"Merge layer copies together"                FALSE))

(script-fu-menu-register "script-fu-multi-replicate"
                         "<Toolbox>/Script-Fu/Layer")

(script-fu-menu-register "script-fu-multi-replicate"
                         "<Image>/Edit")

(script-fu-menu-register "script-fu-multi-replicate"
                         "<Layers>")

(script-fu-menu-register "script-fu-multi-replicate"
                         "<Channels>")