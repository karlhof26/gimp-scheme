;; RGB-TO-HSV taken from http://pages.interlog.com/~kcozens/software/gimp/2.0/neon-sign.scm
;; Convert RGB ([0-1],[0-1],[0-1]) to HSV ([0-360],[0-1],[0-1])

(define (rgb-to-hsv color)
  (let* ((r (car color))
         (g (cadr color))
         (b (caddr color))
         (cmin (min r (min g b)))
         (cmax (max r (max g b)))
         (diff (- cmax cmin))
         (rc (/ (- cmax r) diff))
         (gc (/ (- cmax g) diff))
         (bc (/ (- cmax b) diff))
         (h (/ (if (= r cmax)
               (- bc gc)
               (if (= g cmax)
                   (+ 2.0 (- rc bc))
                   (+ 4.0 (- gc rc))
               )
               )
           6.0))
        )
        (list (if (= cmin cmax)
			  0
			  (* 360 (if (< h 0.0)
					 (+ h 1.0)
					 h)))
		  (if (= cmin cmax)
			  0
			  (/ (- cmax cmin) cmax))
		  cmax)
  )
)


;; Do RGB to HSV in gimp ranges

(define (gimp-rgb-to-hsv color)
  (let* ((r (car color))
         (g (cadr color))
         (b (caddr color))
         (hsv (rgb-to-hsv (list (/ r 255.0) (/ g 255.0) (/ b 255.0))))
         (h (car hsv))
         (s (cadr hsv))
         (v (caddr hsv))
        )
        (list h (* s 100.0) (* v 100.0))
  )
)



(define (midnight-sepia-kw inimage indraw blur blend)
(let* (
        (theImage 0)
        (theDraw 0)
        (theWidth 0)
        (theHeight 0)
        (copy-layer 0)
        (copy-float 0)
        (hsv 0)
        (h 0)
        (s 0)
        (v 0)
        (screen-layer 0)
    )  
    (set! theImage inimage)
    (set! theDraw indraw)
    (gimp-image-undo-group-start theImage)
    (if (= 0 (car(gimp-drawable-has-alpha theDraw)))
    (gimp-layer-add-alpha theDraw))
    (set! theHeight (car (gimp-image-height theImage)))
    (set! theWidth (car (gimp-image-width theImage)))
    (gimp-edit-copy-visible theImage)
    (set! copy-layer (gimp-layer-new theImage theWidth theHeight 1 "copied layer" 100 3) )
    (gimp-drawable-fill (car copy-layer) 3)
    (gimp-image-add-layer theImage (car copy-layer) -1)
    (set! copy-float (car (gimp-edit-paste (car copy-layer) 0)))
    (gimp-floating-sel-anchor copy-float)
    (gimp-image-lower-layer theImage (car copy-layer))
    (gimp-image-raise-layer-to-top theImage (car copy-layer))
    (plug-in-gauss 1 theImage (car copy-layer) blur blur 0)
    (set! hsv (gimp-rgb-to-hsv blend))
    (set! h(car hsv))
    (set! s(cadr hsv))
    (set! v (caddr hsv))
    (gimp-colorize (car copy-layer) h s v)
    
    (gimp-edit-copy-visible theImage )
    (set! screen-layer (gimp-layer-new theImage theWidth theHeight 1 "screen" 100 4) )
    (gimp-drawable-fill (car screen-layer) 3)
    (gimp-image-add-layer theImage (car screen-layer) -1)
    (set! copy-float (car (gimp-edit-paste (car screen-layer) 0)))
    (gimp-floating-sel-anchor copy-float)
    (gimp-image-lower-layer theImage (car screen-layer))
    (gimp-image-raise-layer-to-top theImage (car screen-layer))
    (gimp-image-undo-group-end theImage)
    (gimp-displays-flush)
  )
)
(script-fu-register "midnight-sepia-kw"
    "<Image>/Script-Fu/Kward1979uk/midnight-sepia..."
    "Midnight Sepia applies a soft-focus effect, as well as a slight sepia toning (or any other hue, you can choose) to create a 'dreamy' effect. \nfile: kward1979uk_midnight_sepia.scm"
    "Karl Ward"
    "Karl Ward"
    "Feb 2006"
    ""
    SF-IMAGE      "SF-IMAGE" 0
    SF-DRAWABLE   "SF-DRAWABLE" 0
    SF-ADJUSTMENT "Blur:" '(25 1 100 1 10 0 0)
    SF-COLOR      "Colour:"      '(32 64 54)

)

; end of script