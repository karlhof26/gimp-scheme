;; Licensed for any usage allowed by law. 
;;
;; Script is based upon a tutorial by 'Fencepost' of GIMPtalk.com
;; The original tutorial is available at
;; http://www.gimptalk.com/forum/topic/Making-An-Animated-Flag-2443-1.html
;;
;;


(define (script-fu-flag-wave image drawable frames waviness speed deac depth)
  (let* (
      (width (car (gimp-image-width image)))
      (height (car (gimp-image-height image)))
      (layer)
      (orig-name (car (gimp-drawable-get-name drawable)))
      (orig)
      (cloud)
      (tmp-cloud)
      (xsize 0.1)
      (ysize 0.1)
      (total-frames frames)
      )
    (gimp-image-undo-group-start image)
    (set! cloud (car (gimp-image-flatten image)))
    (gimp-layer-add-alpha cloud)
    (gimp-drawable-set-name cloud "cloud")
    (gimp-image-resize image (* width 1.20) (* height 1.20) (/ width 10) (/ height 10))
    (gimp-layer-resize-to-image-size cloud)
    (if (> height width)
      (set! ysize depth)
      (set! xsize depth)
      )
    (set! orig (car (gimp-layer-new-from-drawable cloud image)))
    (gimp-image-insert-layer image orig 0 0)
    (gimp-drawable-set-name orig "orig")
    (plug-in-solid-noise RUN-NONINTERACTIVE image cloud TRUE
                                                        FALSE
                                                        (realtime)
                                                        1
                                                        xsize
                                                        ysize
                                                        )
    (while (> frames 0)
      (gimp-selection-none image)
      (set! layer (car (gimp-layer-new-from-drawable orig image)))
      (gimp-image-insert-layer image layer 0 0)
      (gimp-drawable-set-name layer (string-append orig-name
                                                   " #"
                                                   (number->string (+ (- total-frames frames) 1))
                                                   " (replace)"
                                                   ))
      (set! tmp-cloud (car (gimp-layer-new-from-drawable cloud image)))
      (gimp-image-insert-layer image tmp-cloud 0 0)
      (if (> height width)
        (begin
          (gimp-selection-none image)
          (gimp-drawable-offset cloud TRUE FALSE 0  (- speed (rand (* speed (/ deac 100)))))
          (gimp-rect-select image 0 0 (car (gimp-image-width image)) (/ height 10) CHANNEL-OP-REPLACE FALSE 0)
          (gimp-edit-fill tmp-cloud FILL-FOREGROUND)
          (gimp-selection-none image)
          (gimp-edit-blend tmp-cloud
                           BLEND-FG-TRANSPARENT
                           LAYER-MODE-NORMAL
                           GRADIENT-LINEAR
                           100
                           0
                           REPEAT-NONE
                           FALSE
                           FALSE
                           1
                           0
                           FALSE
                           0 (/ height 10) 0 (/ height 5))
          )
        (begin
          (gimp-selection-none image)
          (gimp-drawable-offset cloud TRUE FALSE (- speed (rand (* speed (/ deac 100)))) 0)
          ;(gimp-rect-select image 0 0 (/ width 10) (car (gimp-image-height image)) CHANNEL-OP-REPLACE FALSE 0)
          (gimp-image-select-rectangle image CHANNEL-OP-REPLACE 0 0 (/ width 10) (car (gimp-image-height image)) )
          (gimp-edit-fill tmp-cloud FILL-FOREGROUND)
          (gimp-selection-none image)
          (gimp-edit-blend tmp-cloud
                           BLEND-FG-TRANSPARENT
                           LAYER-MODE-NORMAL
                           GRADIENT-LINEAR
                           100
                           0
                           REPEAT-NONE
                           FALSE
                           FALSE
                           1
                           0
                           FALSE
                           (/ width 10) 0 (/ width 5) 0)
          )
        )
      (plug-in-bump-map RUN-NONINTERACTIVE image layer
                        tmp-cloud
                        135
                        45
                        depth
                        0 0 0 0
                        TRUE
                        FALSE
                        2
                        )
      (plug-in-displace RUN-NONINTERACTIVE image layer
                        waviness waviness
                        TRUE TRUE
                        tmp-cloud tmp-cloud
                        1
                        )
      (gimp-image-remove-layer image tmp-cloud)
      (set! frames (- frames 1))
      )
    (gimp-image-remove-layer image cloud)
    (gimp-image-remove-layer image orig)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-flag-wave"
  "<Toolbox>/Script-Fu2/Animation/Flag wave"
  "Turns image into an animated, waving flag. \nfile:anim-flag.scm"
  "Saul Goode"
  "Saul Goode"
  "9/1/2007"
  "RGB*"
  SF-IMAGE    "Image"    0
  SF-DRAWABLE "Drawable" 0
  SF-ADJUSTMENT "Frames" '( 10 1 100 1 1 0 1 )
  SF-ADJUSTMENT "Waviness" '( 40 1 200 1 1 0 1 )
  SF-ADJUSTMENT "Speed" '( 40 1 200 1 1 0 1 )
  SF-ADJUSTMENT "Variation" '( 25 1 100 1 1 0 1 )
  SF-ADJUSTMENT "Depth" '( 10 1 100 1 1 0 1 )
)
