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
;; Merge all linked layers.
;; 
;

(define (script-fu-sg-merge-linked image)
  (gimp-image-undo-group-start image)
  (let* ((layers (vector->list (cadr (gimp-image-get-layers image))))
         (visibles (let loop ((layers layers)
                              (visibles '()) )
                        (if (null? layers)
                          visibles 
                          (loop (cdr layers)
                                (if (zero? (car (gimp-drawable-get-visible (car layers))))
                                  visibles
                                  (cons (car layers) visibles) )))))
         (mergibles (let loop ((layers layers)
                              (mergibles '()) )
                        (if (null? layers)
                          mergibles 
                          (loop (cdr layers)
                                (if (zero? (car (gimp-drawable-get-linked (car layers))))
                                  mergibles
                                  (cons (car layers) mergibles) ))))
         )
        )
    (map (lambda (layer) 
                 (gimp-drawable-set-visible layer FALSE) )
         layers )
    (map (lambda (layer) 
                 (gimp-drawable-set-visible layer TRUE) )
         mergibles )
    (gimp-image-merge-visible-layers image EXPAND-AS-NECESSARY)
    (let loop ((layers visibles))
      (if (null? layers)
        #t
        (begin
          (unless (zero? (car (gimp-drawable-is-valid (car layers))))
              (gimp-drawable-set-visible (car layers) TRUE)
          )
          (loop (cdr layers)) 
        )
      )
    )
  )
  (gimp-displays-flush)
  (gimp-image-undo-group-end image)
)
      
        
(script-fu-register "script-fu-sg-merge-linked"
  "Merge linked"
  "Merge all linked layers (ignoring visibility) \nfile:sg-merge-linked.scm"
  "Saul Goode"
  "Saul Goode"
  "January 2011"
  "*"
  SF-IMAGE    "Image"    0
)

(script-fu-menu-register "script-fu-sg-merge-linked"
 "<Toolbox>/Script-Fu/Map/"
 )

