; Script-Fu cossin_en_spirale.scm
; Download : http://gimpchat.com/viewtopic.php?f=9&t=6949
;
;   Copyright (C) 2013 samj ( http://aljacom.com/~gmic/ )
;
;   Licence GPL 
;       Ce fichier est le Script-Fu "cossin_en_spirale.scm" pour Gimp. 
;       Ce Script-Fu "cossin_en_spirale.scm" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;       telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;       Ce Script-Fu "cossin_en_spirale.scm" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;       pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;       Consultez la GNU General Public License pour plus de détails.
;       Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;       
;   Ce fichier cossin_en_spirale.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;   
;   
;   
;   
;   Version 2 : 20130512 Ajout Save_Path_As_SVG , version où les chemins peuvent être transformés en sélections (nouvelle façon de créer gimp-vectors-stroke-new-from-points )
;   Version 1 : 20130414 Origine.
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;	Accès sur l'image par :
;
;
;
;
;	Filtres > Cossin En Spirale ...
;	Filters > Cossin En Spirale ...
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	Les chemins sont sauvegardés.
;	Paths are saved.
;






;******************************************************************************************************
; 
;******************************************************************************************************


(define (cossin_en_spirale_samj
        image
        drawable
        Ecartement_du_centre
        angle_origine
        Nombre_de_spirales
        Rotation_angulaire_inversion
        Increment_Spirale
        Color_Pencil_1
        Pencil_1_Line_Thickness
        Color_Pencil_2
        Pencil_2_Line_Thickness
        Color_Pencil_3
        Pencil_3_Line_Thickness
        Color_Pencil_4
        Pencil_4_Line_Thickness
        Color_Pencil_5
        Pencil_5_Line_Thickness
        Color_Pencil_6
        Pencil_6_Line_Thickness
        Color_Pencil_7
        Pencil_7_Line_Thickness
        Color_Pencil_8
        Pencil_8_Line_Thickness
        Color_intensity
        Fill_all_the_image
        Save_Path_As_SVG
        
        
    )
    (let* (
            (dummy 1)
            (message (car (gimp-message-get-handler)))
            (old_context_transform_direction (car (gimp-context-get-transform-direction)))
            (old_context_interpolation (car (gimp-context-get-interpolation)))
            (old_context_recursion (car (gimp-context-get-transform-recursion)))
            (old_context_transform_resize (car (gimp-context-get-transform-resize)))
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            (old_context-antialias (car (gimp-context-get-antialias)))
            (old_context-feather (car (gimp-context-get-feather)))
            (old_context-feather-radiusx (car (gimp-context-get-feather-radius)))
            (old_context-feather-radiusy (cadr (gimp-context-get-feather-radius)))
            (old_context-sample-merged (car (gimp-context-get-sample-merged)))
            (old_ontext-sample-criterion (car (gimp-context-get-sample-criterion)))
            (old_context-sample-threshold (car (gimp-context-get-sample-threshold)))
            (old_context-sample-transparent (car (gimp-context-get-sample-transparent)))
            
            (old-brush-size (car (gimp-context-get-brush-size)))
            (old-brush-aspect-ratio (car (gimp-context-get-brush-aspect-ratio)))
            (old-brush-angle (car (gimp-context-get-brush-angle)))
            (old-brush-name (car (gimp-context-get-brush)))
            
            (old-pattern (car (gimp-context-get-pattern)))
            
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            
            (transparence (car (gimp-drawable-has-alpha drawable)))
            
            
            (2pi (* 2 3.14159265358979323846))
            (calque_spirales)
            (increment_angle (/ 2pi Nombre_de_spirales) )
            
            (Nouvel_Ecartement_du_centre)
            (X_debut)
            (Y_debut)
            (angle_fin)
            (X_fin)
            (Y_fin)
            (chemin_en_spirale)
            (sortie_boucle_secondaire 0)
            (Nom_de_la_brosse "Ma_Brosse_Spirale_samj")
            (The_shape_of_the_generated_brush 0) ; "Circle" "Square" "Diamond"
            (rayon_brosse)
            (sortie_boucle_tracer)
            
            
            (Nombre_Points_Chemin 0)
            (Points_Chemin)
            (Ajout_Points_Chemin)
            
            (filename "qwerty")
            
        )
        
        (gimp-message "Started OK")
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 0 0)
        (gimp-context-set-sample-merged FALSE)
        (gimp-context-set-sample-criterion 0)
        (gimp-context-set-sample-threshold 0)
        (gimp-context-set-sample-transparent FALSE)
        
        (gimp-context-set-transform-direction TRANSFORM-FORWARD)
        (gimp-context-set-interpolation 2)
        (gimp-context-set-transform-recursion 3)
        (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
        
        (gimp-context-set-paint-mode 0) ; 
        
        
        (gimp-image-undo-group-start image) 
        
        
        (gimp-selection-none image)
        
        
        
        
        
        
        ;******************************************************************************************************
        ;
        ;******************************************************************************************************
        
        
        (gimp-selection-none image)
        
        ; créer le premier calque
        (set! calque_spirales (car (gimp-layer-new image width height 0 "calque_spirales" 100 0)))
        
        (gimp-image-insert-layer image calque_spirales 0 -1)  ; was -1 0
        
        (gimp-layer-add-alpha calque_spirales)
        
        (gimp-drawable-fill calque_spirales 3)
        
        (set! angle_origine (* (/ 2pi 360) angle_origine) )
        
        (if (= Rotation_angulaire_inversion 1)
            (set! increment_angle (* -1 increment_angle))
        )
        
        (gimp-progress-init "drawing spiral" -1)
        (while (> Nombre_de_spirales 0)
                (begin
                    (gimp-progress-pulse)
                    ;(gimp-message "line198")
                    (if (= (modulo Nombre_de_spirales 8) 1)
                        (begin
                            (gimp-context-set-foreground Color_Pencil_1)
                            (set! rayon_brosse (/ Pencil_1_Line_Thickness 2) )
                        )
                    )
                    (if (= (modulo Nombre_de_spirales 8) 2) (begin (gimp-context-set-foreground Color_Pencil_2)   (set! rayon_brosse (/ Pencil_2_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 3) (begin (gimp-context-set-foreground Color_Pencil_3)   (set! rayon_brosse (/ Pencil_3_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 4) (begin (gimp-context-set-foreground Color_Pencil_4)   (set! rayon_brosse (/ Pencil_4_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 5) (begin (gimp-context-set-foreground Color_Pencil_5)   (set! rayon_brosse (/ Pencil_5_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 6) (begin (gimp-context-set-foreground Color_Pencil_6)   (set! rayon_brosse (/ Pencil_6_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 7) (begin (gimp-context-set-foreground Color_Pencil_7)   (set! rayon_brosse (/ Pencil_7_Line_Thickness 2) )   ) )
                    (if (= (modulo Nombre_de_spirales 8) 0) (begin (gimp-context-set-foreground Color_Pencil_8)   (set! rayon_brosse (/ Pencil_8_Line_Thickness 2) )   ) )
                    
                    ; créer une brosse
                    (gimp-brush-new Nom_de_la_brosse)
                    
                    (gimp-context-set-brush-size rayon_brosse )
                    ;;;(gimp-context-set-brush-default-size)
                    (gimp-context-set-brush-aspect-ratio 1)
                    (gimp-context-set-brush-angle 0)
                    
                    (gimp-context-set-brush Nom_de_la_brosse)
                    
                    (gimp-brush-set-shape Nom_de_la_brosse The_shape_of_the_generated_brush) ; forme
                    
                    (gimp-brush-set-radius Nom_de_la_brosse rayon_brosse ) ; rayon
                    
                    (gimp-brush-set-spikes Nom_de_la_brosse 2) ; pointes
                    (gimp-brush-set-hardness Nom_de_la_brosse 1.0) ; dureté 
                    (gimp-brush-set-aspect-ratio Nom_de_la_brosse 1 ) ; proportions
                    (gimp-brush-set-angle Nom_de_la_brosse 0) ; angle
                    (gimp-brush-set-spacing Nom_de_la_brosse 1.0 ) ; espacement
                    
                    ; sélectionner brosse
                    (gimp-context-set-brush Nom_de_la_brosse)
                    
                    
                    (set! X_debut (+ (/ width 2) (* Ecartement_du_centre (cos angle_origine) ) ) )
                    (set! Y_debut (+ (/ height 2) (* Ecartement_du_centre (sin angle_origine) ) ) )
                    (set! Nouvel_Ecartement_du_centre (+ Ecartement_du_centre Increment_Spirale) )
                    (set! angle_fin (+ angle_origine increment_angle) )
                    (set! X_fin (+ (/ width 2) (* Nouvel_Ecartement_du_centre (cos angle_fin) ) ) )
                    (set! Y_fin (+ (/ height 2) (* Nouvel_Ecartement_du_centre (sin angle_fin) ) ) )
                    
                    (set! Points_Chemin (list X_debut Y_debut X_debut Y_debut X_debut Y_debut ) )
                    
                    (set! Nombre_Points_Chemin 6 )
                    
                    (set! chemin_en_spirale (car (gimp-vectors-new image "chemin_en_spirale") ) )
                    (gimp-image-add-vectors image chemin_en_spirale 0)
                    
                    (while (= sortie_boucle_secondaire 0)
                            (begin
                                (gimp-progress-pulse)
                                
                               ;(gimp-vectors-stroke-new-from-points
                               ;    chemin_en_spirale
                               ;    0
                               ;    12
                               ;    (vector X_debut Y_debut X_debut Y_debut X_debut Y_debut X_fin Y_fin X_fin Y_fin X_fin Y_fin )
                               ;    TRUE
                               ;)
                                
                                (set! Nombre_Points_Chemin (+ 6 Nombre_Points_Chemin) )
                                
                                (set! Ajout_Points_Chemin (list X_fin Y_fin X_fin Y_fin X_fin Y_fin ) )
                                (set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )
                                
                               ; (set! X_debut X_fin)
                               ; (set! Y_debut Y_fin)
                                (set! Nouvel_Ecartement_du_centre (+ Nouvel_Ecartement_du_centre Increment_Spirale) )
                                (set! angle_fin (+ angle_fin increment_angle) )
                                (set! X_fin (+ (/ width 2) (* Nouvel_Ecartement_du_centre (cos angle_fin) ) ) )
                                (set! Y_fin (+ (/ height 2) (* Nouvel_Ecartement_du_centre (sin angle_fin) ) ) )
                                
                                (if (= Fill_all_the_image 0)
                                    (begin
                                        (if (< X_fin 0) (set! sortie_boucle_secondaire 1) )
                                        (if (> X_fin width) (set! sortie_boucle_secondaire 1) )
                                        (if (< Y_fin 0) (set! sortie_boucle_secondaire 1) )
                                        (if (> Y_fin height) (set! sortie_boucle_secondaire 1) )
                                    )
                                    ;else
                                    (begin
                                        (if (< X_fin (* -1 width) ) (set! sortie_boucle_secondaire 1) )
                                        (if (> X_fin (* 2 width) ) (set! sortie_boucle_secondaire 1) )
                                        (if (< Y_fin (* -1 height) ) (set! sortie_boucle_secondaire 1) )
                                        (if (> Y_fin (* 2 height) ) (set! sortie_boucle_secondaire 1) )
                                    )
                                )
                                
                            )
                    )
                    (gimp-progress-pulse)
                    
                    ; créer le chemin
                    (gimp-vectors-stroke-new-from-points
                        chemin_en_spirale
                        0
                        Nombre_Points_Chemin
                        (list->vector Points_Chemin )
                        FALSE ; TRUE
                    )
                    
                    (set! sortie_boucle_tracer Color_intensity)
                    (while (> sortie_boucle_tracer 0)
                        (begin
                            (gimp-edit-stroke-vectors calque_spirales (car (gimp-image-get-active-vectors image) ) )
                            (set! sortie_boucle_tracer (- sortie_boucle_tracer 1) )
                        )
                    )
                    ;(gimp-image-remove-vectors image chemin_en_spirale)
                    (gimp-brush-delete Nom_de_la_brosse)
                    (set! sortie_boucle_secondaire 0)
                    (set! angle_origine (+ angle_origine increment_angle) )
                    (set! Nombre_de_spirales (- Nombre_de_spirales 1) )
                    
                    
                )
        )
        
        
        
        (if (= 1 Save_Path_As_SVG) 
            (begin
                (set! width (car (gimp-image-width image) ) )
                (set! height (car (gimp-image-height image) ) )
                (set! filename (string-append gimp-directory "/" (strcat "Cossin_En_Spirale_" (number->string width ) "_" (number->string height )   ".svg") ) )
                ; (gimp-vectors-export-to-file image filename chemin_en_spirale)
                (gimp-vectors-export-to-file image filename 0)
                (gimp-message (strcat "Save Path As :" filename ) )
            )
        )
        
        (gimp-item-set-visible calque_spirales TRUE)
        
        ;******************************************************************************************************
        
        ;; rétablir le mode message
        (gimp-message-set-handler message)
        
        (gimp-context-set-transform-direction old_context_transform_direction )
        (gimp-context-set-interpolation old_context_interpolation)
        (gimp-context-set-transform-recursion old_context_recursion)
        (gimp-context-set-transform-resize old_context_transform_resize)
        (gimp-context-set-background old-bg)
        (gimp-context-set-foreground old-fg)
        
        (gimp-context-set-antialias old_context-antialias)
        (gimp-context-set-feather old_context-feather)
        (gimp-context-set-feather-radius old_context-feather-radiusx old_context-feather-radiusy)
        (gimp-context-set-sample-merged old_context-sample-merged)
        (gimp-context-set-sample-criterion old_ontext-sample-criterion)
        (gimp-context-set-sample-threshold old_context-sample-threshold)
        (gimp-context-set-sample-transparent old_context-sample-transparent)
        
        (gimp-context-set-brush-default-size)
        (gimp-context-set-brush-size old-brush-size)
        (gimp-context-set-brush-aspect-ratio old-brush-aspect-ratio)
        (gimp-context-set-brush-angle old-brush-angle)		
        (gimp-context-set-brush old-brush-name)
        
        (gimp-context-set-pattern old-pattern)
        
        (gimp-message "Good finish OK")
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)  
        
    )
)

(script-fu-register "cossin_en_spirale_samj"
                    "Mulicolor Shaped spiral Cossin En Spirale ..."
                    "Mulicolor Shaped spiral Cossin En Spirale. Shape is based on number of spirals selected e.g.5 is a pentagon, 3 a triangle. Also determines the numbers of lines-pencils used. \nfile:cossin_en_spirale_02.scm"
                    "samj"
                    "samj"
                    "20130414"
                    "*"
                    SF-IMAGE    "Image"     0
                    SF-DRAWABLE "Drawable"  0
                    SF-ADJUSTMENT "Distance from the center "   '(8 8 128 1 2 0 1)
                    SF-ADJUSTMENT "Angle of departure "         '(0 0 360 1 2 2 1)
                    SF-ADJUSTMENT "Number of spirals "          '(3 3 32 1 2 0 1)
                    SF-TOGGLE "Angular rotation inversion "     FALSE
                    SF-ADJUSTMENT "Increment Spiral "           '(8 3 128 1 10 0 1)
                    SF-COLOR "Color Pencil 1 " '(0 0 0)
                    SF-ADJUSTMENT "Pencil 1 - Line Thickness " '(2 2 20 1 5 0 1) ; '(2 2 20 1 5 0 1)
                    SF-COLOR "Color Pencil 2 " '(255 0 0)
                    SF-ADJUSTMENT "Pencil 2 - Line Thickness " '(2 2 20 1 5 0 1)
                    SF-COLOR "Color Pencil 3 " '(0 255 0)
                    SF-ADJUSTMENT "Pencil 3 - Line Thickness " '(2 2 20 1 5 0 1)
                    SF-COLOR "Color Pencil 4 " '(0 0 255)
                    SF-ADJUSTMENT "Pencil 4 - Line Thickness " '(2 2 20 1 5 0 1)
                    SF-COLOR "Color Pencil 5 " '(255 255 0)
                    SF-ADJUSTMENT "Pencil 5 - Line Thickness " '(2 2 20 1 5 0 1)
                    
                    SF-COLOR "Color Pencil 6 " '(255 0 255)
                    SF-ADJUSTMENT "Pencil 6 - Line Thickness " '(2 2 20 1 5 0 1)
                   
                    SF-COLOR "Color Pencil 7 " '(0 255 255)
                    SF-ADJUSTMENT "Pencil 7 - Line Thickness " '(2 2 20 1 5 0 1)
                     
                    SF-COLOR "Color Pencil 8 " '(255 255 255)
                    SF-ADJUSTMENT "Pencil 8 - Line Thickness " '(2 2 20 1 5 0 1)
                    
                    SF-ADJUSTMENT "Color intensity " '(3 1 10 1 5 0 1)
                    SF-TOGGLE "Fill whole image " FALSE
                    
                    SF-TOGGLE "Save Paths As SVG " FALSE
                    
                    
)

(script-fu-menu-register "cossin_en_spirale_samj"
                         "<Toolbox>/Script-Fu/Render/Pattern")



;; FIN