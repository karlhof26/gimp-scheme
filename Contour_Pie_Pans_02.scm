; Contour_Pie_Pans.scm
; Download : 
; 
;	Copyright (C) 2013 samj ( http://aljacom.com/ )
; 
;	Licence GPL
;		Ce fichier est le Script-Fu "Contour_Pie_Pans.scm" pour Gimp. 
;		Ce Script-Fu "Contour_Pie_Pans.scm" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;		telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;		Ce Script-Fu "Contour_Pie_Pans.scm" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;		pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;		Consultez la GNU General Public License pour plus de détails.
;		Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;
;	Ce fichier Contour_Pie_Pans.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;
;
;
;	Version 1 du 20130520
;
;	Ce script est adapté pour obtenir un petit chemin "Path_Inbetweener" afin d'utiliser "path-inbetweener" créé par Ofnuts, fichier path-inbetweener-0.0.py (testé avec version v0.0: 2012-06-22 )
;		site :           http://gimp-path-tools.sourceforge.net/tools.shtml#path-inbetweener
;		téléchargement : http://sourceforge.net/projects/gimp-path-tools/files/scripts/path-inbetweener-0.0.py/download
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;	Accès par :
;
;	Fichier > Créer > Motifs > Contour Pie Pans ...
;	File > Create > Patterns > Contour Pie Pans ...
;
;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;

		
(define (Contour_Pie_Pans_samj
        Type_Contour
        Rayon_1
        Rayon_2
        Frequence_2
        Angle_2
        Color_Pencil_1
        Pencil_1_Line_Thickness
        Color_intensity
        Autocrop_Image
        Save_Path_As_SVG
        Coeff_reduction
        
    )
    (let* 
        (
            
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
            
            (width 0)
            (width 0)
            (height 0)
            (image)
            
            ;calque_fond
            (calque_fond)
            (angle_origine 0)
            (angle_fin_boucle 0)
            (2pi (* 2 3.14159265358979323846))
            
            (decalage_X 0)
            (decalage_Y 0)
            (temps_origine 0)
            (increment_temps 0)
            (temps_fin 0)
            (X_1 0)
            (Y_1 0)
            (X_total 0)
            (Y_total 0)
            
            (epaisseur_ligne Pencil_1_Line_Thickness)
            
            
            (Chemin_Contour_Pie_Pans_samj)
            
            (sortie_boucle_tracer 0)
            (Nom_de_la_brosse "Ma_Brosse_Spirale_samj")
            (The_shape_of_the_generated_brush 0) ; "Circle" "Square" "Diamond"
            (rayon_brosse (/ Pencil_1_Line_Thickness 2) )
            
            (Nombre_Points_Chemin 0)
            (Points_Chemin)
            (Ajout_Points_Chemin)
            
            (filename "qwerty")
            
            (sortie_boucle 0)
            
            (chemin_reduit)
            (Points_chemin_reduit)
            (Ajout_Points_chemin_reduit)
            
            (Frequence_1 1)
            (Angle_1 0)
            (Rayon_Resultant 0)
            
        )
        
        
        (gimp-context-set-antialias FALSE)
        (gimp-context-set-feather FALSE)
        (gimp-context-set-feather-radius 0 0)
        (gimp-context-set-sample-merged FALSE)
        (gimp-context-set-sample-criterion 0)
        (gimp-context-set-sample-threshold 0.15)
        (gimp-context-set-sample-transparent FALSE)
        
        (gimp-context-set-transform-direction TRANSFORM-FORWARD)
        (gimp-context-set-interpolation 2)
        (gimp-context-set-transform-recursion 3)
        (gimp-context-set-transform-resize TRANSFORM-RESIZE-ADJUST)
        
        (gimp-context-set-paint-mode 0) ;
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        (set! width (* 2 (+  Rayon_1 Rayon_2 (* 2 epaisseur_ligne) ) ) )
        (set! height width )
        
        ; créer une nouvelle image rgb
        (set! image (car (gimp-image-new width height 0)))
        
        ;; Start undo group.
        (gimp-image-undo-group-start image)
        (gimp-message "started OK")
        
        ;; sélectionner la console erreurs pour envoyer un message
        (gimp-message-set-handler 2)
        
        ; créer calque_fond
        (set! calque_fond (car (gimp-layer-new image width height 1 "calque_fond" 100 0)))	
        
        ; ajouter le calque calque_fond
        (gimp-image-insert-layer image calque_fond 0 -1)
        
        (gimp-drawable-fill calque_fond 3)
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        ;transformation degrés radians
        (set! Angle_1 (* (/ 2pi 360) Angle_1) )
        (set! Angle_2 (* (/ 2pi 360) Angle_2) )
        
        
        
        (set! temps_fin 1)	
        (set! increment_temps (/ temps_fin (* 180 (+ Frequence_1 (abs Frequence_2) ) ) ) )
        
        (if (> (/ temps_fin increment_temps) 8280)
            (begin
                (set! increment_temps (/ temps_fin 8280) )
            )
        )   
        
        ; (gimp-message (number->string (/ temps_fin increment_temps) ) )
        
        (set! temps_origine 0 )
         
        
        (set! decalage_X (/ width 2) )
        (set! decalage_Y (/ height 2) )
        
        (if (= Type_Contour 0)
            (begin
                (set! Rayon_Resultant (+  Rayon_1 (* Rayon_2   (sin (+ (* (* 2pi Frequence_2) temps_origine ) Angle_2 ) )    ) ) )
            )
        )
        
        ;(gimp-message "reached 2nd type")
        (if (= Type_Contour 1)
            (begin
                (if (> (sin (+ (* (* 2pi Frequence_2) temps_origine ) Angle_2 ) )   0)
                    (begin
                        (set! Rayon_Resultant (+  Rayon_1 Rayon_2) )
                    )
                    ;else
                    (begin
                        (set! Rayon_Resultant Rayon_1 )
                    )
                )
                
            )
        )
        
        
        (set! X_1 (* Rayon_Resultant (cos (+ (* (* 2pi Frequence_1) temps_origine ) Angle_1 ) ) ) )
        (set! Y_1 (* Rayon_Resultant (sin (+ (* (* 2pi Frequence_1) temps_origine ) Angle_1 ) ) ) )
        
        
        (set! X_total (+ decalage_X X_1) )
        (set! Y_total (+ decalage_Y Y_1) )
        
        
        (set! Points_Chemin (list X_total Y_total X_total Y_total X_total Y_total ) )
        
        (set! Nombre_Points_Chemin 6 )
        
        (set! Chemin_Contour_Pie_Pans_samj (car (gimp-vectors-new image "Contour_Pie_Pans") ) )
        (gimp-image-add-vectors image Chemin_Contour_Pie_Pans_samj 0)
        
        ; chemin réduit
        (set! X_1 (/ X_1 Coeff_reduction) )
        (set! Y_1 (/ Y_1 Coeff_reduction) )
        (set! X_total (+ decalage_X X_1) )
        (set! Y_total (+ decalage_Y Y_1) )
        (set! Points_chemin_reduit (list X_total Y_total X_total Y_total X_total Y_total ) )
        ;(gimp-message "reached while")
        (gimp-message (number->string temps_fin))
        
        
        
        (while (= 0 sortie_boucle)
            (begin
                
                
                ;;(gimp-message "while loop inside")
                
                ;(if (= (remainder (round (* temps_origine 100)) 1) 0) ; operators are modulo and remainder 
                ;    (begin
                ;        (gimp-message "*****&******")
                ;        (gimp-message (number->string temps_origine))
                ;        (gimp-progress-set-text  (string-append "So far:" (number->string temps_origine))) 
                ;    )
                ;)
                ;(gimp-message "*****&BB&******")
                (gimp-message (number->string temps_origine))
                (gimp-progress-set-text  (string-append "Rendering points: " (number->string (floor (* temps_origine 100) 2)) "%")) 
                (gimp-progress-update temps_origine)
                
                (set! temps_origine (+ temps_origine increment_temps) )
                
                
                ; bon
                (if (< temps_origine temps_fin) 
                    (begin
                        (set! sortie_boucle 0 ) ; was 0 and commented out
                    )
                    ;else
                    (begin
                        (set! sortie_boucle 1 ) ; was 1
                    )
                )
                
                (if (= Type_Contour 0)
                    (begin
                        (set! Rayon_Resultant (+  Rayon_1 (* Rayon_2   (sin (+ (* (* 2pi Frequence_2) temps_origine ) Angle_2 ) )    ) ) )
                    )
                )
                
                (if (= Type_Contour 1)
                    (begin
                        (if (> (sin (+ (* (* 2pi Frequence_2) temps_origine ) Angle_2 ) )   0)
                            (begin
                                (set! Rayon_Resultant (+  Rayon_1 Rayon_2) )
                            )
                            ;else
                            (begin
                                (set! Rayon_Resultant Rayon_1 )
                            )
                        )
                        
                    )
                )
                
                (set! X_1 (* Rayon_Resultant (cos (+ (* (* 2pi Frequence_1) temps_origine ) Angle_1 ) ) ) )
                (set! Y_1 (* Rayon_Resultant (sin (+ (* (* 2pi Frequence_1) temps_origine ) Angle_1 ) ) ) )
                
                (set! X_total (+ decalage_X X_1) )
                (set! Y_total (+ decalage_Y Y_1) )
                
                (set! Nombre_Points_Chemin (+ 6 Nombre_Points_Chemin ) )
                
                (set! Ajout_Points_Chemin (list X_total Y_total X_total Y_total X_total Y_total ) )
                
                (set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )
                
                ; chemin réduit
                (set! X_1 (/ X_1 Coeff_reduction) )
                (set! Y_1 (/ Y_1 Coeff_reduction) )
                (set! X_total (+ decalage_X X_1) )
                (set! Y_total (+ decalage_Y Y_1) )
                (set! Ajout_Points_chemin_reduit (list X_total Y_total X_total Y_total X_total Y_total ) )
                (set! Points_chemin_reduit (append Points_chemin_reduit Ajout_Points_chemin_reduit) )
                ;;(gimp-message "looping...")
            )
        )
        
        
        
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        ; créer le chemin
        (gimp-vectors-stroke-new-from-points
            Chemin_Contour_Pie_Pans_samj
            0
            Nombre_Points_Chemin
            (list->vector Points_Chemin )
            TRUE
        )
        
        
        
        (gimp-context-set-foreground Color_Pencil_1)
        
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
        
        (set! sortie_boucle_tracer Color_intensity)
        (while (> sortie_boucle_tracer 0)
            (begin
                (gimp-edit-stroke-vectors calque_fond (car (gimp-image-get-active-vectors image) ) )
                (set! sortie_boucle_tracer (- sortie_boucle_tracer 1) )
            )
        )
        (gimp-message "second while ended ok")
        
        (gimp-brush-delete Nom_de_la_brosse)
        
        ; créer le chemin réduit
        (set! chemin_reduit (car (gimp-vectors-new image "Path_Inbetweener") ) )
        ; (gimp-image-add-vectors image chemin_reduit 0)
        (gimp-image-insert-vectors image chemin_reduit 0 -1)
        (gimp-vectors-stroke-new-from-points
            chemin_reduit
            0
            Nombre_Points_Chemin
            (list->vector Points_chemin_reduit )
            TRUE
        )
        
        
        
        
        (if (= 1 Autocrop_Image) (plug-in-autocrop-layer 1 image calque_fond) )
        
        
        ; ajuster taille image à la taille des calques
        (gimp-image-resize-to-layers image)
        
        (if (= 1 Save_Path_As_SVG) 
            (begin
                (set! width (car (gimp-image-width image) ) )
                (set! height (car (gimp-image-height image) ) )
                (set! filename (string-append gimp-directory "/" (strcat "Contour_Pie_Pans_samj_" (number->string width ) "_" (number->string height )   ".svg") ) )
                (gimp-vectors-export-to-file image filename Chemin_Contour_Pie_Pans_samj)
                (gimp-message (strcat "Save Path As :" filename ) )
            )
        )
        
        
        ;******************************************************************************************************
        ; 
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
        
        
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        
        ; afficher l'image
        (gimp-display-new image)
        
        ; End undo group.
        (gimp-image-undo-group-end image)
        
    )
)

(script-fu-register "Contour_Pie_Pans_samj"
                    "<Toolbox>/File/Create/Patterns/Contour Pie Pans ..."
                    "Contour Pie Pans. Creates a Sin or Square wave on a circle border. Similar to a cookie cutter shape. Long running.\nfile:Contour_Pie_Pans_02.scm"
                    "samj"
                    "samj"
                    "20130520"
                    ""
                    SF-OPTION "Type Contour " '("Sine Wave" "Square Wave") 
                    SF-ADJUSTMENT "Radius 1 " '(400 16 800 1 5 0 1)
                    SF-ADJUSTMENT "Radius 2 " '(20 16 800 1 5 0 1)
                    SF-ADJUSTMENT "Frequency 2 " '(32 0 100 1 5 0 1)
                    SF-ADJUSTMENT "Angle 2 " '(0 0 360 1 5 4 1)
                    SF-COLOR "Color Pencil 1 " '(0 0 0)
                    SF-ADJUSTMENT "Pencil 1 - Line Thickness " '(6 2 50 1 5 0 1)
                    SF-ADJUSTMENT "Color intensity " '(3 1 10 1 5 0 1)
                    SF-TOGGLE "Autocrop " FALSE
                    SF-TOGGLE "Save Path As SVG " FALSE
                    SF-ADJUSTMENT "Reduction Coefficient To Create Path Inbetweener " '(10 2 12 5 4 1)
)

;; FIN