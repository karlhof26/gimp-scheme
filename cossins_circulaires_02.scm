; cossins_circulaires.scm
; Download :  
;
;   Copyright (C) 2013 samj ( http://aljacom.com/ )
;
;   Licence GPL 
;       Ce fichier est le Script-Fu "cossins_circulaires" pour Gimp. 
;       Ce Script-Fu "cossins_circulaires" est un logiciel libre ; vous pouvez le redistribuer ou le modifier suivant les termes de la GNU General Public License
;       telle que publiée par la Free Software Foundation ; soit la version 3 de la licence, soit (à votre gré) toute version ultérieure.
;		Ce Script-Fu "cossins_circulaires" est distribué dans l'espoir qu'il sera utile, mais SANS AUCUNE GARANTIE ; 
;		pas même la garantie implicite de COMMERCIABILISABILITÉ ni d'ADÉQUATION à UN OBJECTIF PARTICULIER.
;		Consultez la GNU General Public License pour plus de détails.
;		Vous devez avoir reçu une copie de la GNU General Public License en même temps que GIMP ; si ce n'est pas le cas, consultez <http://www.gnu.org/licenses>
;
;	Ce fichier cossins_circulaires.scm est édité avec Notepad++    http://notepad-plus-plus.org/
;
;
;	Version 1.01 du 20130522 Corrections mineures et ajouts dans le texte de présentation
;	Version 1 du 20130517
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
;	Fichier > Créer > Motifs > Cossins Circulaires ...
;	File > Create > Patterns > Cossins Circulaires ...
;
;	Les résultats sont la somme de 4 mouvements circulaires uniformes. Chaque mouvement circulaire est défini par un rayon, une vitesse de rotation (fréquence), un angle de déphasage, un sens de rotation.
;	Le paramétrage de ce script-fu n'est pas simple.
;
;	The results are the sum of 4 uniform circular motion. Each circular motion is defined by a radius, a rotation speed (frequency), a phase angle, a direction of rotation.
;	The setting of this script-fu is not easy.
;
;	Ce script permet d'obtenir un petit chemin "Path_Inbetweener" afin d'utiliser "path-inbetweener" créé par Ofnuts, fichier path-inbetweener-0.0.py (testé avec version v0.0: 2012-06-22 )
;	This script can create a small path "Path_Inbetweener" to use "path inbetweener" created by Ofnuts.
;		site :           http://gimp-path-tools.sourceforge.net/tools.shtml#path-inbetweener
;		téléchargement : http://sourceforge.net/projects/gimp-path-tools/files/scripts/path-inbetweener-0.0.py/download
;
;
; exemples de courbes significatives :
;	Radius 1			400		400		150
;	Frequency 1			1		0.25	1
;	Angle 1				?		?		270
;	Reverse Rotation 1	?		?		?
;	Radius 2			80		80		125
;	Frequency 2			5		1.25	1.25
;	Angle 2				?		?		270
;	Reverse Rotation 2	?		?		?
;	Radius 3			20		4		100
;	Frequency 3			100		20		2
;	Angle 3				?		?		270
;	Reverse Rotation 3	?		?		?
;	Radius 4			0		8		0
;	Frequency 4			0		100		0
;	Angle 4				?		?		0
;	Reverse Rotation 4	?		?		?
;	Time End			1		4		4
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
; Inspiration :
;		http://www.mathcurve.com/courbes2d/epicycloid/epicycloid.shtml
;		http://www.mathcurve.com/courbes2d/hypocycloid/hypocycloid.shtml
;		http://www.mathcurve.com/courbes2d/epitrochoid/epitrochoid.shtml
;		http://www.mathcurve.com/courbes2d/poursuite/poursuitemutuelle.shtml
;		http://fr.wikipedia.org/wiki/%C3%89picyclo%C3%AFde
;		http://fr.wikipedia.org/wiki/Hypotrocho%C3%AFde
;		http://fr.wikipedia.org/wiki/%C3%89pitrocho%C3%AFde
;		http://fr.wikipedia.org/wiki/Spirographe_(jeu)
;		http://www.immo-tt.be/spirograph/index.htm
;		http://www.wordsmith.org/~anu/java/spirograph.html
;		http://www.youtube.com/watch?feature=player_embedded&v=YsiPHWn68Q8
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;	Il est possible de créer un Spirographe, des épitrochoïdes avec ce script-fu.
;	Frequency 3 = Frequency 4 = Radius 3 = Radius 3 = 0
;
;	*****
;
;	It is possible to create a Spirograph, Epitrochoids with this script-fu.
;	Frequency 3 = Frequency 4 = Radius 3 = Radius 3 = 0
;
;	http://en.wikipedia.org/wiki/Spirograph
;	http://en.wikipedia.org/wiki/Epitrochoid
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;	Il est possible de créer des épicycloïde et des hypocycloïdes avec ce script-fu.
;	Frequency 3 = Frequency 4 = Radius 3 = Radius 3 = 0
;
;	Utiliser un coefficient de très petite valeur pour les fréquences. Exemple KF = 3.5
;
;	Pour créer une épicycloïde :
;		Frequency 1 = 10
;		Frequency 2 = Frequency 1 * KF = 10 * 3.5 = 35
;		Radius 2 = 50
;		Radius 1 = KF * Radius 2 = 3.5*50 = 175
;		Reverse Rotation 1 = 0
;		Reverse Rotation 2 = 0
;
;	Pour créer une hypocycloïde :
;		Frequency 1 = 10
;		Frequency 2 = Frequency 1 * KF = 10 * 3.5  = 35
;		Radius 2 = 50
;		Radius 1 = KF * Radius 2 = 3.5*50 = 175
;		Reverse Rotation 1 = 0
;		Reverse Rotation 2 = 1
;
;	*****
;
;	It is possible to create Epicycloids and Hypocycloids with this script-fu.
;	Frequency 3 = Frequency 4 = Radius 3 = Radius 3 = 0
;
;	Use a very small coefficient value for the frequency. Example KF = 3.5
;
;	To create an Epicycloid :
;		Frequency 1 = 10
;		Frequency 2 = Frequency 1 * KF = 10 * 3.5 = 35
;		Radius 2 = 50
;		Radius 1 = KF * Radius 2 = 3.5*50 = 175
;		Reverse Rotation 1 = 0
;		Reverse Rotation 2 = 0
;
;	To create a Hypocycloid :
;		Frequency 1 = 10
;		Frequency 2 = Frequency 1 * KF = 10 * 3.5  = 35
;		Radius 2 = 50
;		Radius 1 = KF * Radius 2 = 3.5*50 = 175
;		Reverse Rotation 1 = 0
;		Reverse Rotation 2 = 1
;
;	http://en.wikipedia.org/wiki/Epicycloid
;	http://en.wikipedia.org/wiki/Hypocycloid
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; It is possible to create interesting curves from Spirographs, Epitrochoids, Epicycloids,  Hypocycloids.
; - Radius 4 = Frequency 4 = 0
; - Frequency 3 = high value
; - Radius 3 = low value
; - Reverse Rotation 2 = 0 or 1
;
; Example :
;  - Frequency 1 = 1
;  - Frequency 2 = 5
;  - Frequency 3 = 80
;  - Frequency 4 = 0
;  - Radius 1 = 400
;  - Radius 2 = 200
;  - Radius 3 = 20
;  - Radius 4 = 0
;  - Reverse Rotation 1 = 0
;  - Reverse Rotation 2 = 0
;  - Reverse Rotation 3 = 0
;  - Reverse Rotation 4 = 0
;  - Time End = 1
;
;

        
(define	(Cossins_Circulaires
        Rayon_1
        Frequence_1
        Dephasage_1
        Inversion_Rotation_1
        Rayon_2
        Frequence_2
        Dephasage_2
        Inversion_Rotation_2
        Rayon_3
        Frequence_3
        Dephasage_3
        Inversion_Rotation_3
        Rayon_4
        Frequence_4
        Dephasage_4
        Inversion_Rotation_4
        temps_fin
        Color_Pencil_1
        Pencil_1_Line_Thickness
        Color_intensity
        Autocrop_Image
        Save_Path_As_SVG
        Creer_Petit_Chemin
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
            ;; (width 0)
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
            
            (x_1 0)
            (Y_1 0)
            (X_2 0)
            (Y_2 0)
            (X_3 0)
            (Y_3 0)
            (X_4 0)
            (Y_4 0)
            (X_total 0)
            (Y_total 0)
            
            (epaisseur_ligne Pencil_1_Line_Thickness)
            
            
            (Chemin_Cossins_Circulaires)
            
            (sortie_boucle_tracer 0)
            (Nom_de_la_brosse "Ma_Brosse_Spirale_samj")
            (The_shape_of_the_generated_brush 0) ; "Circle" "Square" "Diamond"
            (rayon_brosse (/ Pencil_1_Line_Thickness 2) )
            
            (Nombre_Points_Chemin 0)
            (Points_Chemin)
            (Ajout_Points_Chemin)
            (Fermer_Chemin 0)
            
            (filename "qwerty")
            
            (sortie_boucle 0)
            (progress_counter 1)
            (percent_loop 0.1)
            
            (chemin_reduit)
            (Points_chemin_reduit)
            (Ajout_Points_chemin_reduit)
            
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
        (gimp-message "reached one")
        
        
        ;******************************************************************************************************
        ;        
        ;******************************************************************************************************
        
        
        (set! width (* 2 (+  (abs Rayon_1) (abs Rayon_2) (abs Rayon_3) (abs Rayon_4) (* 2 epaisseur_ligne) ) ) )
        (set! height width )
        
        
        ; créer une nouvelle image rgb
        (set! image (car (gimp-image-new width height 0)))
        
        ;; Start undo group.
        (gimp-image-undo-group-start image)
        
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
        (set! Dephasage_1 (* (/ 2pi 360) Dephasage_1) )
        (set! Dephasage_2 (* (/ 2pi 360) Dephasage_2) )
        (set! Dephasage_3 (* (/ 2pi 360) Dephasage_3) )
        (set! Dephasage_4 (* (/ 2pi 360) Dephasage_4) )
        
        ; sens de rotation angulaire
        (if (= Inversion_Rotation_1 1) (set! Frequence_1 (* Frequence_1 -1) )  )
        (if (= Inversion_Rotation_2 1) (set! Frequence_2 (* Frequence_2 -1) )  )
        (if (= Inversion_Rotation_3 1) (set! Frequence_3 (* Frequence_3 -1) )  )
        (if (= Inversion_Rotation_4 1) (set! Frequence_4 (* Frequence_4 -1) )  )
        
        (set! increment_temps (/ 1 (* 180 temps_fin (+ (abs Frequence_1) (abs Frequence_2) (abs Frequence_3) (abs Frequence_4) ) ) ) )
        
        ; fonctionne avec  8280 , ne fonctionne pas avec 8370
        (if (> (/ temps_fin increment_temps) 8280)
            (begin
                (set! increment_temps (/ temps_fin 8280) )
            )
        )
        
        ; (gimp-message (number->string (/ temps_fin increment_temps) ) )
        
        (set! temps_origine 0 )
        (gimp-message "reached two")
        
        (set! decalage_X (/ width 2) )
        (set! decalage_Y (/ height 2) )
        
        (set! x_1 (* Rayon_1 (cos (+ (* (* 2pi Frequence_1) temps_origine ) Dephasage_1 ) ) ) )
        (set! Y_1 (* Rayon_1 (sin (+ (* (* 2pi Frequence_1) temps_origine ) Dephasage_1 ) ) ) )
        (set! X_2 (* Rayon_2 (cos (+ (* (* 2pi Frequence_2) temps_origine ) Dephasage_2 ) ) ) )
        (set! Y_2 (* Rayon_2 (sin (+ (* (* 2pi Frequence_2) temps_origine ) Dephasage_2 ) ) ) )
        (set! X_3 (* Rayon_3 (cos (+ (* (* 2pi Frequence_3) temps_origine ) Dephasage_3 ) ) ) )
        (set! Y_3 (* Rayon_3 (sin (+ (* (* 2pi Frequence_3) temps_origine ) Dephasage_3 ) ) ) )
        (set! X_4 (* Rayon_4 (cos (+ (* (* 2pi Frequence_4) temps_origine ) Dephasage_4 ) ) ) )
        (set! Y_4 (* Rayon_4 (sin (+ (* (* 2pi Frequence_4) temps_origine ) Dephasage_4 ) ) ) )
        
        (set! X_total (+ decalage_X x_1 X_2 X_3 X_4) )
        (set! Y_total (+ decalage_Y Y_1 Y_2 Y_3 Y_4) )
        (gimp-progress-pulse)
        
        (set! Points_Chemin (list X_total Y_total X_total Y_total X_total Y_total ) )
        
        (set! Nombre_Points_Chemin 6 )
        
        (set! Chemin_Cossins_Circulaires (car (gimp-vectors-new image "Chemin_Cossins_Circulaires") ) )
        (gimp-image-add-vectors image Chemin_Cossins_Circulaires 0)
        
        
        ; chemin réduit
        (if (= Creer_Petit_Chemin 1)
            (begin
                (set! x_1 (/ x_1 Coeff_reduction) )
                (set! Y_1 (/ Y_1 Coeff_reduction) )
                (set! X_2 (/ X_2 Coeff_reduction) )
                (set! Y_2 (/ Y_2 Coeff_reduction) )
                (set! X_3 (/ X_3 Coeff_reduction) )
                (set! Y_3 (/ Y_3 Coeff_reduction) )
                (set! X_4 (/ X_4 Coeff_reduction) )
                (set! Y_4 (/ Y_4 Coeff_reduction) )
                (set! X_total (+ (+ (+ (+ decalage_X x_1) X_2) X_3) X_4) )
                (set! Y_total (+ decalage_Y Y_1 Y_2 Y_3 Y_4) )
                (set! Points_chemin_reduit (list X_total Y_total X_total Y_total X_total Y_total ) )
            )
        )
        
        (gimp-progress-init "Calculating points" 0)
        (gimp-progress-update 0.2)
        
        (while (= 0 sortie_boucle)
            (begin
                
                
                ;(gimp-message "inside while 1")
                (set! temps_origine (+ temps_origine increment_temps) )
                
                ; mauvais : impossibilité dans quelques cas de transformer le chemin en sélection
                ;  (if (> temps_origine temps_fin) 
                ;       (begin
                ;           (set! sortie_boucle 1 )
                ;       )
                ;  )
                
                ; bon
                (if (< temps_origine temps_fin) 
                    (begin
                        ; (set! sortie_boucle 0 )
                    )
                    ;else
                    (begin
                        (set! sortie_boucle 1 )
                    )
                )
                
                
                (set! x_1 (* Rayon_1 (cos (+ (* (* 2pi Frequence_1) temps_origine ) Dephasage_1 ) ) ) )
                (set! Y_1 (* Rayon_1 (sin (+ (* (* 2pi Frequence_1) temps_origine ) Dephasage_1 ) ) ) )
                (set! X_2 (* Rayon_2 (cos (+ (* (* 2pi Frequence_2) temps_origine ) Dephasage_2 ) ) ) )
                (set! Y_2 (* Rayon_2 (sin (+ (* (* 2pi Frequence_2) temps_origine ) Dephasage_2 ) ) ) )
                (set! X_3 (* Rayon_3 (cos (+ (* (* 2pi Frequence_3) temps_origine ) Dephasage_3 ) ) ) )
                (set! Y_3 (* Rayon_3 (sin (+ (* (* 2pi Frequence_3) temps_origine ) Dephasage_3 ) ) ) )
                (set! X_4 (* Rayon_4 (cos (+ (* (* 2pi Frequence_4) temps_origine ) Dephasage_4 ) ) ) )
                (set! Y_4 (* Rayon_4 (sin (+ (* (* 2pi Frequence_4) temps_origine ) Dephasage_4 ) ) ) )
                
                (set! X_total (+ (+ (+ (+ decalage_X x_1) X_2) X_3) X_4) )
                (set! Y_total (+ (+ (+ (+ decalage_Y Y_1) Y_2) Y_3) Y_4) )
                
                (set! Nombre_Points_Chemin (+ 6 Nombre_Points_Chemin ) )
                
                (set! Ajout_Points_Chemin (list X_total Y_total X_total Y_total X_total Y_total ) )
                
                (set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )
                
                
                ; chemin réduit
                (if (= Creer_Petit_Chemin 1)
                    (begin
                        (set! x_1 (/ x_1 Coeff_reduction) )
                        (set! Y_1 (/ Y_1 Coeff_reduction) )
                        (set! X_2 (/ X_2 Coeff_reduction) )
                        (set! Y_2 (/ Y_2 Coeff_reduction) )
                        (set! X_3 (/ X_3 Coeff_reduction) )
                        (set! Y_3 (/ Y_3 Coeff_reduction) )
                        (set! X_4 (/ X_4 Coeff_reduction) )
                        (set! Y_4 (/ Y_4 Coeff_reduction) )
                        (set! X_total (+ (+ (+ (+ decalage_X x_1) X_2) X_3) X_4) )
                        (set! Y_total (+ (+ (+ (+ decalage_Y Y_1) Y_2) Y_3) Y_4) )
                        (set! Ajout_Points_chemin_reduit (list X_total Y_total X_total Y_total X_total Y_total ) )
                        (set! Points_chemin_reduit (append Points_chemin_reduit Ajout_Points_chemin_reduit) )
                    )
                )
                ;(gimp-message "inside while 1 looping")
                (set! percent_loop (/ temps_origine temps_fin))
                (gimp-message (number->string percent_loop))                
                (set! progress_counter (+ progress_counter 1))
                (gimp-message (number->string progress_counter))
                ;(gimp-progress-update (/ progress_counter 8500))
                (gimp-progress-update percent_loop)
            )
        )
        
        
        
        
        
        ; exemple 4 côtés pour pouvoir transformer chemin en sélection
        
        ;	(set! Nombre_Points_Chemin 24)
        ;   	(set! X_debut 77.777777)
        ;	(set! Y_debut 77.777777)
        ;	(set! Points_Chemin (list X_debut Y_debut X_debut Y_debut X_debut Y_debut ) )
        
        ;	(set! X_fin 333.33333)
        ;	(set! Y_fin 77.777777)
        ;	(set! Ajout_Points_Chemin (list X_fin Y_fin X_fin Y_fin X_fin Y_fin ) )
        ;	(set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )    
        
        ;	(set! X_fin 333.33333)
        ;	(set! Y_fin 333.33333)
        ;	(set! Ajout_Points_Chemin (list X_fin Y_fin X_fin Y_fin X_fin Y_fin ) )
        ;	(set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )
        
        ;	(set! X_fin 77.777777)
        ;	(set! Y_fin 333.33333)
        ;	(set! Ajout_Points_Chemin (list X_fin Y_fin X_fin Y_fin X_fin Y_fin ) )
        ;	(set! Points_Chemin (append Points_Chemin Ajout_Points_Chemin) )
        
        ;******************************************************************************************************
        ; 
        ;******************************************************************************************************
        
        ; créer le chemin
        
        (if (= temps_fin 1)  (set! Fermer_Chemin 1)  )
        (if (= temps_fin 2)  (set! Fermer_Chemin 1)  )
        (if (= temps_fin 3)  (set! Fermer_Chemin 1)  )
        (if (= temps_fin 4)  (set! Fermer_Chemin 1)  )
        
        (gimp-progress-pulse)
        
        (gimp-vectors-stroke-new-from-points
            Chemin_Cossins_Circulaires
            0
            Nombre_Points_Chemin
            (list->vector Points_Chemin )
            Fermer_Chemin
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
        
        (gimp-brush-delete Nom_de_la_brosse)
        (gimp-progress-pulse)
        
        ; créer le chemin réduit
        (if (= Creer_Petit_Chemin 1)
            (begin
                (set! chemin_reduit (car (gimp-vectors-new image "Path_Inbetweener") ) )
                ; (gimp-image-add-vectors image chemin_reduit 0)
                (gimp-image-insert-vectors image chemin_reduit 0 -1)
                (gimp-vectors-stroke-new-from-points
                    chemin_reduit
                    0
                    Nombre_Points_Chemin
                    (list->vector Points_chemin_reduit )
                    Fermer_Chemin
                )
            )
        )
        
        
        (if (= 1 Autocrop_Image) (plug-in-autocrop-layer 1 image calque_fond) )
        
        
        ; ajuster taille image à la taille des calques
        (gimp-image-resize-to-layers image)
        
        (if (= 1 Save_Path_As_SVG) 
            (begin
                (set! width (car (gimp-image-width image) ) )
                (set! height (car (gimp-image-height image) ) )
                (set! filename (string-append gimp-directory "/" (strcat "Cossins_Circulaires_" (number->string width ) "_" (number->string height )   ".svg") ) )
                (gimp-vectors-export-to-file image filename Chemin_Cossins_Circulaires)
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
        
        (gimp-message "good finish")
        
        
        
  )
)



;	(script-fu-register "Cossins_Circulaires"
;                    _"<Image>/File/Create/Patterns/Cossins Circulaires ..."
;                    "Cossins Circulaires ..."
;                    "samj"
;                    "samj"
;                    "20130517"
;                    ""
;					SF-ADJUSTMENT "Rayon 1 " '(400 -800 800 1 5 0 1)
;					SF-ADJUSTMENT "Frequence 1 " '(1 0 100 1 5 2 1)
;					SF-ADJUSTMENT "Dephasage 1 " '(0 0 360 1 5 4 1)
;					SF-TOGGLE "Inversion Rotation 1 " FALSE
;					SF-ADJUSTMENT "Rayon 2 " '(80 -800 800 1 5 0 1)
;					SF-ADJUSTMENT "Frequence 2 " '(5 0 100 1 5 2 1)
;					SF-ADJUSTMENT "Dephasage 2 " '(0 0 360 1 5 4 1)
;					SF-TOGGLE "Inversion Rotation 2 " FALSE
;					SF-ADJUSTMENT "Rayon 3 " '(20 -800 800 1 5 0 1)
;					SF-ADJUSTMENT "Frequence 3 " '(50 0 100 1 5 2 1)
;					SF-ADJUSTMENT "Dephasage 3 " '(0 0 360 1 5 4 1)
;					SF-TOGGLE "Inversion Rotation 3 " FALSE
;					SF-ADJUSTMENT "Rayon 4 " '(0 -800 800 1 5 0 1)
;					SF-ADJUSTMENT "Frequence 4 " '(0 0 100 1 5 2 1)
;					SF-ADJUSTMENT "Dephasage 4 " '(0 0 360 1 5 4 1)
;					SF-TOGGLE "Inversion Rotation 4 " FALSE
;					SF-ADJUSTMENT "Temps Fin " '(1 0.01 4 1 2 2 1)
;					SF-COLOR "Color Pencil 1 " '(0 0 0)
;					SF-ADJUSTMENT "Pencil 1 - Line Thickness " '(10 2 40 1 5 0 1)
;					SF-ADJUSTMENT "Color intensity " '(3 1 10 1 5 0 1)
;					SF-TOGGLE "Autocrop " FALSE
;					SF-TOGGLE "Save Path As SVG " FALSE
;					SF-TOGGLE "Create Path__Inbetweener " FALSE
;					SF-ADJUSTMENT "Reduction Coefficient To Create Path__Inbetweener" '(10 2 12 5 4 1)
;					
;
;
;	)

(script-fu-register "Cossins_Circulaires"
                    "Cossins Circulaires  Spirograph Epicycloid..."
                    "Circulaires Spirograph Epicycloid. This file contains examples of good values and reference websites etc. \nfile:cossins_circulaires_02.scm"
                    "samj"
                    "samj"
                    "20130517"
                    ""
                    SF-ADJUSTMENT "Radius 1 " '(400 -800 800 1 5 0 1)
                    SF-ADJUSTMENT "Frequency 1 " '(1 0 100 1 5 2 1)
                    SF-ADJUSTMENT "Angle 1 " '(0 0 360 1 5 4 1)
                    SF-TOGGLE "Reverse Rotation 1 " FALSE
                    SF-ADJUSTMENT "Radius 2 " '(80 -800 800 1 5 0 1)
                    SF-ADJUSTMENT "Frequency 2 " '(5 0 100 1 5 2 1)
                    SF-ADJUSTMENT "Angle 2 " '(0 0 360 1 5 4 1)
                    SF-TOGGLE "Reverse Rotation 2 " FALSE
                    SF-ADJUSTMENT "Radius 3 " '(16 -800 800 1 5 0 1)
                    SF-ADJUSTMENT "Frequency 3 " '(25 0 100 1 5 2 1)
                    SF-ADJUSTMENT "Angle 3 " '(0 0 360 1 5 4 1)
                    SF-TOGGLE "Reverse Rotation 3 " FALSE
                    SF-ADJUSTMENT "Radius 4 " '(4 -800 800 1 5 0 1)
                    SF-ADJUSTMENT "Frequency 4 " '(100 0 100 1 5 2 1)
                    SF-ADJUSTMENT "Angle 4 " '(0 0 360 1 5 4 1)
                    SF-TOGGLE "Reverse Rotation 4 " FALSE
                    SF-ADJUSTMENT "Time End " '(1 0.01 4 1 2 2 1)
                    SF-COLOR "Color Pencil 1 " '(0 0 0)
                    SF-ADJUSTMENT "Pencil 1 - Line Thickness " '(10 2 40 1 5 0 1)
                    SF-ADJUSTMENT "Color intensity " '(3 1 10 1 5 0 1)
                    SF-TOGGLE "Autocrop " FALSE
                    SF-TOGGLE "Save Path As SVG " FALSE
                    SF-TOGGLE "Create Path__Inbetweener " FALSE
                    SF-ADJUSTMENT "Reduction Coefficient To Create Path__Inbetweener" '(10 2 12 5 4 1)
)

(script-fu-menu-register "Cossins_Circulaires"
                         "<Toolbox>/Script-Fu/Render/Pattern/")

; end of script

;; FIN