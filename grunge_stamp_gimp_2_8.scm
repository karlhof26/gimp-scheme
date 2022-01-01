; Début du script-fu grunge_stamp_gimp_2_8.scm
;
; Accès par :   Fichier > Créer > Logos > Grunge stamp...
;               File > Create > Logos > Grunge stamp...
;
;
;  adaptation du didacticiel "Create a simple Grunge-Stamp!" de HostedDinner sur http://www.gimpusers.com/tutorials/create-a-simple-grunge-stamp
; 
;
; Licence GNU/GPL 
;
; --------------------------------------------------------------------
; version 1.0 par samj (www.aljacom.com/~gimp) 2010/12/14
; version 1.01 par samj (www.aljacom.com/~gimp) 2010/12/15
; version 1.02 par samj (www.aljacom.com/~gimp) 2010/12/15 Ajout paramètre usure
; version 2.0 par samj (www.aljacom.com/~gimp) 2010/12/15 Adaptation Gimp 2.7.2
; version 2.1 par samj (www.aljacom.com/~gimp) 2011/05/17 Adaptation Gimp 2.7.3 ajout Brosse_usure car noms des brosses modifiés
; --------------------------------------------------------------------
;
;

(define (Grunge_Stamp
                Texte
                Police
                Couleur_encre
                Taille_Police
                Angle
                Contour
                Bordure
                Usure
                Aplatir
                Couleur_fond
                Brosse_usure
        )
        
  (let*
        (
            ; affectation des variables			
            
            ; mémoriser les couleurs PP et AP
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            ; mémoriser brosse
            (old-brush (car (gimp-context-get-brush)))
            
            
            ; caractéristiques de la surface occupée par le texte
            (fond_texte (gimp-text-get-extents-fontname Texte Taille_Police 0 Police))
            
            ; largeur de la future image
            (width (+ (car fond_texte) (* 2 (+ Contour (* 2 Bordure)))))
            
            ; hauteur de la future image
            (height (+ (cadr fond_texte) (* 2 (+ Contour (* 2 Bordure)))))
            
            ; créer une nouvelle image rgb
            (img (car (gimp-image-new width height 0)))
            
            ; calque Texte
            (calque_texte)
            
            ; calque Bordure
            (calque_bordure)
            
            ; masque de calque
            (masque_de_calque)
            
            ; où appliquer la brosse
            (*array_points_brosse* (cons-array 4 'double))
            
            ; index
            (index 1) ; was 0
            
            ; ajout_dimension
            (ajout_dimension 0)
            
        )
        
        ;; Start undo group.
        (gimp-image-undo-group-start img)		
        
        
        ; calque Texte *******************************************************************************		
        
        ; mettre pp = couleur de l'encre
        (gimp-context-set-foreground Couleur_encre)
        
        ; créer le calque texte
        ;                  (gimp-text-fontname image drawable x y text border antialias size size-type fontname)
        (set! calque_texte (car (gimp-text-fontname img -1 (+ Contour (* 2 Bordure)) (+ Contour (* 2 Bordure)) Texte 0 TRUE Taille_Police 0 Police)))
        
        (gimp-item-set-name calque_texte "Texte")
        
        
        ; calque Bordure *******************************************************************************		
        
        ; créer calque Bordure
        (set! calque_bordure (car (gimp-layer-new img width height 1 "Bordure" 100 0)))	
        
        ; ajouter le calque Bordure
        (gimp-image-insert-layer img calque_bordure 0 -1)	
        
        ; créer une sélection rectangulaire intérieure
        ;(gimp-round-rect-select 
        ;						img ; image
        ;						(* 2 Bordure) ; x
        ;						(* 2 Bordure) ; y 
        ;						(- width (* 4 Bordure)) ; width 
        ;						(- height (* 4 Bordure)) ; height 
        ;						1 ; corner-radius-x 
        ;						1 ; corner-radius-y 
        ;						0 ; operation 
        ;						TRUE ; antialias 
        ;						FALSE ; feather 
        ;						0 ; feather-radius-x 
        ;						0 ; feather-radius-y
        ;)	
        ; modifications gimp 2.7.3
        (gimp-context-set-antialias TRUE)
        (gimp-context-set-feather FALSE)
        ; (gimp-context-set-feather-radius feather-radius-x feather-radius-y)
        (gimp-context-set-feather-radius 0 0)
        (gimp-image-select-round-rectangle 
                            img ; image 
                            0 ; operation 
                            (* 2 Bordure) ; x 
                            (* 2 Bordure) ; y 
                            (- width (* 4 Bordure)) ; width 
                            (- height (* 4 Bordure)) ; height 
                            1 ; corner-radius-x 
                            1 ; corner-radius-y
        )
        
        
        
        ; inverser la sélection
        (gimp-selection-invert img)
        
        
        ; créer une sélection rectangulaire extérieure
        ;(gimp-round-rect-select 
        ;                       img ; image
        ;                       Bordure ; x
        ;                       Bordure ; y 
        ;                       (- width (* 2 Bordure)) ; width 
        ;                       (- height (* 2 Bordure)) ; height 
        ;                       1 ; corner-radius-x 
        ;                       1 ; corner-radius-y 
        ;                       3 ; operation (3 intersect)
        ;                       TRUE ; antialias 
        ;                       FALSE ; feather 
        ;                       0 ; feather-radius-x 
        ;                       0 ; feather-radius-y
        ;)  
        ; modifications gimp 2.7.3
        (gimp-context-set-antialias TRUE)
        (gimp-context-set-feather FALSE)
        ; (gimp-context-set-feather-radius feather-radius-x feather-radius-y)
        (gimp-context-set-feather-radius 0 0)
        (gimp-image-select-round-rectangle 
                            img ; image 
                            3 ; operation 
                            Bordure ; x
                            Bordure ; y 
                            (- width (* 2 Bordure)) ; width 
                            (- height (* 2 Bordure)) ; height 
                            1 ; corner-radius-x 
                            1 ; corner-radius-y 
        )
        
        
        ; remplir la sélection de la couleur
        (gimp-edit-bucket-fill calque_bordure 0 0 100 0 TRUE 0 0)
        
        
        ; ne rien sélectionner
        (gimp-selection-none img)
        
        
        ; *******************************************************************************		
        
        ; angle en radians
        (set! Angle ( / (* Angle (* 2 3.14159265358979323846)) 360))
        
        ; rotation des 2 calques
        ;(gimp-item-transform-rotate item angle auto-center center-x center-y)
        (gimp-item-transform-rotate calque_bordure Angle TRUE (/ width 2) (/ height 2))
        (gimp-item-transform-rotate calque_texte Angle TRUE (/ width 2) (/ height 2))
        
        ; ajuster taille image à la taille des calques
        (gimp-image-resize-to-layers img)
        
        ; fusionner les calques et donner une nouvelle valeur à calque_texte
        (set! calque_texte (car (gimp-image-merge-visible-layers img 0)))
        
        ; découpage automatique
        (plug-in-autocrop 1 img calque_texte)
        
        ; nouvelles dimensions de l'image width et height
        (set! width (car (gimp-image-width img)))
        (set! height (car (gimp-image-width img)))
        
        ; ajouter un masque de calque blanc
        (set! masque_de_calque (car (gimp-layer-create-mask calque_texte 0)))
        (gimp-layer-add-mask calque_texte masque_de_calque)
        
        ; mettre pp = noir
        (gimp-context-set-foreground '(0 0 0))
        
        ; mettre ap = blanc
        (gimp-context-set-background '(255 255 255))
        
        ; sélectionner brosse
        ;(gimp-context-set-brush "Confetti")
        (gimp-context-set-brush (car Brosse_usure))
        ;(gimp-context-set-brush "Grass")
        ;(gimp-context-set-brush Brosse_usure)
        
        (gimp-context-set-brush-angle 1.0)
        (gimp-context-set-brush-size 35.0)
        (gimp-context-set-brush-spacing 1.50)
        (gimp-context-set-brush-force 1.0)
        (gimp-context-set-paint-mode LAYER-MODE-NORMAL)
        
        ;(gimp-message (number->string width))
        
        ; boucle pour appliquer la brosse avec le pinceau
        (while (< ajout_dimension (+ width height)) 
            
            ;(gimp-message "paint br")
            
            (gimp-context-set-brush-size (+ (rand 45) 10))
            
            ; incrémentation de l'index
            (set! index (+ 1 index))
            
            ; contenu des 4 valeurs de array
            (aset *array_points_brosse* 0 (rand width))
            (aset *array_points_brosse* 1 (rand height))
            (aset *array_points_brosse* 2 (rand width))
            (aset *array_points_brosse* 3 (rand height))
            
            ; appliquer pinceau
            (gimp-paintbrush 
                    masque_de_calque ; drawable
                    0 ; fade-out
                    4 ; num-strokes 
                    *array_points_brosse* ; strokes 
                    0 ; method 
                    index ; gradient-length
            )
            
            ; contenu des 4 valeurs de array
            (aset *array_points_brosse* 0 (rand width))
            (aset *array_points_brosse* 1 (rand height))
            (aset *array_points_brosse* 2 (rand width))
            (aset *array_points_brosse* 3 (rand height))
            
            ; appliquer pinceau
            (gimp-paintbrush 
                    masque_de_calque ; drawable
                    0 ; fade-out
                    4 ; num-strokes 
                    *array_points_brosse* ; strokes 
                    0 ; method 
                    (round (/ index 2)) ; gradient-length
            )
            
            ; nouvelle valeur de ajout_dimension
            (set! ajout_dimension (+ ajout_dimension (+ 1 Usure))) ; was + 1 (rand Usure)
        )
        
        ; créer une brosse
        (gimp-brush-new "brosse_grunge_stamp_gimp_2_8")
        
        
        ; créer et sélectionner brosse
        (gimp-context-set-brush "brosse_grunge_stamp_gimp_2_8")	
        (gimp-brush-set-shape "brosse_grunge_stamp_gimp_2_8" 0) ; forme ronde
        (gimp-brush-set-radius "brosse_grunge_stamp_gimp_2_8" 0.05) ; rayon
        (gimp-brush-set-spikes "brosse_grunge_stamp_gimp_2_8" 2) ; pointes
        (gimp-brush-set-hardness "brosse_grunge_stamp_gimp_2_8" 0.7) ; dureté
        (gimp-brush-set-aspect-ratio "brosse_grunge_stamp_gimp_2_8" 1) ; proportions
        (gimp-brush-set-angle "brosse_grunge_stamp_gimp_2_8" 0) ; angle
        (gimp-brush-set-spacing "brosse_grunge_stamp_gimp_2_8" 10) ; espacement
        
        
        ; ajout_dimension
        (set! ajout_dimension 1)
        
        ; boucle pour appliquer pencil
        (while (< ajout_dimension (+ width height)) ;
            
            ;(gimp-message "pencil")
            ; contenu des 4 valeurs de array
            (aset *array_points_brosse* 0 (rand width))
            (aset *array_points_brosse* 1 (rand height))
            (aset *array_points_brosse* 2 (rand width))
            (aset *array_points_brosse* 3 (rand height))
            
            
            (gimp-pencil 
                    masque_de_calque  ; drawable 
                    4 ; num-strokes 
                    *array_points_brosse* ; strokes
            )
            
            
            
            ; nouvelle valeur de ajout_dimension
            (set! ajout_dimension (+ ajout_dimension (+ 1 (rand (* 2 Usure)))))
            
        )
        
        ; supprimer la brosse
        (gimp-brush-delete "brosse_grunge_stamp_gimp_2_8")
        
      ;  ; appliquer et supprimer le masque
      ;  (gimp-layer-remove-mask calque_texte 0)
        
        
        ; aplatir
        (if (= Aplatir TRUE)
            (begin
                ; mettre AP à Couleur_fond
                (gimp-context-set-background Couleur_fond)				
                ; aplatir
                (gimp-image-flatten img)
            )
        )
        
        
        
        ;*******************************************************************************************
        
        
        
        ; restaurer PP et AP
        (gimp-context-set-foreground  old-fg)
        (gimp-context-set-background old-bg)
        
        ; restaurer brosse
        (gimp-context-set-brush old-brush)
        
        ; ne rien sélectionner
        (gimp-selection-none img)
        
        ; afficher l'image
        (gimp-display-new img)
        
        ;; End undo group.
        (gimp-image-undo-group-end img)
        (gc) ; memory garbage cleanup - an array was used
  )
    
)

(script-fu-register
    "Grunge_Stamp"
    "<Image>/File/Create/Logos/Grunge stamp..."
    "Un tampon avec inclinaison ajustable / A simple Grunge-Stamp!  \nfile:grunge_stamp_gimp_2_8.scm"
    "samj"
    "samj"
    "2010-12-15"
    ""
    SF-STRING "Texte / Text" "Gimp 2.8"
    SF-FONT "Police / Font" "Serif Bold" ; Serif Bold
    SF-COLOR "Couleur encre / Color ink" '(221 0 0) ; dd0000
    SF-ADJUSTMENT "Taille Police / Font Size [pixels]" '(72 12 480 1 10 0 1)
    SF-ADJUSTMENT "Angle [degres]" '(-15 -180 180 1 10 0 1)
    SF-ADJUSTMENT "Contour [pixels]" '(20 10 240 1 10 0 1) ; distance de la bordure
    SF-ADJUSTMENT "Bordure / Thickness [pixels]" '(5 2 240 1 10 0 1) ; épaisseur
    SF-ADJUSTMENT "Usure / Abrasion (higher is less)" '(40 1 100 1 10 0 1) ; usure du tampon
    SF-TOGGLE "Aplatir / Flatten" FALSE
    SF-COLOR "Couleur fond / Background color" '(255 255 255) ; ffffff
    SF-BRUSH "Brosse usure / Brush" '("Grass" 100 5 0) ; Brosse_usure
)

; FIN du script