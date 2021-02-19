; Début du script-fu Colorful_Light_Effect_gimp_2_10.scm 
;
; Accès par :   Fichier > Créer > Logos > Colorful Light Effect...
;               File > Create > Logos > Colorful Light Effect... 
;
;                __________________________________________________________
; 
;
; Script-fu créé à partir du didacticiel : Colorful Light Effect in GIMP
;                                          http://abduzeedo.com/reader-tutorial-colorful-light-effect-gimp
;                       About the Author : Ricardo Francés here, a freelance web programmer/designer from El Salvador with a passion for open source software. 
;
;
;
;
;
; Licence GNU/GPL
;
; --------------------------------------------------------------------
; version 1.0 par samj (www.aljacom.com/~gimp) 2010/12/23
; version 1.01 par samj (www.aljacom.com/~gimp) 2010/12/23 ajout motif_a_appliquer
; version 1.02 par samj (www.aljacom.com/~gimp) 2010/12/24 modification set! espacement_texte
; version 2.0 par samj (www.aljacom.com/~gimp) 2010/12/24 version pour gimp 2.7.2
; version 2.1 par samj (www.aljacom.com/~gimp) 2011/05/17 version pour gimp 2.7.3
;
; version 2.2 par karlhof26 2020/10/21 version pour GIMP-2.10.22
; --------------------------------------------------------------------
;
;

(define (Colorful_Light_Effect
                Largeur_fond
                Hauteur_fond
                Positionnement_ligne_lumineuse
                Texte
                Police
                Taille_Police
                espacement_texte
                flou_texte
                Opacite_Clouds
                motif_a_appliquer
                degrade
                
        )
        
        ; espacement_texte en pixels (à appliquer par rapport à la mi-hauteur)
        (set! espacement_texte (* (/ espacement_texte 100) (* (/ (- 100 Positionnement_ligne_lumineuse) 100) Hauteur_fond)) )
        ; (gimp-message (number->string espacement_texte))
        
        
    (let*
        (
            ; affectation des variables			
            
            (message (car (gimp-message-get-handler)))
            ; mémoriser les couleurs PP et AP
            (old-fg (car (gimp-context-get-foreground)))
            (old-bg (car (gimp-context-get-background)))
            
            ; créer une nouvelle image rgb
            (img (car (gimp-image-new Largeur_fond Hauteur_fond 0)))
            
            
            ; caractéristiques de la surface occupée par le texte
            (fond_texte (gimp-text-get-extents-fontname Texte Taille_Police 0 Police))
            
            ; largeur du texte
            (width_texte (car fond_texte))
            
            ; hauteur du texte
            (height_texte (cadr fond_texte))
            
            ;calque_fond
            (calque_fond)
            
            ;Clouds
            (Clouds)
            
            ; 4 calques Line_Layer
            (Line_Layer)
            (Line_Layer_1)
            (Line_Layer_2)
            (Line_Layer_3)
            
            ; ScanLines
            (ScanLines)
            
            ; motif
            (motif (car (gimp-context-get-pattern)))
            
            ; mémoriser le dégradé actif
            (old_gradient (car (gimp-context-get-gradient)))
            
            ; Colorful
            (Colorful)
            ; calque_texte
            (calque_texte)
            
            ; continue
            (continue 1)
            
        )
        
        ;; Start undo group.
        (gimp-image-undo-group-start img)
        
        ;; sélectionner la console erreurs pour envoyer un message
        (gimp-message-set-handler 2)	
        
        
        ; vérification hauteur disponible pour le texte et ajustement hauteur police******************
        ;	(gimp-message (number->string height_texte))
        ;	(gimp-message (number->string (- (* Hauteur_fond (/ (- 100 Positionnement_ligne_lumineuse) 100)) (* 2 espacement_texte))   ))
        
        (if 
            (< (- (* Hauteur_fond (/ (- 100 Positionnement_ligne_lumineuse) 100)) (* 2 espacement_texte))  height_texte)
                (begin
                    ; boucle pour ajuster la taille de la police
                    (while (= continue 1)
                        (if (> height_texte (- (* Hauteur_fond (/ (- 100 Positionnement_ligne_lumineuse) 100)) (* 2 espacement_texte))) 
                            (begin
                                (set! Taille_Police (- Taille_Police 1))
                                (set! fond_texte (gimp-text-get-extents-fontname Texte Taille_Police 0 Police))
                                (set! width_texte (car fond_texte))
                                (set! height_texte (cadr fond_texte))
                                (set! continue 1)
                            )
                            ;else
                                (set! continue 0)
                        )	
                        ; définir minimum police
                        (if (> 8 Taille_Police) 
                            (begin
                                (set! continue 0)
                            )
                        )	
                    )
                    
                    ; message d'alerte
                    (gimp-message "La hauteur de la police du texte est trop grande, elle sera diminuee")
                    ;(gimp-message (number->string Taille_Police))
                )
        )
        
        (set! continue 1)
        
        ; vérification largeur disponible pour le texte et ajustement hauteur police******************	
        ; (gimp-message (number->string width_texte))
        (if (< Largeur_fond  (* 1.2 width_texte))
            (begin
                ; boucle pour ajuster la taille de la police
                (while (= continue 1)
                    (if (> (* 1.2 width_texte) Largeur_fond)
                        (begin
                            (set! Taille_Police (- Taille_Police 1))
                            (set! fond_texte (gimp-text-get-extents-fontname Texte Taille_Police 0 Police))
                            (set! width_texte (car fond_texte))
                            (set! height_texte (cadr fond_texte))
                            (set! continue 1)
                        )
                        ;else
                        (set! continue 0)
                    )
                    ; définir minimum police
                    (if (> 8 Taille_Police) 
                        (begin
                            (set! continue 0)
                        )
                    )	
                )
                
                ; message d'alerte
                (gimp-message "La police du texte est trop grande, elle sera diminuee")
                ;(gimp-message (number->string Taille_Police))
            )
        )
        
        ; calque_fond********************************************************************************
        
        ; créer calque_fond
        (set! calque_fond (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "calque_fond" 100 0)))	
        
        ; ajouter le calque calque_fond
        (gimp-image-insert-layer img calque_fond -1 0)
        
        ; choisir noir pp
        (gimp-context-set-foreground  '(0 0 0))
        
        ; remplir de noir
        (gimp-drawable-fill calque_fond 0)
        
        ; Clouds***********************************************************************************
        
        ; créer Clouds
        (set! Clouds (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Clouds" 100 0)))	
        
        ; ajouter le calque Clouds
        (gimp-image-insert-layer img Clouds -1 0)
        
        ; appliquer greffon solid noise
        (plug-in-solid-noise 
                1 ; run-mode 
                img ; image 
                Clouds ; drawable 
                0 ; tilable 
                0 ; turbulent 
                140087409 ; seed 
                1 ; detail 
                6.0 ; xsize 
                6.0 ; ysize
        )
        
        ; appliquer motion blur
        (plug-in-mblur 
                1 ; run-mode 
                img ; image 
                Clouds ; drawable 
                0 ; type 
                256 ; length 
                90 ; angle 
                0 ; center-x 
                0 ; center-y
        )
        
        
        ; mettre opacité à Opacite_Clouds
        (gimp-layer-set-opacity Clouds Opacite_Clouds)
        
        
        ;ScanLines***********************************************************************************
        
        ; créer ScanLines
        (set! ScanLines (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "ScanLines" 100 0)))
        
        ; ajouter le calque ScanLines
        (gimp-image-insert-layer img ScanLines -1 0)	
        
        ; tout sélectionner
        (gimp-selection-all img)
        
        ; valider le motif motif_a_appliquer : Stripes Fine
        (gimp-context-set-pattern motif_a_appliquer)
        
        ; remplir le calque ScanLines du motif Stripes Fine
        (gimp-edit-bucket-fill 
                ScanLines ; drawable 
                2 ; fill-mode 
                0 ; paint-mode 
                100 ; opacity 
                0 ; threshold 
                TRUE ; sample-merged 
                0 ; x 
                0 ; y
        )
        
        ; ne rien sélectionner
        (gimp-selection-none img)
        
        ; calques (Line_Layer)	(Line_Layer_1) (Line_Layer_2) (Line_Layer_3) *************************
        
        ; créer Line_Layer
        (set! Line_Layer (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Line_Layer" 100 0)))
        
        ; ajouter le calque Line_Layer
        (gimp-image-insert-layer img Line_Layer -1 0)
        
        ; créer Line_Layer_1
        (set! Line_Layer_1 (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Line_Layer_1" 100 0)))
        
        ; ajouter le calque Line_Layer_1
        (gimp-image-insert-layer img Line_Layer_1 -1 0)
        
        ; créer Line_Layer_2
        (set! Line_Layer_2 (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Line_Layer_2" 100 0)))
        
        ; ajouter le calque Line_Layer_2
        (gimp-image-insert-layer img Line_Layer_2 -1 0)
        
        ; créer Line_Layer_3
        (set! Line_Layer_3 (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Line_Layer_3" 100 0)))	
        
        ; ajouter le calque Line_Layer_3
        (gimp-image-insert-layer img Line_Layer_3 -1 0)
        
        ; créer un sélection rectangulaire de 10 pixels de haut
        ;(gimp-rect-select image x y width height operation feather feather-radius)
        ;(gimp-rect-select img 0 (- (* (/ Positionnement_ligne_lumineuse 100) Hauteur_fond) 5) Largeur_fond 10 0 FALSE 0)
        ; modifications gimp 2.7.3
        ;(gimp-context-set-antialias TRUE)
        (gimp-context-set-feather FALSE)
        ; (gimp-context-set-feather-radius feather-radius-x feather-radius-y)
        (gimp-context-set-feather-radius 0 0)
        ;(gimp-image-select-rectangle image operation x y width height)
        (gimp-image-select-rectangle img 0 0 (- (* (/ Positionnement_ligne_lumineuse 100) Hauteur_fond) 5) Largeur_fond 10)
        
        ; choisir #cccccc pp
        (gimp-context-set-foreground  '(204 204 204))
        
        ; remplir le calque Line_Layer de couleur PP
        (gimp-edit-bucket-fill 
                Line_Layer ; drawable 
                0 ; fill-mode 
                0 ; paint-mode 
                100 ; opacity 
                0 ; threshold 
                TRUE ; sample-merged 
                0 ; x 
                0 ; y
        )
        
        ; remplir le calque Line_Layer_1 de couleur PP
        (gimp-edit-bucket-fill 
                Line_Layer_1 ; drawable 
                0 ; fill-mode 
                0 ; paint-mode 
                100 ; opacity 
                0 ; threshold 
                TRUE ; sample-merged 
                0 ; x 
                0 ; y
        )
        
        ; remplir le calque Line_Layer_2 de couleur PP
        (gimp-edit-bucket-fill 
                Line_Layer_2 ; drawable 
                0 ; fill-mode 
                0 ; paint-mode 
                100 ; opacity 
                0 ; threshold 
                TRUE ; sample-merged 
                0 ; x 
                0 ; y
        )
        
        ; remplir le calque Line_Layer_3 de couleur PP
        (gimp-edit-bucket-fill 
                Line_Layer_3 ; drawable 
                0 ; fill-mode 
                0 ; paint-mode 
                100 ; opacity 
                0 ; threshold 
                TRUE ; sample-merged 
                0 ; x 
                0 ; y
        )
        
        ; ne rien sélectionner
        (gimp-selection-none img)
        
        ; appliquer flou gaussien de rayon 70 sur le calque Line_Layer 
        (plug-in-gauss 1 img Line_Layer 70 70 1)
        
        ; appliquer flou gaussien de rayon 30 sur le calque Line_Layer_1
        (plug-in-gauss 1 img Line_Layer_1 30 30 1)	
        
        ; appliquer flou gaussien de rayon 10 sur le calque Line_Layer_2
        (plug-in-gauss 1 img Line_Layer_2 10 10 1)	
        
        ; calque Texte *******************************************************************************		
        
        ; choisir #cccccc pp
        (gimp-context-set-foreground  '(204 204 204))
        
        ; créer le calque texte
        ;                  (gimp-text-fontname image drawable x y text border antialias size size-type fontname)
        ; (+ (* Hauteur_fond (/ Positionnement_ligne_lumineuse 100)) espacement_texte)
        ; (- Largeur_fond (* 1.2 width_texte))
        (set! calque_texte 
            (car (gimp-text-fontname 
                    img
                    -1
                    (- Largeur_fond (* 1.2 width_texte))
                    (+ (* Hauteur_fond (/ Positionnement_ligne_lumineuse 100)) espacement_texte)
                    Texte
                    0 
                    TRUE 
                    Taille_Police 
                    0 
                    Police
                )
            )
        )
        
        ; donner un nom au calque
        (gimp-item-set-name calque_texte "Texte")	
        
        ; appliquer flou gaussien de rayon 10 sur le calque Texte
        (plug-in-gauss 1 img calque_texte flou_texte flou_texte 1)	
        
        ;Colorful************************************************************************************	
        
        ; créer Colorful
        (set! Colorful (car (gimp-layer-new img Largeur_fond Hauteur_fond 1 "Colorful" 100 0)))	
        
        ; ajouter le calque ScanLines
        (gimp-image-insert-layer img Colorful -1 0)		
        
        ; sélectionner dégradé Full saturation spectrum CW
        (gimp-context-set-gradient degrade)	
        
        ; appliquer un dégradé sur calque Colorful
        (gimp-edit-blend 
                    Colorful ; drawable
                    3 ; blend-mode      BLEND-CUSTOM (3) 
                    0 ; paint-mode
                    0 ; gradient-type   GRADIENT-LINEAR (0)
                    100 ; opacity
                    0 ; offset
                    0 ; repeat
                    FALSE ; reverse
                    TRUE ; supersample
                    3 ; max-depth
                    0.2 ; threshold
                    FALSE ; dither
                    0 ; x1 
                    (/ Hauteur_fond 2) ; y1 
                    Largeur_fond ; x2
                    (/ Hauteur_fond 2) ; y2
        )
        
        ; mettre le calque Colorful en mode color
        (gimp-layer-set-mode Colorful 13)
        
        ;********************************************************************************************
        
        
        
        ;; rétablir le mode message
        (gimp-message-set-handler message)
        
        ; restaurer PP et AP
        (gimp-context-set-foreground  old-fg)
        (gimp-context-set-background old-bg)
        
        ; restaurer ancien dégradé
        (gimp-context-set-gradient old_gradient)
        
        ; rétablir motif
        (gimp-context-set-pattern motif)
        
        ; ne rien sélectionner
        (gimp-selection-none img)
        
        ; afficher l'image
        (gimp-display-new img)
        
        ;; End undo group.
        (gimp-image-undo-group-end img)	
        
    )
    
)

(script-fu-register "Colorful_Light_Effect"
    "<Image>/File/Create/Logos/Colorful Light Effect..."
    "Texte et effet lumineux colore (priorite aux dimensions image) / Text and colorful Light Effect (Prioritising image dimensions)...\nfile:Colorful_Light_Effect_gimp_2_10.scm "
    "samj"
    "samj"
    "2010-12-23"
    ""
    SF-ADJUSTMENT "Largeur fond / Background width [pixels] "  '(1600 100 4000 1 10 0 0)
    SF-ADJUSTMENT "Hauteur fond / Background height [pixels] "  '(1200 100 4000 1 10 0 0)
    SF-ADJUSTMENT "Positionnement ligne lumineuse / Positioning light line [%] "  '(50 10 90 1 10 0 0)
    SF-STRING "Texte / Text " "Gimp 2.10.22"
    SF-FONT "Police / Font " "Serif Bold" ; Serif Bold
    SF-ADJUSTMENT "Taille Police / Font Size [pixels] " '(64 12 480 1 10 0 1)
    SF-ADJUSTMENT "Espacement texte / Spacing text [% Pos. line] " '(40 10 90 1 10 0 0)
    SF-ADJUSTMENT "Flou texte / Blur text " '(5 1 30 1 10 0 0)
    SF-ADJUSTMENT "Opacite calque Clouds / Opacity layer Clouds " '(20 15 30 1 10 0 0)
    SF-PATTERN "Motif transparent / Stripes pattern " "Stripes Fine"
    SF-GRADIENT "Degrade calque Colorful / Gradient layer Colorful " "Full saturation spectrum CW"
    
)

; FIN du script