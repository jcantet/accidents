options(scipen = 999999)

# Packages ====
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# Data ====

# Type de fichiers
type_fichiers <- c("caracteristiques_","lieux_","usagers_","vehicules_")
annee_fichiers <- c("2015","2016","2017","2018")

# Boucle avec les années et type de fichiers pour importer tout d'un bloc
for (i in seq_along(type_fichiers)){
  for (j in seq_along(annee_fichiers)){
    assign(x = paste0(type_fichiers[i],annee_fichiers[j]),value = read.csv(paste0("00_Inputs/",type_fichiers[i],annee_fichiers[j],".csv"), stringsAsFactors = FALSE))
  }
}

rm(i,j) # Supprestion des variables temporaires



# Préparation table CARACTERISTIQUES ====
head(caracteristiques_2015)
# Création des variables : date et heure,  code INSEE, departement, lat, long + exclusion des DOM + coord GPS à nulle si 0
caracteristiques_2015 <- 
  caracteristiques_2015 %>%
  mutate(date_heure = mdy_hms(
           paste0(
             paste(str_pad(caracteristiques_2015$mois,"0",width = 2, side = "left"),
                   str_pad(caracteristiques_2015$jour,"0",width = 2, side = "left"),
                   caracteristiques_2015$an,
                   sep= "-"),
             " ",
             paste(substr(str_pad(caracteristiques_2015$hrmn,"0",width = 4, side = "left"),1,2),
                   substr(str_pad(caracteristiques_2015$hrmn,"0",width = 4, side = "left"),3,4),
                   "00",
                   sep = ":"))),
         departement = substr(dep,1,2),
         code_INSEE = paste0(substr(dep,1,2),com),
         lat = ifelse(lat == "0000000" & long == "0000000", NA,as.numeric(lat) / 100000),
         long = ifelse(lat == "0000000" & long == "0000000", NA, as.numeric(long) / 100000)) %>% 
  filter(departement != "97") %>% 
  select(-com, -gps)

# Liste des modalités pour chaque variable quali
lum_lab <- c("NA","Plein jour","Crépuscule ou aube","Nuit sans éclairage public","Nuit avec éclairage public non allumé","Nuit avec éclairage public allumé")
int_lab <- c("NA","Hors intersection", "Intersection en X","Intersection en T","Intersection en Y","Intersection à plus de 4 branches","Giratoire",
             "Place", "Passage à niveau","Autre intersection")
agg_lab <- c("NA","Hors agglomération", "En agglomération")
atm_lab <- c("NA","Normale","Pluie légère","Pluie forte","Neige - grêle","Brouillard - fumée","Vent fort - tempête","Temps éblouissant","Temps couvert","Autre")
col_lab <- c("NA","Deux véhicules - frontale","Deux véhicules – par l’arrière","Deux véhicules – par le coté","Trois véhicules et plus – en chaîne",
             "Trois véhicules et plus - collisions multiples","Autre collision","Sans collision")


liste_var <- c("lum","int","agg","atm","col")


# Boucle pour transformer toutes les variables quali en facteur
for (j in seq_along(annee_fichiers)){
  
  # Fichier temporaire pour transformer les variables
  assign(x = "fichier", value = get(paste0("caracteristiques_",annee_fichiers[j])))
  
  for (var in liste_var){
    
    # Nombre d'étiquettes
    longueur <- length(get(paste0(var,"_lab")))
    
    # Transformation de la variable en facteur. Opérateur := permet d'assigner une valeur à un paramètre dont le nom est une variable. Plus d'infos sur : vignette("programming", "dplyr")
    # !! pour unquote une variable. Ici, la variable créée "var" prend la valeur définie avec la boucle.
    fichier <- fichier %>% mutate(!!var := factor(x = fichier[,var], 
                                               levels = c(seq(0,longueur-1,1)),
                                               labels = get(paste0(var,"_lab"))))
  
  }
  # On réattribue le fichier temporaire au fichier d'origine
  assign(x = paste0("caracteristiques_",annee_fichiers[j]), value = fichier)
}



# Préparation table LIEUX ====

# Liste des modalités pour chaque variable quali
catr_lab <- c("NA","Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public",
              "Parc de stationnement ouvert à la circulation publique","autre") # 9 pour autre
circ_lab <- c("NA","A sens unique","Bidirectionnelle","A chaussées séparées","Avec voies d’affectation variable")
vosp_lab <- c("NA","Piste cyclable","Banque cyclable","Voie réservée") 
prof_lab <- c("NA","Plat","Pente","Sommet de côte","Bas de côte")
plan_lab <- c("NA","Partie rectiligne","En courbe à gauche","En courbe à droite","En « S »")
surf_lab <- c("NA","normale","mouillée","flaques","inondée","enneigée","boue","verglacée","corps gras - huile","autre" )
infra_lab <- c("NA","Souterrain - tunnel","Pont - autopont","Bretelle d’échangeur ou de raccordement","Voie ferrée",
               "Carrefour aménagé","Zone piétonne","Zone de péage")
situ_lab <- c("NA","Sur chaussée","Sur bande d’arrêt d’urgence","Sur accotement","Sur trottoir","Sur piste cyclable")


# Liste des variables pour lesquelles il faut ajouter les libellés
liste_var <- c("catr","circ","vosp","prof","plan","surf","infra","situ")


# Boucle pour transformer toutes les variables quali en facteur
for (j in seq_along(annee_fichiers)){
  
  # Fichier temporaire pour transformer les variables
  assign(x = "fichier", value = get(paste0("lieux_",annee_fichiers[j])))
  
  for (var in liste_var){
    
    # Nombre d'étiquettes
    longueur <- length(get(paste0(var,"_lab")))
    
    # Transformation de la variable en facteur. Opérateur := permet d'assigner une valeur à un paramètre dont le nom est une variable. Plus d'infos sur : vignette("programming", "dplyr")
    # !! pour unquote une variable. Ici, la variable créée "var" prend la valeur définie avec la boucle.
    fichier <- fichier %>% mutate(!!var := factor(x = fichier[,var], 
                                                  levels = c(seq(0,longueur-1,1)),
                                                  labels = get(paste0(var,"_lab"))))
    
  }
  # On réattribue le fichier temporaire au fichier d'origine
  assign(x = paste0("lieux_",annee_fichiers[j]), value = fichier)
}


# Préparation table USAGERS ====

# Liste des modalités pour chaque variable quali
catu_lab <- c("Conducteur","Passager","Piéton","Piéton en roller ou trotinette") # A partir de 2018, trotinette renseignée dans véhicule et non pas ici
grav_lab <- c("Indemne","Tué","Blessé hospitalisé","Blessé léger")
sexe_lab <- c("Homme","Femme")
trajet_lab <- c("Domicile - Travail", "Domicile - école", "Courses - achats", "Utilisation professionnelle", "Promenade - loisirs", "Autres") # Autres = 9
# secu sur 2 caractères donc à retravailler
secu1_lab <- c("Ceinture","Casque","Dispositif enfants","Equipement réfléchissant", "Autre") # Autres = 9
secu2_lab <- c("Oui","Non","Non déterminable")
locp <- c("A + de 50m du passage piéton","A - de 50m du passage piéton","Sans signalisation lumineuse", "Avec signalisation lumineuse","Sur trottoir","Sur accotement","Sur refuge ou BAU","Sur contre allée")
actp <- c("Non renseigné ou sans objet", "Sens véhicule heurtant", "Sens inverse du véhicule", "Traversant","Masqué","Jouant - courant","Avec animal","Autre")
etatp <- c("Seul","Accompagné","En groupe")


# Préparation table VEHICULES ====

# Liste des modalités pour chaque variable quali
sens_lab <- c("PK ou PR ou numéro d’adresse postale croissant","PK ou PR ou numéro d’adresse postale décroissant")
# Penser à faire des regroupement. Des catégories sont dites inutiliées depuis 2006, à vérifier avant de les supprimer
catv_lab <- c("Bicyclette","Cyclomoteur <50cm3","Voiturette","Scooter immatriculé","Motocyclette","Side-car","VL seul","VL + caravane",
              "VL + remorque","VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque","VU (10) + caravane)","VU (10) + remorque)",
              "PL seul 3,5T <PTCA <= 7,5T 14 - PL seul > 7,5T 15 - PL > 3,5T + remorque 16 - Tracteur routier seul",
              "Tracteur routier + semi-remorque 18 - Référence plus utilisée depuis 2006 (transport en commun)",
              "tramway","Engin spécial","Tracteur agricole","Scooter < 50 cm3","Motocyclette > 50 cm3 et <= 125 cm3","Scooter > 50 cm3 et <= 125 cm3",
              "Motocyclette > 125 cm3","Scooter > 125 cm3","Quad léger <= 50 cm3","Quad lourd > 50 cm3","Autobus","Autocar","Train","Tramway",
              "Autre véhicule") # 99 pour autre véhicules
obs_lab <- c("Véhicule en stationnement","Arbre","Glissière métallique","Glissière en béton","Autre glissière"," Bâtiment, mur, pile de pont",
             "Support de signalisation verticale ou poste d’appel d’urgence","Poteau","Mobilier urbain","Parapet","Ilot, refuge, borne haut",
             "Bordure de trottoir","Fossé, talus, paroi rocheuse","Autre obstacle fixe sur chaussée","Autre obstacle fixe sur trottoir ou accotement",
             " Sortie de chaussée sans obstacle")
obsm_lab <- c("Piéton","Véhicule","Véhicule sur rail","Animal domestique","Animal sauvage","Autre") # Autre = 9
choc_lab <- c("Avant","Avant droit","Avant gauche", "Arrière","Arrière droit","Arrière gauche", "Côté droit","Côté gauche","Chocs multiples (tonneaux)")
manv_lab <- c("Sans changement de direction","Même sens, même file","Entre 2 files","En marche arrière","A contresens",
              "En franchissant le terre-plein central","Dans le couloir bus, dans le même sens","Dans le couloir bus, dans le sens inverse",
              "En s’insérant","En faisant demi-tour sur la chaussée","Changeant de file à gauche","Changeant de file à droite","Déporté à gauche",
              "Déporté à droite","Tournant à gauche","Tournant à droite","Dépassant à gauche","Dépassant à droite","Traversant la chaussée",
              "Manœuvre de stationnement","Manœuvre d’évitement","Ouverture de porte","Arrêté (hors stationnement)","En stationnement (avec occupants)")


# Final pour shinyapp ====

#saveRDS()