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


# # Pour observer les différentes modalités de chaque variable dans les fichiers de chaque année, pour être sûr de prévoit toutes les modalités
# var <- "catv"
# for (j in seq_along(annee_fichiers)){
#   print(table(get(paste0("vehicules_",annee_fichiers[j]))[,c(var)]))
# }



# Préparation table CARACTERISTIQUES ====


# Liste des modalités pour chaque variable quali
lum_lab <- c(NA,"Plein jour","Crépuscule ou aube","Nuit sans éclairage public","Nuit avec éclairage public non allumé","Nuit avec éclairage public allumé")
int_lab <- c(NA,"Hors intersection", "Intersection en X","Intersection en T","Intersection en Y","Intersection à plus de 4 branches","Giratoire",
             "Place", "Passage à niveau","Autre intersection")
agg_lab <- c(NA,"Hors agglomération", "En agglomération")
atm_lab <- c(NA,"Normale","Pluie légère","Pluie forte","Neige - grêle","Brouillard - fumée","Vent fort - tempête","Temps éblouissant","Temps couvert","Autre")
col_lab <- c(NA,"Deux véhicules - frontale","Deux véhicules – par l’arrière","Deux véhicules – par le coté","Trois véhicules et plus – en chaîne",
             "Trois véhicules et plus - collisions multiples","Autre collision","Sans collision")


liste_var <- c("lum","int","agg","atm","col")


min(substr(str_pad(caracteristiques_2017$dep,width = 3,side = "left","0"),1,2))

# Boucle pour transformer toutes les variables quali en facteur
for (j in seq_along(annee_fichiers)){
  
  # Fichier temporaire pour transformer les variables
  assign(x = "fichier", value = get(paste0("caracteristiques_",annee_fichiers[j])))
  
  # Création des variables : date et heure,  code INSEE, departement, lat, long + exclusion des DOM + coord GPS à nulle si 0
  fichier <- 
    fichier %>%
    mutate(lat = as.numeric(lat),
           long = as.numeric(long)) %>% 
    mutate(date_heure = mdy_hms(
      paste0(
        paste(str_pad(fichier$mois,"0",width = 2, side = "left"),
              str_pad(fichier$jour,"0",width = 2, side = "left"),
              fichier$an,
              sep= "-"),
        " ",
        paste(substr(str_pad(fichier$hrmn,"0",width = 4, side = "left"),1,2),
              substr(str_pad(fichier$hrmn,"0",width = 4, side = "left"),3,4),
              "00",
              sep = ":"))),
      dep = substr(str_pad(fichier$dep,width = 3,side = "left","0"),1,2),
      code_INSEE = paste0(substr(str_pad(fichier$dep,width = 3,side = "left","0"),1,2),
                          str_pad(fichier$com,width = 3, side = "left", "0")),
      lat = ifelse(lat == 0 & long == 0, NA,as.numeric(lat) / 100000),
      long = ifelse(lat == 0 & long == 0, NA, as.numeric(long) / 100000),
      an = an + 2000) %>% 
    filter(dep != "97") %>% 
    select(-com, -gps)
  
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

rm(lum_lab,int_lab,agg_lab,atm_lab,col_lab)


# Préparation table LIEUX ====

# Liste des modalités pour chaque variable quali
# Ajout des levels quand la suite n'est pas continue
catr_lab <- c("Autoroute","Route Nationale","Route Départementale","Voie Communale","Hors réseau public",
              "Parc de stationnement ouvert à la circulation publique","Inconnu","Autre") # 9 pour autre
catr_lev <- c(1,2,3,4,5,6,7,9) # La modalité 7 appraît en 2018, mais pas d'explication sur ce qu'elle représente
circ_lab <- c(NA,"A sens unique","Bidirectionnelle","A chaussées séparées","Avec voies d’affectation variable")
vosp_lab <- c(NA,"Piste cyclable","Banque cyclable","Voie réservée") 
prof_lab <- c(NA,"Plat","Pente","Sommet de côte","Bas de côte")
plan_lab <- c(NA,"Partie rectiligne","En courbe à gauche","En courbe à droite","En « S »")
surf_lab <- c(NA,"normale","mouillée","flaques","inondée","enneigée","boue","verglacée","corps gras - huile","autre" )
infra_lab <- c(NA,"Souterrain - tunnel","Pont - autopont","Bretelle d’échangeur ou de raccordement","Voie ferrée",
               "Carrefour aménagé","Zone piétonne","Zone de péage")
situ_lab <- c(NA,"Sur chaussée","Sur bande d’arrêt d’urgence","Sur accotement","Sur trottoir","Sur piste cyclable")



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
                                                  levels = if (exists(x = paste0(var,"_lev")) == TRUE){ # si le fichier des niveaux existe...
                                                    get(paste0(var,"_lev")) # ... on retient ce fichier ...
                                                    }else{
                                                      c(seq(0,longueur-1,1)) # ... sinon on retient une séquence  
                                                    },
                                                  labels = get(paste0(var,"_lab"))))
    
  }
  # On réattribue le fichier temporaire au fichier d'origine
  assign(x = paste0("lieux_",annee_fichiers[j]), value = fichier)
}


rm(catr_lab,catr_lev,circ_lab,vosp_lab,prof_lab,plan_lab,surf_lab,infra_lab,situ_lab)




# Préparation table USAGERS ====

# Liste des modalités pour chaque variable quali
catu_lab <- c("Conducteur","Passager","Piéton","Piéton en roller ou trotinette") # A partir de 2018, trotinette renseignée dans véhicule et non pas ici
grav_lab <- c("Indemne","Tué","Blessé hospitalisé","Blessé léger")
grav_lev <- c(1,2,3,4)
sexe_lab <- c("Homme","Femme")
trajet_lab <- c(NA,"Domicile - Travail", "Domicile - école", "Courses - achats", "Utilisation professionnelle", "Promenade - loisirs", "Autres") # Autres = 9
trajet_lev <- c(0,1,2,3,4,5,9)
# secu sur 2 caractères donc à retravailler
secu1_lab <- c("Ceinture","Casque","Dispositif enfants","Equipement réfléchissant", "Autre") # Autres = 9
secu1_lev <- c(1,2,3,4,9)
secu2_lab <- c(NA,"Oui","Non","Non déterminable")
secu2_lev <- c("",1,2,3)
locp_lab <- c(NA,"A + de 50m du passage piéton","A - de 50m du passage piéton","Sans signalisation lumineuse", "Avec signalisation lumineuse","Sur trottoir","Sur accotement","Sur refuge ou BAU","Sur contre allée")
locp_lev <- c(0,1,2,3,4,5,6,7,8)

actp_lab <- c("Non renseigné ou sans objet", "Sens véhicule heurtant", "Sens inverse du véhicule", "Traversant","Masqué","Jouant - courant","Avec animal","Autre")
actp_lev <- c(0,1,2,3,4,5,6,9)
etatp_lab <- c(NA,"Seul","Accompagné","En groupe")



# Liste des variables pour lesquelles il faut ajouter les libellés
liste_var <- c("catu","grav","sexe","trajet","secu1","secu2","locp","actp","etatp")


# Boucle pour transformer toutes les variables quali en facteur
for (j in seq_along(annee_fichiers)){
  
  # Fichier temporaire pour transformer les variables
  assign(x = "fichier", value = get(paste0("usagers_",annee_fichiers[j])))
  
  # Secu est une variable sur 2 positions, chaque position avec une signification particulièrer
  # Création de deux variables distincte
  fichier <- fichier %>% mutate(secu1 = substr(secu,1,1),
                                secu2 = substr(secu,2,2),
                                age = as.integer(annee_fichiers[j]) - an_nais)
  
  for (var in liste_var){
    
    # Nombre d'étiquettes
    longueur <- length(get(paste0(var,"_lab")))
    
    # Transformation de la variable en facteur. Opérateur := permet d'assigner une valeur à un paramètre dont le nom est une variable. Plus d'infos sur : vignette("programming", "dplyr")
    # !! pour unquote une variable. Ici, la variable créée "var" prend la valeur définie avec la boucle.
    fichier <- fichier %>% mutate(!!var := factor(x = fichier[,var], 
                                                  levels = if (exists(x = paste0(var,"_lev")) == TRUE){ # si le fichier des niveaux existe...
                                                    get(paste0(var,"_lev")) # ... on retient ce fichier ...
                                                  }else{
                                                    c(seq(0,longueur-1,1)) # ... sinon on retient une séquence  
                                                  },
                                                  labels = get(paste0(var,"_lab"))))
    
  }
  # On réattribue le fichier temporaire au fichier d'origine
  assign(x = paste0("usagers_",annee_fichiers[j]), value = fichier)
}

rm(catu_lab,grav_lab,sexe_lab,trajet_lab,trajet_lev,secu1_lab,secu1_lev,secu2_lab,secu2_lev,locp_lab,locp_lev,actp_lab, actp_lev,etatp_lab)



# Préparation table VEHICULES ====

# Liste des modalités pour chaque variable quali
senc_lab <- c(NA,"PK ou PR ou numéro d’adresse postale croissant","PK ou PR ou numéro d’adresse postale décroissant")
# Penser à faire des regroupement. Des catégories sont dites inutiliées depuis 2006, à vérifier avant de les supprimer
catv_lab <- c("Bicyclette","Cyclomoteur <50cm3","Voiturette","Voiture","VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque",
              "PL seul 3,5T <PTCA <= 7,5T", "PL seul > 7,5T","PL > 3,5T + remorque 16 - Tracteur routier seul",
              "Tracteur routier + semi-remorque 18 - Référence plus utilisée depuis 2006 (transport en commun)",
              "tramway","Engin spécial","Tracteur agricole","Scooter < 50 cm3","Motocyclette > 50 cm3 et <= 125 cm3","Scooter > 50 cm3 et <= 125 cm3",
              "Motocyclette > 125 cm3","Scooter > 125 cm3","Quad léger <= 50 cm3","Quad lourd > 50 cm3","Autobus","Autocar","Train","Tramway",
              "Autre véhicule") # 99 pour autre véhicules
catv_lev <- c(1,2,3,7,10,13,14,15,16,17,20,21,30,31,32,33,34,35,36,37,38,39,40,99)
obs_lab <- c(NA,"Véhicule en stationnement","Arbre","Glissière métallique","Glissière en béton","Autre glissière"," Bâtiment, mur, pile de pont",
             "Support de signalisation verticale ou poste d’appel d’urgence","Poteau","Mobilier urbain","Parapet","Ilot, refuge, borne haut",
             "Bordure de trottoir","Fossé, talus, paroi rocheuse","Autre obstacle fixe sur chaussée","Autre obstacle fixe sur trottoir ou accotement",
             " Sortie de chaussée sans obstacle")
obsm_lab <- c(NA,"Piéton","Véhicule","Véhicule sur rail","Animal domestique","Animal sauvage","Autre") # Autre = 9
obsm_lev <- c(0,1,2,3,4,5,9)
choc_lab <- c(NA,"Avant","Avant droit","Avant gauche", "Arrière","Arrière droit","Arrière gauche", "Côté droit","Côté gauche","Chocs multiples (tonneaux)")
manv_lab <- c(NA,"Sans changement de direction","Même sens, même file","Entre 2 files","En marche arrière","A contresens",
              "En franchissant le terre-plein central","Dans le couloir bus, dans le même sens","Dans le couloir bus, dans le sens inverse",
              "En s’insérant","En faisant demi-tour sur la chaussée","Changeant de file à gauche","Changeant de file à droite","Déporté à gauche",
              "Déporté à droite","Tournant à gauche","Tournant à droite","Dépassant à gauche","Dépassant à droite","Traversant la chaussée",
              "Manœuvre de stationnement","Manœuvre d’évitement","Ouverture de porte","Arrêté (hors stationnement)","En stationnement (avec occupants)")



# Liste des variables pour lesquelles il faut ajouter les libellés
liste_var <- c("senc","catv","obs","obsm","choc","manv")

# Boucle pour transformer toutes les variables quali en facteur
for (j in seq_along(annee_fichiers)){
  
  # Fichier temporaire pour transformer les variables
  assign(x = "fichier", value = get(paste0("vehicules_",annee_fichiers[j])))

  for (var in liste_var){
    
    # Nombre d'étiquettes
    longueur <- length(get(paste0(var,"_lab")))
    
    # Transformation de la variable en facteur. Opérateur := permet d'assigner une valeur à un paramètre dont le nom est une variable. Plus d'infos sur : vignette("programming", "dplyr")
    # !! pour unquote une variable. Ici, la variable créée "var" prend la valeur définie avec la boucle.
    fichier <- fichier %>% mutate(!!var := factor(x = fichier[,var], 
                                                  levels = if (exists(x = paste0(var,"_lev")) == TRUE){ # si le fichier des niveaux existe...
                                                    get(paste0(var,"_lev")) # ... on retient ce fichier ...
                                                  }else{
                                                    c(seq(0,longueur-1,1)) # ... sinon on retient une séquence  
                                                  },
                                                  labels = get(paste0(var,"_lab"))))
    
  }
  # On réattribue le fichier temporaire au fichier d'origine
  assign(x = paste0("vehicules",annee_fichiers[j]), value = fichier)
}

rm(senc_lab,catv_lab,catv_lev,obs_lab,obsm_lab,obsm_lev,choc_lab,manv_lab)
rm(annee_fichiers,j,liste_var,longueur,type_fichiers,var,fichier)



# Fusion des jeux de données par année ====
caracteristiques <- rbind(caracteristiques_2015,caracteristiques_2016,caracteristiques_2017,caracteristiques_2018)
lieux <- rbind(lieux_2015,lieux_2016,lieux_2017,lieux_2018)
usagers <- rbind(usagers_2015,usagers_2016,usagers_2017,usagers_2018)
vehicules <- rbind(vehicules_2015,vehicules_2016,vehicules_2017,vehicules_2018)

# Final pour shinyapp ====
saveRDS(object = caracteristiques,file = "00_Inputs/caracteristiques.rds")
saveRDS(object = lieux,file = "00_Inputs/lieux.rds")
saveRDS(object = usagers,file = "00_Inputs/usagers.rds")
saveRDS(object = vehicules,file = "00_Inputs/vehicules.rds")