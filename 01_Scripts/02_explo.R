# A intégrer dans le script 
  # 1) Ordered factor pour gravité : Indemne < blessé leger < blesse hospi < tué
  # 2) Pourquoi des usagers sans accidents associés ? (pour l'année, on peut utiliser les 4 premiers numéros de l'id d'accident) >Trop de 0 dans l'id je pense

# Pour package
  # 1) Prévoir fonction pour axe en pourcent

# Tuto à prévoir
  # 1) Carto
  # 2) bar chart avec largeur proportionnelle aux effectifs
  # 3) bar chart 'fill' avec label au milieu de chaque catégorie

# Packages ====
library(dplyr)
library(ggplot2)
library(forcats)
library(lubridate)
library(jcan)

extrafont::loadfonts(device = "win")


# Data ====
# import des fichiers générés à partir du script 01_data_prep
usagers <- readRDS(file = "00_Inputs/usagers.rds")
vehicules <- readRDS(file = "00_Inputs/vehicules.rds")
caracteristiques <- readRDS(file = "00_Inputs/caracteristiques.rds")
lieux <- readRDS(file = "00_Inputs/lieux.rds")


# Nombre d'accidents = nombre de lignes dans la table caractéristiques 
n_distinct(unique(caracteristiques$Num_Acc)) == nrow(caracteristiques)

# Nombre d'accidents en fonction de la période de l'année
caracteristiques %>% group_by(an, mois = (month(date_heure,label = TRUE))) %>% summarize(nb_acc = n()) %>% 
ggplot(aes(x = mois, y = nb_acc, color = as.factor(an), group = an))+
  geom_line(size = 1.1)+
  geom_point(size = 1.5,shape = 21, fill = "white", stroke = 1.5)+
  theme_jcan()+
  labs(title = "Nombre d'accidents corporels par mois en France",
       subtitle = "Source : Base de données des accidents corporels de la circulation - data.gouv.fr",
       y = "Nombre d'accidents",
       x = NULL,
       caption ="@j_cantet")+
scale_color_jcan(name = "Année",labels = c("2015","2016","2017","2018"))


# Répartition des victimes par année
victimes <- usagers %>% left_join(caracteristiques %>% select(Num_Acc,an))

victimes %>% group_by(an,grav) %>% 
  summarize(nb_acc = n()) %>%
  ungroup() %>%
  group_by(an) %>% 
  mutate(perc_acc = nb_acc/sum(nb_acc)) %>% 
  arrange(an,desc(grav)) %>% 
  ggplot(aes(x=as.factor(an),y = nb_acc, fill = grav, group = an))+
  geom_col(position = "fill")+
  geom_text(aes(y = perc_acc,group = grav, label = paste0(scales::percent(perc_acc,accuracy = 0.1),
                                                          " - ",
                                                          format(nb_acc,big.mark = " "))),
            position = position_stack(vjust = .5),
            size = 3, color = "white")+
  theme_jcan()+
  scale_fill_jcan(name = "Gravité")+
  scale_x_discrete(labels = c(2015,2016,2017,2018), expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  labs(title = "Répartition des victimes selon la gravité de l'accident",
       subtitle = "Source : Base de données des accidents corporels de la circulation - data.gouv.fr",
       x = "Année",
       y = NULL,
       caption ="@j_cantet")+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())
  


