# setwd("D:/Documents/2019-Private-sauve_dec24/Etudes/DONNEES/Browse")
# bridge<-read.csv("data originales/bridge_geraldine.csv",sep=";",dec=".", header=T)
# browse<-read.csv("new/Releves_complet_bota_2023.csv",sep=";",dec=".", header=T)
# data_Rens<- read.csv ("new/Trait_data_Rens-crh.csv",sep=";",dec=".", header=T)
# helio<-read.csv("new/Heliophiles.csv",sep=";",dec=".", header=T)

# GD (just to adapt it to my folders)
bridge<-read.csv("data/bridge_geraldine.csv",sep=";",dec=".", header=T)
browse<-read.csv("data/Releves_complet_bota_2023.csv",sep=";",dec=".", header=T) # raw data per indiv
data_Rens<- read.csv ("data/Trait_data_Rens-crh.csv",sep=";",dec=".", header=T) # traits (une ligne par indiv et trait)
helio<-read.csv("data/Heliophiles.csv",sep=";",dec=".", header=T) # une ligne par sp (à peu prêt...)

library(tidyverse)
library(dplyr)
library(rstatix)
library(ggpubr)
library(lme4)
library(nlme)
library(MASS)
library(conflicted)
library(ggplot2)
library(xlsx)
library(summarytools)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################""# PREPARATION DATA ##################


bridge$gen_sp<-paste(bridge$Genus,bridge$species)
browse$gen_sp<-paste(browse$Genre,browse$Espece)

helio2<-select(helio,gen_sp,Helio) # 3 catégories 
browse[is.na(browse$fiab_genet),"fiab_genet"]<-0

#### calculs traits moyens par espece selon data Bridge
df<-as_tibble(bridge)
head (df)
traits_bridge<-df%>%
  group_by(gen_sp) %>%
  summarise (across(c(thickness,SPAD,toughness,dry_mass, sapwood_dens),mean))
colnames(traits_bridge)[2]<-"thick_B"  
colnames(traits_bridge)[3]<-"SPAD_B"
colnames(traits_bridge)[4]<-"tough_B"
colnames(traits_bridge)[5]<-"dry_mass_B"
colnames(traits_bridge)[6]<-"sapwood_dens_B"

#### calculs traits moyens par espece selon data Rens 
df2<-as_tibble(data_Rens)
head (df2)
traits_Rens<-df2%>%
  group_by(Species,Variable) %>%
  summarise (moyenneR=mean(Value))

traits_Rens<-traits_Rens %>%
  spread(key="Variable",moyenneR)
colnames(traits_Rens)[1]<-"gen_sp"
colnames(traits_Rens)[2]<-"leaf_N_R"
colnames(traits_Rens)[3]<-"thick_R"
colnames(traits_Rens)[4]<-"tough_R"
colnames(traits_Rens)[5]<-"SLA_R"

traits_R_sd<-df2%>%
  group_by(Species,Variable) %>%
  summarise (sd=sd(Value))

traits_R_sd<-traits_R_sd %>%
  spread(key="Variable",sd)
colnames(traits_R_sd)[1]<-"gen_sp"
colnames(traits_R_sd)[2]<-"sd_leaf_N_R"
colnames(traits_R_sd)[3]<-"sd_thick_R"
colnames(traits_R_sd)[4]<-"sd_tough_R"
colnames(traits_R_sd)[5]<-"sd_SLA_R"

traits_Rens<-left_join(traits_Rens,traits_R_sd)

### traits moyens par espece selon mesures browse: uniquement avec genet 1

df3<-filter(browse,fiab_genet<2)
df3  <-filter (df3,Espece != "")
head (df3) 
df3$Ep<-as.numeric(df3$Ep)
traits_browse<-df3%>%
  group_by(gen_sp) %>%
  summarise (
    moyenne_Dur=mean(Dur_Fmat,na.rm = TRUE),
    SD_Dur=sd(Dur_Fmat,na.rm = TRUE),
    moyenne_Ep=mean(Ep, na.rm = TRUE),
    SD_Ep=sd(Ep,na.rm = TRUE),
    n=n()
  )



#### ajoute traits moyens (Bridge et Rens et browe et helio) par espece a chaque echantillon
browse_B<-left_join(browse,traits_bridge)
browse_BR<-left_join(browse_B, traits_Rens) 
browse_BR<-left_join(browse_BR,helio2)
browse_BBR<-left_join(browse_BR, traits_browse) # a utiliser pour donnees terrain Browse
browse_BBR$Helio[browse_BBR$gen_sp == "Pourouma "] <- "H_NP"


## corrections erreurs dans comptage des plantules par chablis
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH1"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH1"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH2"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH2"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH3"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH3"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH4"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH4"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH5"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH5"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH6"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH6"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH7"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH7"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH8"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH8"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH9"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH9"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="CH10"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="CH10"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT1"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT1"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT2"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT2"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT3"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT3"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT4"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT4"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT5"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT5"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT6"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT6"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT7"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT7"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT8"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT8"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT9"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT9"])
browse_BBR$Nbr_plantules_emplcmt[browse_BBR$Emplacement=="PT10"] <-length(browse_BBR$Emplacement[browse_BBR$Emplacement=="PT10"])

browse_BBR$Brt<-as.factor(browse_BBR$Brt)
browse_BBR$Brt_pas2<-as.factor(browse_BBR$Brt_pas2)
browse_BBR$Brt_pas2[browse_BBR$Brt_pas2==""]<-NA
browse_BBR$Brt_pas2<-droplevels(browse_BBR$Brt_pas2)



## broute au premier ou au 2eme passage = Brtot
browse_BBR$Brtot<-NA
browse_BBR$Brtot[browse_BBR$Brt=="O" | browse_BBR$Brt_pas2=="O"] <-"O"
browse_BBR$Brtot[browse_BBR$Brt=="N" & browse_BBR$Brt_pas2=="N"] <-"N"
browse_BBR$Brtot[browse_BBR$Brt=="N" & is.na(browse_BBR$Brt_pas2)] <-"N"
browse_BBR$Brtot<-as.factor(browse_BBR$Brtot)

## Emplacement 
# QUESTION pour Cécile: 
  # ça ne marche pas car j'ai pas de data qui s'appelle browse_fin 
  # je fais tourner les lignes suivantes avant
Emplacement<-data.frame(unique(browse_fin$Emplacement))
colnames(Emplacement)<-"Emplacement"
Emplacement$type<-substr(Emplacement$Emplacement,1,2)
Emplacement$nbplant<-unique(browse_fin$Nbr_plantules_emplcmt)


#### si pas de mesure de traits par Rens, utilise traits moyens mesures par nous
# QUESTION pour Cécile: 
  # ca veut dire que si il y a une mesure de traits chez Rens, tu privilégie la mesure de Rens et pas celle de browse?
  # pourquoi? j'aurai gardé plutôt celle de browse car sur les mêmes indivs...
  # on en discute si tu veux
browse_fin<-browse_BBR
browse_fin <- browse_fin %>%
  mutate(thick  = ifelse(is.na(thick_R), moyenne_Ep, thick_R),
         tough=ifelse(is.na(tough_R), moyenne_Dur, tough_R))

#### paires de placettes: CH1-PT1..., CH2-PT2
browse_fin$paire<-NA
browse_fin$paire[browse_fin$Emplacement=="CH1" | browse_fin$Emplacement=="PT1"] <-"P1"
browse_fin$paire[browse_fin$Emplacement=="CH2" | browse_fin$Emplacement=="PT2"] <-"P2"
browse_fin$paire[browse_fin$Emplacement=="CH3" | browse_fin$Emplacement=="PT3"] <-"P3"
browse_fin$paire[browse_fin$Emplacement=="CH4" | browse_fin$Emplacement=="PT4"] <-"P4"
browse_fin$paire[browse_fin$Emplacement=="CH5" | browse_fin$Emplacement=="PT5"] <-"P5"
browse_fin$paire[browse_fin$Emplacement=="CH6" | browse_fin$Emplacement=="PT6"] <-"P6"
browse_fin$paire[browse_fin$Emplacement=="CH7" | browse_fin$Emplacement=="PT7"] <-"P7"
browse_fin$paire[browse_fin$Emplacement=="CH8" | browse_fin$Emplacement=="PT8"] <-"P8"
browse_fin$paire[browse_fin$Emplacement=="CH9" | browse_fin$Emplacement=="PT9"] <-"P9"
browse_fin$paire[browse_fin$Emplacement=="CH10" | browse_fin$Emplacement=="PT10"] <-"P10"


#### nettoye: pour calculs avec traits moy par espece : donnees genet fiabilite 1 uniquement: 
browse_fin_net<-browse_fin[browse_fin$fiab_genet<2,] # a utiliser pour analyse sur donnees specifiques
browse_fin_net

## donnees que en chablis
browse_fin_CH<-browse_fin[browse_fin$Type_placette=="CH",]
browse_fin_net_CH<-browse_fin_net[browse_fin_net$Type_placette=="CH",]

## données pour H<150
browse_fin_H<-browse_fin[browse_fin$H<151,]
browse_fin_net_H<-browse_fin_H[browse_fin_H$fiab_genet<2,]



# Fichier traits specifiques = moyennes des traits selon gen_sp, venant de plusieurs BDD (Browse,Rens et Bridge)

traits<-data.frame(table(browse_fin_net$gen_sp))
colnames (traits) [1] <- "gen_sp"
traits<-left_join(traits, helio2)
traits<-left_join(traits,traits_browse)
traits<-left_join(traits,traits_Rens)
traits<-left_join(traits,traits_bridge)
row.names(traits)<-traits$gen_sp

### traits moyens sur plantules brt ou non , uniquement CH: pour compararer meme sp en ch si brt ou pas (reaction au broutage)
traits_react<-browse_fin_net_CH%>%group_by(gen_sp,Brt)
react_thick<-traits_react%>%summarise(Dur_moy=mean(Dur_Fmat, na.rm=T),n=n(), sd_dur=sd(Dur_Fmat,na.rm=T))
react_tough<-traits_react%>%summarise(Ep_moy=mean(Ep, na.rm=T),n=n(), sd_Ep=sd(Ep,na.rm=T))
react_thick<-as.data.frame(react_thick)
react_tough<-as.data.frame(react_tough)
traits_react_tot<-left_join(react_thick,react_tough)
traits_react_fin<- subset(traits_react_tot, n>5)

# write.xlsx(traits_react_fin, "traits_react_fin.xlsx")
write.csv(traits_react_fin, "data_processed/traits_react_fin.csv")



### traits moyens pour plantules en chablis/PT : meme sp : comparer effet du type de placette sur traits, pour une meme espece
traits_plot<-browse_fin_net%>% group_by(gen_sp,Type_placette)
plot_dur<-traits_plot%>%summarise(Dur_moy=mean(Dur_Fmat, na.rm=T), n=n(), SD_Dur=sd(Dur_Fmat,na.rm=T))
# plot_Ep<-traits_plot2%>%summarise(Ep_moy=mean(Ep, na.rm=T), n=n(), SD_Ep=sd(Ep,na.rm=T))
plot_Ep<-traits_plot%>%summarise(Ep_moy=mean(Ep, na.rm=T), n=n(), SD_Ep=sd(Ep,na.rm=T))
plot_dur<-as.data.frame(plot_dur)
plot_Ep<-as.data.frame(plot_Ep)
traits_plots_tot<-left_join(plot_dur,plot_Ep)
traits_plots_fin<-subset(traits_plots_tot,traits_plots_tot$n>5)

# write.xlsx(traits_plots_fin, "traits_plots_fin.xlsx")
write.csv(traits_plots_fin, "data_processed/traits_plots_fin.csv")


## broutée au moins 1 fois = espèce pouvant etre consommée "conso", >5% des fois : "RA" (regime alim), > 10% : Préferere)

consomme<-as.data.frame.matrix(table(browse_fin_net$gen_sp,browse_fin_net$Brtot))
consomme
str(consomme)
colnames(consomme)<- c("nb_nonconso","nb_conso")
consomme$gen_sp<-row.names(consomme)

traits<-left_join(traits,consomme)
traits$conso[traits$nb_conso>0]<-"O"
traits$conso[traits$nb_conso==0]<-"N"

pourcent<-100*traits$nb_conso/traits$Freq
traits$pourcent<-pourcent
traits$RA[traits$pourcent>=5]<-"O"  
traits$RA[traits$pourcent<5] <-"N"
traits$RA[traits$Freq<10] <- NA

traits$Pref[traits$pourcent>9]<-"O"
traits$Pref[is.na(traits$Pref)]<-"N"
traits$Pref[traits$Freq<10] <- NA
# QUESTION pour Cécile
# dans le manuscript, tu dis (en début de section analyses)
# que les prefered species ont au moins 10% des individus broutés
# mais ici tu prend à partir de 9%
# min(traits[traits$Pref =="O", "pourcent"], na.rm = T)

traits$Evit[traits$pourcent<=1]<-"O"
traits$Evit[traits$pourcent>1]<-"N"
traits$Evit[traits$Freq<10]<-NA 

traits$valeur<-NA
traits$valeur[traits$Pref=="O"] <- "pref"
traits$valeur[traits$Evit=="O"] <- "evit"

#### spfreq = espèces fréquentes : fichier traits specifiques, sp relevées plus de 9 fois- champ "valeur"(evit vs pref) et champ pourcent

spfreq<-traits[traits$Freq>9,]


table(traits$Evit)
table(traits$Pref)
table(traits$RA)
table(traits$conso)

traits$conso<-as.factor(traits$conso)
traits$RA<-as.factor(traits$RA)
traits$Pref<-as.factor(traits$Pref)
traits$Evit<-as.factor(traits$Evit)

traits$Helio[traits$gen_sp=="Pourouma "]<-"H_NP"






summary(browse_fin_net)

# write.xlsx(browse_fin_net, "browse_fin_net.xlsx")
# write.xlsx(browse_fin, "browse_fin.xlsx")
# write.xlsx(traits, "traits.xlsx")
# write.xlsx(spfreq, file="trait_spfreq.xlsx")

# write.csv(browse_fin_net, file="browse_fin_net.csv")
# write.csv(browse_fin, file="browse_fin.csv")
# write.csv(traits,file="traits.csv")
# write.csv(spfreq, file="trait_spfreq.csv")

write.csv(browse_fin_net, file="data_processed/browse_fin_net.csv")
write.csv(browse_fin, file="data_processed/browse_fin.csv")
write.csv(traits,file="data_processed/traits.csv")
write.csv(spfreq, file="data_processed/trait_spfreq.csv")

########### POUR GAGNER DU TEMPS: CHARGER FICHIER FINAL#############################

# browse_fin<-read.csv("browse_fin.csv",sep=",",dec=".", header=T)
# browse_fin_net<-read.csv("browse_fin_net.csv",sep=",",dec=".", header=T)
browse_fin<-read.csv("data_processed/browse_fin.csv",sep=",",dec=".", header=T)
browse_fin_net<-read.csv("data_processed/browse_fin_net.csv",sep=",",dec=".", header=T)

browse_fin_CH<-browse_fin[browse_fin$Type_placette=="CH",]
browse_fin_net_CH<-browse_fin_net[browse_fin_net$Type_placette=="CH",]

browse_fin_net$Brtot<- as.factor(browse_fin_net$Brtot)

# traits<-read.csv("traits.csv", sep=",", header=T)
# spfreq<-read.csv("traits_spfreq.csv", sep=",", header=T)
traits<-read.csv("data_processed/traits.csv", sep=",", header=T)
spfreq<-read.csv("data_processed/trait_spfreq.csv", sep=",", header=T)

##### Browse_fin :::: colonne thick_R et tough_R : moyennes traits d'apres la base de Rens, 
#                  colonne Dur_Fmat et Ep : mesure directe sur terrain etude Margot,par echantillon
#                 colonne moyenne_Dur et moyenne_Ep: moyenne des valeurs terrain , par espece
#                colonne thick et tough : moyenne specifique de Rens quand dispo, sinon, moyenne specifique de terrain de margot
#               colonnes finissant par _B : data de traits de Bridge
#               Broutage : prendre Colonnes Brtot: cumule si item brouté au premier ou deuxieme passage
#             Helio : H_P = Helio Pionniere, H_NP = Helio Non pionniere, NonH = non Heliophile  .. d'apres Molino + Giacomo

#### Browse_fin_net = nettoyé : uniquement les espèces bien identifiées en genetique ( fiab_genet = 1)
#    



############################ ANALYSES #########################################

#################################### comparaison Brt pas brt #############################################################

#EPAISSEUR#########################


#####>>>> sur tous individus, avec data moyennes browse : W test p=0.002 : évitent les feuilles les plus épaisses

testEp<-browse_fin_net %>% wilcox_test(moyenne_Ep~Brtot)
testEp

####### SUR espèces  classifiées selon consommation ; fichier traits ou fichier spfreq

## conso : especes consommee au moins 1 fois , fichier traits specifiques  p=0.34
testEpConso<-traits %>% wilcox_test(moyenne_Ep~conso)
testEpConso

## especes consommées >5% des fois : RA =Oui  p=0.1
testEpRA<-traits %>% wilcox_test(moyenne_Ep~RA)
testEpRA

## especes consommées >10% des fois : Pref =Oui >>> NS p=0.06 
testEpPref<-traits %>% wilcox_test(moyenne_Ep~Pref)
testEpPref





####prefere vs avoided, sp frequentes
# section 3.3
testEp_PE<- spfreq %>% wilcox_test(moyenne_Ep~valeur)
testEp_PE  # p=0.045    


bxp_Ep_PE<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=moyenne_Ep)) +
  geom_boxplot() +
  labs(caption = get_test_label(testEp_PE))+
  labs (y = "Leaf thickness", x = NULL)+
  scale_x_discrete(labels=c("Species Avoided","Species Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_Ep_PE


## violin plots

bxp_Ep_PEv<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=moyenne_Ep)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(testEp_PE))+
  labs (y = "Leaf thickness", x = NULL)+
  scale_x_discrete(labels=c("Avoided","Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )
bxp_Ep_PEv


###Modele influence epaisseur sur prob abroutissement
###beta=-0.05; p<0.01

model_ep_moy<-glm(Brtot~moyenne_Ep, family= "binomial", data=browse_fin_net)
summary(model_ep_moy)

pourcent_ep_moy<-lm(pourcent~moyenne_Ep, data=spfreq)
summary(pourcent_ep_moy)  ## 0.04



### DURETE############################


## sur tous les individus avec data moyennes browse  >>> p=0.01 : evitent les plus dures

testDur<-browse_fin_net %>% wilcox_test(moyenne_Dur~Brtot)
testDur

##Sur espèces selon taux de consommation

## conso : especes consommee au moins 1 fois , fichier traits specifiques p=0.74
testDurConso<-traits %>% wilcox_test(moyenne_Dur~conso)
testDurConso

## especes consommées >5% des fois : RA =Oui  p=0.09
testDurRA<-traits %>% wilcox_test(moyenne_Dur~RA)
testDurRA

## especes consommées >10% des fois : Pref =Oui  >> p=0.32 
testDurPref<-traits %>% wilcox_test(moyenne_Dur~Pref)
testDurPref

## pref vs evit especes frequentes
# section 3.3
testDur_PE<- spfreq %>% wilcox_test(moyenne_Dur~valeur)
testDur_PE # p=0.252    sur sp frequentes

bxp_durPE<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=moyenne_Dur)) +
  geom_boxplot() +
  labs(caption = get_test_label(testDur_PE))+
  labs (y = "Leaf toughness", x = NULL)+
  scale_x_discrete(labels=c("Species Avoided","Species Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )
bxp_durPE

## violin plots

bxp_durPEv<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=moyenne_Dur)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(testDur_PE))+
  labs (y = "Leaf toughness", x = NULL)+
  scale_x_discrete(labels=c("Avoided","Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )
bxp_durPEv


#MODELS

model_dur_moy<-glm(Brtot~moyenne_Dur, family= "binomial", data=browse_fin_net)
summary(model_dur_moy)


pourcent_dur_moy<-lm(pourcent~moyenne_Dur, data=spfreq)
summary(pourcent_dur_moy) ## p=0.04



######## SLA##################


## sur tous les individus, data Rens  : p=0,002 en less : brout?es SLA superieur

  ##comparaisons moyennes : SLA Brt : 25.3/ SLA non brt : 20.9, p=0.004
browse_fin_net %>% t_test(SLA_R~Brtot)
t.test(browse_fin_net$SLA_R~browse_fin_net$Brtot, alternative="less")

  #wilcox test  p<0.01
testSLA<-browse_fin_net %>% wilcox_test(SLA_R~Brtot)
testSLA


## par espèce, sur fichier traits

## conso : especes consommee au moins 1 fois , fichier traits specifiques p=0.18
testSLA_conso<-traits %>% wilcox_test(SLA_R~conso)
testSLA_conso

## especes consommées >5% des fois : RA =Oui  >>> p<0.38
testSLA_RA<-traits %>% wilcox_test(SLA_R~RA)
testSLA_RA

## especes consommées >10% des fois : Pref =Oui  >>>> p<0.01
testSLA_pref<-traits %>% wilcox_test(SLA_R~Pref)
testSLA_pref


## pref vs evit especes frequentes   # p=0.007
# section 3.3
testSLA_PE<- spfreq %>% wilcox_test(SLA_R~valeur)
testSLA_PE      

bxp_SLA_PE<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=SLA_R)) +
  geom_boxplot() +
  labs(caption = get_test_label(testSLA_PE))+
  labs (y = "SLA", x = "Browsers' Choice")+
  scale_x_discrete(labels=c("Species Avoided","Species Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_SLA_PE


###violin plot
bxp_SLA_PEv<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=SLA_R)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(testSLA_PE))+
  labs (y = "SLA", x = "Browsers' Choice")+
  scale_x_discrete(labels=c("Avoided","Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_SLA_PEv

#MODELS  

model_SLA<-glm(Brtot~SLA_R, family= "binomial", data=browse_fin_net)
summary(model_SLA)  #p***


pourcent_SLA<-lm(pourcent~SLA_R, data=spfreq)
summary(pourcent_SLA) ## p=***


####leaf_N


## Sur tous les individus - donnees Rens p<0.019*
testN<-browse_fin_net %>% wilcox_test(leaf_N_R~Brtot)
testN

##moyennes two sample / test less p= 0.010*  1.9 vs 2.1 pour brt
browse_fin_net %>% t_test(leaf_N_R~Brtot)
t.test(browse_fin_net$leaf_N_R~browse_fin_net$Brtot, alternative="less")

## sur espèces - fichier traits, 
## conso : especes consommee au moins 1 fois , fichier traits specifiques p=0.11
testNconso<-traits %>% wilcox_test(leaf_N_R~conso)
testNconso


## especes consommées >5% des fois : RA =Oui  >>> p<0.63
testN_RA<-traits %>% wilcox_test(leaf_N_R~RA)
testN_RA

## especes consommées >10% des fois : Pref =Oui  >>>> p=0.019*
testN_Pref<-traits %>% wilcox_test(leaf_N_R~Pref)
testN_Pref


## pref vs evit especes frequentes p= 0.09
# section 3.3
testN_PE<- spfreq %>% wilcox_test(leaf_N_R~valeur)
testN_PE  # p=0.097    sur sp frequentes

bxp_N_PE<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=leaf_N_R)) +
  geom_boxplot() +
  labs(caption = get_test_label(testN_PE))+
  labs (y = "leaf N", x = "Browsers' Choice")+
  scale_x_discrete(labels=c("Species Avoided","Species Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_N_PE



###violin plot
bxp_N_PEv<-ggplot(data=subset(spfreq, !is.na(valeur)), aes(x=valeur, y=leaf_N_R)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(testN_PE))+
  labs (y = "leaf N", x = "Browsers' Choice")+
  scale_x_discrete(labels=c("Avoided","Preferred"))+
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_N_PEv


#MODELS  

model_N<-glm(Brtot~leaf_N_R, family= "binomial", data=browse_fin_net)
summary(model_N)  #p**


pourcent_N<-lm(pourcent~leaf_N_R, data=spfreq)
summary(pourcent_N) ## NS


################ graph browse prefere/evite  avec les 4 ############################

graph<-ggarrange(bxp_Ep_PE, bxp_durPE, bxp_N_PE, bxp_SLA_PE,
                 labels= c("A", "B", "C", "D"),
                 ncol= 2, nrow= 2)

graph

graphV<-ggarrange(bxp_Ep_PEv, bxp_durPEv, bxp_N_PEv, bxp_SLA_PEv,
                  labels= c("A", "B", "C", "D"),
                  ncol= 2, nrow= 2)
graphV





## SAPWOOD Densité de bois donnees de Bridge  
## jeu de donn?es Bridge >>>> p<0.086 ns
testDens<-browse_fin_net %>% wilcox_test(sapwood_dens_B~Brtot)
testDens




####################### heliophiles et pionnieres  : total :19% H (11.6 % NP; 7.2 % Pionnieres)


fisher.test(browse_fin_net$Brtot,browse_fin_net$Helio)  #p<0.0005
brtot_helio<-table(browse_fin_net$Brtot,browse_fin_net$Helio)
proportions(brtot_helio, 1) #  13%  des brt sont H_P, 21% sont H_NP, 66% sont nonH
proportions(brtot_helio, 2) # 6% des H_NP sont broutées, et 6% des H_P, 3% des Non NH
fisher.test(brtot_helio) # p<0.005
brtot_helio
table(browse_fin_net$Helio)
proportions(table(browse_fin_net$Helio))  
#81% des plantules sont non helio, mais 66% des broutées
##12% des plantules sont H_NP, mais 21 % des broutées >> selection 
## 7% des plantules sont H_P , mais 13% des broutées >> selection


## ELECTIVITY (taux preference) 
library(electivity)
#indice Ivlev

ivlev_electivity(0.13, 0.07)
# 0.3 pour H_P
ivlev_electivity(0.21,0.12)
# 0.2727273 pour H_NP

ivlev_electivity(0.66,0.88)
# -0.14 pour les nonH

#jacob electivity

jacob_electivity(0.13, 0.07)
# 0.33 pour H_P

jacob_electivity(0.21,0.12)
# 0.32 pour H_NP

jacob_electivity(0.66,0.88)
#-0.5 pur nonH



#### influence du broutage sur les traits

#epaisseur et durete  
Brt_reac<-data.frame(matrix(nrow=3))
Brt_reac$gen_sp<-c("Inga umbellifera", "Iryanthera hostmannii", "Paypayrola ")
Brt_reac$p_Ep<- NA
Brt_reac$p_dur<- NA
Brt_reac<-select(Brt_reac,gen_sp,p_Ep,p_dur)


for (i in Brt_reac$gen_sp) {
  
  stat.test<-subset(browse_fin_net, gen_sp==i) %>% wilcox_test(Ep~Brt) 
  stat.test2<-subset(browse_fin_net, gen_sp==i) %>% wilcox_test(Dur_Fmat~Brt)
 Brt_reac$p_Ep[Brt_reac$gen_sp==i] <-stat.test$p
 Brt_reac$p_dur[Brt_reac$gen_sp==i] <-stat.test2$p
  
}




###### comparaison traits : Helio/ non helio ##############################"


##DURETE   "H_NP/HP et H_NP/nonH p=0.01**"


testHelio_Dur<-browse_fin_net %>% wilcox_test(moyenne_Dur~Helio)
testHelio_Dur

bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Helio","moyenne_Dur")]), x = "Helio", y = "moyenne_Dur",
  ylab = "Toughness", xlab = "Heliophily"
)

bxp +labs(subtitle = "H_NP/HP et H_NP/nonH p=0.01**")


##EPAISSEUR  helio NP moins epaisses , HP plus épaisses 
# section 3.2

testHelio_Ep<-browse_fin_net %>% wilcox_test(moyenne_Ep~Helio)
testHelio_Ep
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Helio","moyenne_Ep")]), x = "Helio", y = "moyenne_Ep",
  ylab = "epaisseur", xlab = "Helio"
)
bxp +labs(subtitle = "Wilcox tests Helio/nonH p<0.001")



## SLA   H_NP ont PLUS de SLA que les deux autres
testHelio_SLA<-browse_fin_net %>% wilcox_test(SLA_R~Helio)
testHelio_SLA
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Helio","SLA_R")]), x = "Helio", y = "SLA_R",
  ylab = "SLA_R", xlab = "Helio"
)
bxp +labs(title="data Rens", subtitle = "Wilcox tests p<0001")

## SLA   H_NP ont PLUS de SLA que les deux autres
testHelio_leafN<-browse_fin_net %>% wilcox_test(leaf_N_R~Helio)
testHelio_leafN
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Helio","leaf_N_R")]), x = "Helio", y = "leaf_N_R",
  ylab = "Leaf N", xlab = "Shade tolerance"
)
bxp +labs(title="data Rens", subtitle = "Wilcox tests p<0001")



################################################EFFET TYPE de PLACETTE#############################################################"


## nb plantules ~placette   plus en CH p<0.01  287.2 vs 194.7  p= 0.01

nombre<-tapply(Emplacement$nbplant,Emplacement$type, FUN=mean)
shapiro.test(Emplacement$nbplant[Emplacement$type=="CH"]) #ok
shapiro.test(Emplacement$nbplant[Emplacement$type=="PT"])  #ok
hist(Emplacement$nbplant[Emplacement$type=="PT"])

t.test(Emplacement$nbplant[Emplacement$type=="CH"], Emplacement$nbplant[Emplacement$type=="PT"], alternative="greater")
descr(Emplacement$nbplant[Emplacement$type=="CH"])
descr(Emplacement$nbplant[Emplacement$type=="PT"])


## Hauteur ~placette  :  bcp plus basses en PT mean : PT (93.7) que CH (71.7)

shapiro.test(browse_fin$H[browse_fin$Type_placette=="CH"]) # pas normal
hist(browse_fin$H)
#t.test(browse_BR$H~browse_BR$Type_placette) ## p<0.001 : 71,7 cm en CH / 93.7 cm en PT

stat.test<-browse_fin %>% wilcox_test(H~Type_placette)
stat.test # p<0.0001


          # mediane 53 vs 70
browse_fin %>%
  group_by(Type_placette) %>%
  get_summary_stats(H, type="median")


bxp <- ggboxplot(
  data=na.omit(browse_fin[,c("Type_placette","H")]), x = "Type_placette", y = "H",
  ylab = "H", xlab = "Type_placette"
)
bxp +labs(title="Hauteur plantules", subtitle = get_test_label(stat.test,detailed=T))


## BROUTE ~placette

broute_pla<-table(browse_fin$Brtot,browse_fin$Type_placette)
broute_pla
fisher.test(browse_fin$Brtot,browse_fin$Type_placette) # p<0.0001

proportions(broute_pla, 1) # 94.2 % des brt sont en CH
proportions(broute_pla, 2) # 5.8 % des plant brt en CH, 10 fois moins en PT:0.5%
fisher.test(broute_pla) # <0.0001


# leaf_N ~ PLACETTE


## jeu de donn?es Rens  *** p<0,0001 mais faible diff

shapiro.test(browse_fin_net$leaf_N_R[browse_fin$Type_placette=="PT"]) # pas normal
hist(browse_fin$leaf_N_R)
hist(browse_fin_net$leaf_N_R[browse_fin$Type_placette=="CH"])

stat.test<-browse_fin_net %>% wilcox_test(leaf_N_R~Type_placette)
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Type_placette","leaf_N_R")]), x = "Type_placette", y = "leaf_N_R",
  ylab = "leaf_N_R", xlab = "Type_placette"
)
bxp +labs(title="data Rens", subtitle = get_test_label(stat.test,detailed=T))

bxp+geom_violin()

# moyennes: 2 en CH/1.94 n PT
browse_fin_net %>% t_test(leaf_N_R~Type_placette)
t.test(browse_fin_net$leaf_N_R~browse_fin_net$Type_placette)

# mediane CH 1.87 vs 1.85
browse_fin %>%
  group_by(Type_placette) %>%
  get_summary_stats(leaf_N_R, type="median") 

# SAPWOOD ~ PLACETTE

## jeu de donn?es Bridge 
stat.test<-browse_fin_net %>% wilcox_test(sapwood_dens_B~Type_placette)
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("sapwood_dens_B","Type_placette")]), x = "Type_placette", y = "sapwood_dens_B",
  ylab = "sapwood_dens_B", xlab = "Type_placette"
)
bxp +labs(subtitle = get_test_label(stat.test,detailed=T), title = "data bridge")


## DURETE ~ PLACETTE


# data browse moyennes feuilles plus dures en PT, p =0.0035 ##########
shapiro.test(browse_fin_net$moyenne_Dur) #p<0.001 non normal
hist(browse_fin_net$moyenne_Dur) 

stat.test<-browse_fin_net %>% wilcox_test(moyenne_Dur~Type_placette)
stat.testp ## p=0.0035

bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Type_placette","moyenne_Dur")]), x = "Type_placette", y = "moyenne_Dur",
  ylab = "durete", xlab = "Type_placette"
)
bxp +labs(title= "data browse", subtitle = get_test_label(stat.test,detailed=T))

browse_fin_net %>% wilcox_effsize(moyenne_Dur~Type_placette)


# mediane 0.47 vs 0.5
browse_fin_net %>%
  group_by(Type_placette) %>%
  get_summary_stats(moyenne_Dur, type="median")


## jeu de donn?es Bridge *** p<0.001  Durete> en CH   ONTOGENIE ????
stat.test<-browse_fin_net %>% wilcox_test(tough_B~Type_placette)
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Type_placette","tough_B")]), x = "Type_placette", y = "tough_B",
  ylab = "durete_B", xlab = "Type_placette"
)
bxp +labs(subtitle = get_test_label(stat.test,detailed=T), title = "data bridge")



### sLA ~ Placette

## jeu de donn?es Rens ** p<0.001  SLA feuilles plus  en CH  (22 et 20)
shapiro.test(browse_fin_net$SLA_R) #p<0.001 non normal
hist(browse_fin_net$SLA_R) 

stat.test<-browse_fin_net %>% wilcox_test(SLA_R~Type_placette)
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Type_placette","SLA_R")]), x = "Type_placette", y = "SLA_R",
  ylab = "SLA_R", xlab = "Type_placette"
)
bxp +labs(title="data Rens", subtitle = get_test_label(stat.test,detailed=T))


# mediane ch 20.9 pt 19.3
browse_fin_net %>%
  group_by(Type_placette) %>%
  get_summary_stats(SLA_R, type="median")

#browse_fin_net %>% t_test(SLA_R~Type_placette)
#t.test(browse_fin_net$SLA_R~browse_fin_net$Type_placette)


## EPAISSEUR ~ PLACETTE

## data browse moyennes   p<0.0001 plus epaisses en PT

shapiro.test(browse_fin_net$moyenne_Ep) #p<0.0001 non normal
hist(browse_fin_net$moyenne_Ep) 

stat.test<-browse_fin_net %>% wilcox_test(moyenne_Ep~Type_placette)
bxp <- ggboxplot(
  data=na.omit(browse_fin_net[,c("Type_placette","moyenne_Ep")]), x = "Type_placette", y = "moyenne_Ep",
  ylab = "Epaisseur", xlab = "Type_placette"
)
bxp +labs(title= "data browse moyennes", subtitle = get_test_label(stat.test,detailed=T))


# mediane 0.47 vs 0.5
browse_fin_net %>%
  group_by(Type_placette) %>%
  get_summary_stats(moyenne_Ep, type="median")


###### nb Heliophiles ~placettes # 14 % H_NP et 7% HP en CH, 7% chaque en PT
prop_helio<-proportions(table(browse_fin_net$Helio,browse_fin_net$Type_placette),2)
prop_helio
fisher.test(browse_fin_net$Helio,browse_fin_net$Type_placette)  #p<0.0001
helio_placette<-table(browse_fin_net$Helio,browse_fin_net$Type_placette)
helio_placette

helio_placette<- proportions(table(browse_fin_net$Helio,browse_fin_net$Emplacement),2)
helio_placette

####### nb Lianes ~placettes
prop_liane<-proportions(table(browse_fin$Liane,browse_fin$Type_placette),2)
prop_liane

####### Feuilles jeunes###############""
# la pr?sence de F jeunes influence t elle le brtage (deuxi?me passage) ? NS
# uniquement les placettes ou il y a eu un deuxi?me passage de verif: CH1, CH2 et CH3 >> fich browse_FJ

browse_FJ<- browse_fin[(browse_fin$Emplacement=="CH1"|browse_fin$Emplacement=="CH2"|browse_fin$Emplacement=="CH3"), ]
browse_FJ$FJ<-as.factor(browse_FJ$FJ)

table(browse_FJ$FJ)
table(browse_FJ$Brt_pas2)

table(browse_FJ$Brt_pas2,browse_FJ$FJ)
proportions(table(browse_FJ$Brt_pas2,browse_FJ$FJ),1) # 43% des brt au passage 2 avaient des FJ
proportions(table(browse_FJ$Brt_pas2,browse_FJ$FJ),2) # 8% des plant avec des FJ ont ete broutées (5.7% les autres)
proportions(table(browse_FJ$Brt_pas2,browse_FJ$FJ))
test_FJ<-table (browse_FJ$Brt_pas2,browse_FJ$FJ)
proportions(test_FJ,2)
chisq.test(test_FJ)
fisher.test(test_FJ)

# presence de FJ selon type placette : plus de FJ en CH

test_FJ_pla<-table(browse_fin$FJ,browse_fin$Type_placette)
fisher.test(test_FJ_pla) #p< 0.001
proportions(test_FJ_pla,2) # en CH 31.2% on FJ contre 12.9 en PT

## modele proba brtage 2emme passage en fonction presence FJ au premier

B_FJ <- glm(Brt_pas2 ~ FJ , family = binomial, data = browse_FJ)
summary(B_FJ)
# bof bof


### effet type placette sur traits d'une meme espèce

#epaisseur et durete  ## diff signif pour 5 ou 6 espèces
CH_PT<-read.csv("CH_PT.csv")
CH_PT$p_Ep<- NA
CH_PT$p_dur<- NA

for (i in CH_PT$gen_sp) {

  stat.test<-subset(browse_fin_net, gen_sp==i) %>% wilcox_test(Ep~Type_placette) 
  stat.test2<-subset(browse_fin_net, gen_sp==i) %>% wilcox_test(Dur_Fmat~Type_placette)
  CH_PT$p_Ep[CH_PT$gen_sp==i] <-stat.test$p
  CH_PT$p_dur[CH_PT$gen_sp==i] <-stat.test2$p
  
}

write.xlsx(CH_PT, "CH_PT.xlsx")


###################### MODELES complets #########################################################

#probabilité d'une plantule d'etre broutee



## correlations
traits_num<-traits[4:14]
cor(traits_num, use = "complete.obs")
str(browse_fin_net)
browse_fin_net_num<-select(browse_fin_net, leaf_N_R, SLA_R, moyenne_Dur, moyenne_Ep)
cor(browse_fin_net_num, use="complete.obs")

## SLA_R et leaf_N correles 0.47 et toughness_R neg corr a leaf N -0.41

library(car)
library(rstatix)

browse_sansNA<-drop_na(browse_fin_net,(c(Brtot,SLA_R,leaf_N_R,moyenne_Dur,moyenne_Ep,Helio,Type_placette)))

browse_sansNA$Brtot<-dplyr::recode(browse_sansNA$Brtot, "O"= 1, "N"= 0)

library(MASS)

## donnes browse moyennes _ modele binomial broute/non broute_data reduites, sans NA pour ttes variables

## stepwise >> modle selectionne =glm(formula = Brtot ~ SLA_R + moyenne_Dur + Type_placette, family = binomial, 

browse_sansNA_short<-select(browse_sansNA, Brtot, leaf_N_R, SLA_R, moyenne_Dur, moyenne_Ep, Type_placette)

full.model<-glm(Brtot~., family=binomial, data=browse_sansNA_short)

step.model<-stepAIC(full.model, direction="both")
summary(step.model)
library(car)
vif(step.model)
pseudoR2<-(step.model$null.deviance - step.model$deviance)/step.model$null.deviance
pseudoR2

### avec effet aleatoire sur paire de placette

Bal<-glmer(Brtot ~ moyenne_Dur + SLA_R + Type_placette +(1|paire), family = binomial, data = browse_sansNA)
summary (Bal)


## plantules en chablis uniquement : 
#modele selectionne = glm(formula = Brtot ~ SLA_R + moyenne_Dur, family = binomial, data = browse_sansNA_CH)
#>>SLA et moy_dur

browse_sansNA_CH<-drop_na(select(browse_fin_net_CH, Brtot, leaf_N_R, SLA_R, moyenne_Dur, moyenne_Ep))

full.model<-glm(Brtot~., family=binomial, data=browse_sansNA_CH)

step.model<-stepAIC(full.model, direction="both")
summary(step.model)


### modeles binom sur fichier traits : proba de broutage de l'espèce en fonction "valeur" Evit/Pref)

traits_sansNA<- read.csv("traits_sansNA.csv", sep=";", header=T)
traits_sansNA$valeur<-as.factor(traits_sansNA$valeur)
traits_sansNA_short<-select(traits_sansNA, valeur, leaf_N_R, SLA_R, moyenne_Dur, moyenne_Ep )

full.model<-glm(valeur~., family=binomial, data=traits_sansNA_short)

step.model<-stepAIC(full.model, direction="both")
summary(step.model)


###  modele lineaire broutage espèce en fonction frequence de consommation (pour sp plus de 10 fois)

## modele selectionné =>>> glm(formula = pourcent ~ SLA_R + Helio, data = spfreqsansNA)
#

spfreqsansNA<-select(spfreq,pourcent,SLA_R,leaf_N_R,moyenne_Dur,moyenne_Ep,Helio)
spfreqsansNA <-drop_na(spfreqsansNA,(c(pourcent,SLA_R,leaf_N_R,moyenne_Dur,moyenne_Ep,Helio)))

full.model<-glm(pourcent~., data=spfreqsansNA)


step.model<-stepAIC(full.model, direction="both")
summary(step.model)
plot(step.model)

## SLA et Helio sont corrélés>> sans hélio
spfreqsansNA2<-select(spfreq,pourcent,SLA_R,leaf_N_R,moyenne_Dur,moyenne_Ep)
spfreqsansNA2 <-drop_na(spfreqsansNA2,(c(pourcent,SLA_R,leaf_N_R,moyenne_Dur,moyenne_Ep)))

full.model2<-lm(pourcent~., data=spfreqsansNA2)

step.model2<-stepAIC(full.model2, direction="both")
summary(step.model2)
plot(step.model2)

#######################################################FAUNE###################
faune<-read.csv("testFaune.csv", sep=";")


testMam<-subset(faune,sp=="Mazama_a") %>% wilcox_test(CR~surv)
testMam$p # 0.8
maz<-subset(faune,sp=="Mazama_a") 
t.test(maz$CR~maz$surv) #0.5 19.7 - 31.2

testPass<-subset(faune,sp=="Passalites_n") %>% wilcox_test(CR~surv)
testPass$p #0.55
Pass<-subset(faune,sp=="Passalites_n") 
t.test(Pass$CR~Pass$surv) #0.32  27.4  - 18.3

testPec<-subset(faune,sp=="Pecari_t") %>% wilcox_test(CR~surv)
testPec$p #0.56
Pec<-subset(faune,sp=="Pecari_t") 
t.test(Pec$CR~maz$surv) #0.36  24.3-60.2

testTap<-subset(faune,sp=="Tapirus_t") %>% wilcox_test(CR~surv)
testTap$p  #0.17
Tap<-subset(faune,sp=="Tapirus_t") 
t.test(Tap$CR~maz$surv) #0.37  3.65  -  6.15

testTay<-subset(faune,sp=="Tayassu_p") %>% wilcox_test(CR~surv)
testTay$p #0.5
Tay<-subset(faune,sp=="Tayassu_p") 
t.test(Tay$CR~maz$surv) #0.11 29.5 - 16.5

###############################suppl figures 
## violin plots on seedling browsed
stat.test1<-browse_fin_net %>% wilcox_test(moyenne_Ep~Brtot)
bxp_Ep<-ggplot(data=subset(browse_fin_net, !is.na(Brtot)), aes(x=Brtot, y=moyenne_Ep)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(stat.test1))+
  labs (y = "Leaf thickness", x=NULL) +
  scale_x_discrete(labels=c("No","Yes")) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )
bxp_Ep 

stat.test2<-browse_fin_net %>% wilcox_test(moyenne_Dur~Brtot)
bxp_Dur<-ggplot(data=subset(browse_fin_net, !is.na(Brtot)), aes(x=Brtot, y=moyenne_Dur)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(stat.test2))+
  labs (y = "Leaf toughness", x=NULL) +
  scale_x_discrete(labels=c("No","Yes")) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )
bxp_Dur


stat.test3<-browse_fin_net %>% wilcox_test(SLA_R~Brtot)
bxp_SLA<-ggplot(data=subset(browse_fin_net, !is.na(Brtot)), aes(x=Brtot, y=SLA_R)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75))+
  labs(caption = get_test_label(stat.test3))+
  labs (y = "SLA", x = "Browsed seedlings")+
  scale_x_discrete(labels=c("No","Yes")) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_SLA

stat.test4<-browse_fin_net %>% wilcox_test(leaf_N_R~Brtot)
bxp_N<-ggplot(data=subset(browse_fin_net, !is.na(Brtot)), aes(x=Brtot, y=leaf_N_R)) +
  geom_violin(draw_quantiles = c(0.25,0.5,0.75)) +
  labs(caption = get_test_label(stat.test4))+
  labs (y = "Leaf N", x = "Browsed seedlings")+
  scale_x_discrete(labels=c("No","Yes")) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size=12),
    plot.caption = element_text(size=12)
  )

bxp_N


graph_sup<-ggarrange(bxp_Ep, bxp_Dur, bxp_N, bxp_SLA,
                 labels= c("A", "B", "C", "D"),
                 ncol= 2, nrow= 2)

graph_sup


