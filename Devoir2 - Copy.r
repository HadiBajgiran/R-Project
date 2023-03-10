
#  Question 1:

#Importation du fichier de données

setwd("C:\\Users\\Admin\\OneDrive\\Documents\\Logiciel statistique\\DATA_R") 
frais_garde= read.csv("credit_frais_de_garde_denfants.csv", encoding ="UTF-8")

# 1a) Création de nouvelles variables de façon conditionnelle

frais_garde= data.frame(frais_garde, Age2= ifelse (frais_garde$Age=="Moins de 30 ans", 1,
                                                   ifelse (frais_garde$Age=="30 à 34 ans", 2,
                                                  ifelse (frais_garde$Age=="35 à 39 ans", 3,
                                                  ifelse (frais_garde$Age=="40 à 44 ans", 4,
                                                  ifelse (frais_garde$Age=="45 à 49 ans", 5,6))))))

# 1b) Création de nouvelles variables avec concaténation

# install.packages("stringr")

library("stringr")

frais_garde$genre_age<- paste(str_c(substr(frais_garde$Genre,1,1), frais_garde$Age2, sep = "-"))


# Question 2: avec les fonctions de base de R

# D'abord on sélectionne les données de 2014

echant2014=subset (frais_garde, Annee_imposition==2014)

# Ensuite on crée le nouveau objet

frais_garde_2014 = aggregate(Nombre_de_particuliers~ genre_age + Region_administrative, data = echant2014, FUN = sum)


# la même chose pour 2015

echant2015=subset (frais_garde, Annee_imposition==2015)

frais_garde_2015 = aggregate(Nombre_de_particuliers~ genre_age + Region_administrative, data = echant2015, FUN = sum)

# Renommer les variables avant la justaposition

names(frais_garde_2014)[3]= "Nombre_de_particuliers _2014"
names(frais_garde_2015)[3]= "Nombre_de_particuliers _2015"

# Justaposition

frais_garde_2014 = frais_garde_2014[order(frais_garde_2014$genre_age, frais_garde_2014$Region_administrative),]
frais_garde_2015 = frais_garde_2015[order(frais_garde_2015$genre_age, frais_garde_2015$Region_administrative),]

frais_justa = merge(x=frais_garde_2014, y=frais_garde_2015, by=c("genre_age", "Region_administrative"), all=TRUE)

# Question 2: avec Dplyr

#install.packages("dplyr")

library("dplyr")

frais_garde_20141= frais_garde%>% 
  filter (Annee_imposition==2014)%>% 
  group_by(genre_age, Region_administrative) %>% 
  summarise(Nombre_de_particuliers= sum(Nombre_de_particuliers))

# pour 2015 

library("dplyr")
frais_garde_20151= frais_garde%>% 
  filter (Annee_imposition==2015)%>% 
  group_by (genre_age, Region_administrative) %>% 
  summarise(Nombre_de_particuliers= sum(Nombre_de_particuliers))

# Renommer les variables avant la justaposition

library("dplyr")
frais_garde_20141= rename(frais_garde_20141, Nombre_de_particuliers_2014= Nombre_de_particuliers)

library("dplyr")
frais_garde_20151= rename(frais_garde_20151, Nombre_de_particuliers_2015= Nombre_de_particuliers)


frais_justa2 = full_join(x=frais_garde_20141, y=frais_garde_20151, by=c("genre_age", "Region_administrative"))


# Question 3: 

#D'abord filtrer pour avoir les valeurs sans doublons

sans_double= unique(frais_garde$Region_administrative)

# Ensuite on extrait

library("stringr")

region=unlist(str_extract_all(sans_double,"\\w+-\\w+-\\w+-\\w+|\\w+-\\w+-\\w+|\\w+-\\w+"))

region


# Question 4:
  
  frais=function(x,y){
    sub=subset(frais_garde,frais_garde$Avec_Conjoint==x & frais_garde$Annee_imposition==y)
    aggregate(sub$Nombre_de_particuliers~sub$Avec_Conjoint+sub$Annee_imposition,FUN=sum,na.rm=TRUE)
  print(sum (sub$Nombre_de_particuliers))
    }
  
  frais(FALSE,2014)
  frais(TRUE,2014)
  

# Question 5:
  
# Graphique 1- Diagramme de dispersion: analyse bivariée avec 2 variables quantitatives. 
  
# Graphique 2- Diagrammes en boîte groupée: analyse bivariée avec une variable qualitative et une variable quantitative.
  
# Graphique 3- Histogramme: analyse univariée avec une variable quantitative
  
  
 
  