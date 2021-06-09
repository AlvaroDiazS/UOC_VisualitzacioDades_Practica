#llibreries ----
library(tidyverse)
library(readxl)

rm(list = ls())

#directori per si no es fa servir el projecte
#setwd("/Users/alvar/Documents/Github/UOC_VisualitzacioDades_Practica")

#Preparació dades població ----
#llegir dades + elegir columnes


dadesPobl <- read.csv(file = "data/Registre_central_de_poblaci__del_CatSalut__poblaci__per_municipi.csv", encoding = "UTF-8") %>% 
  select(1,6,9,8,10)

#renombrar columnes
colnames(dadesPobl) <- c("Any", "codi_municipi", "edat", "genere","num_total")

#filtrar dades + creació variable
dadesResPol <- dadesPobl %>% filter(edat >= 3, edat <= 15, Any >= 2017, Any <= 2020) %>% 
  group_by(Any, codi_municipi, edat) %>% 
  summarise(num_total = gsub(x = num_total, pattern = "\\.", replacement = ""),
            num_dona = ifelse(genere == "Dona", as.numeric(num_total), NA),
            num_home = ifelse(genere == "Home", as.numeric(num_total), NA),
            num_dona = sum(num_dona, na.rm = T),
            num_home = sum(num_home, na.rm = T),
            num_total = num_dona + num_home) %>%
  unique() %>% 
  ungroup() %>% 
  select(Any, codi_municipi, edat, num_dona, num_home, num_total )

#Llegir dades educació + preparar les dades ----
dades <- read.csv(file = "data/Estad_stica_de_l_assignaci__de_places_en_el_proc_s_de_la_Preinscripci__en_els_ensenyaments_obligatoris_i_Infantil_segon_cicle.csv", encoding = "UTF-8")

#renombrar dades
colnames(dades) <- c("curs", "codi_centre", "nom_complet", "codi_naturalesa", "nom_naturalesa", "codi_titularitat", "nom_titularitat",
                     "codi_delegacio","nom_delegacio", "codi_comarca", "nom_comarca", "codi_municipi", "codi_municipi_6",
                     "nom_municipi", "codi_districte_minicial", "nom_dm", "UTM_X", "UTM_Y", "lng", "lat", "nom_ensenyament", "nivell", 
                     "nom_grups", "mixt", "nombre_places", "places_ofertades", "assignacions", "assig_1a", "assig_altres", "georeferencia")

#dades per corregir les corrdenades de 2020/2021
#Es seleccionen les correctes per a fer un join més endavant
dadesCoord <- dades %>% filter(curs == "2017/2018") %>% select(codi_centre, lng, lat) %>% unique()

#transformacions de columnes + adaptar codi de municipi a 5 xifres sempre (els de Barcelona en tenien 4)
dades <- dades %>%mutate(mixt = ifelse(mixt =='X', T, F),
                          Any = as.numeric(str_split(curs, pattern = '/', simplify = T)[,1]),
                          codi_municipi = as.character(codi_municipi),
                          codi_municipi = ifelse(nchar(codi_municipi) == 4, paste(0, codi_municipi, sep = ''), codi_municipi)) %>% 
  filter(nchar(codi_municipi) == 5) %>% 
  select(curs, Any, everything())

#Taula Edat per asignar a cada curs escolar un edat diferent entre els 3 i el 15 anys
taulaEdat <- dades %>% select(nom_ensenyament, nivell) %>% unique() %>% arrange(nom_ensenyament, nivell) %>% 
  bind_cols("edat" = c(3:15))

#Creació de la base de dades general ----
#Join de les dades d'educacuó, afegir dades de l'edat, les coordenades corregides i la població per edat (nens i nenes) per cada any
data <- dades %>% select(-lng, -lat) %>%  left_join(taulaEdat)%>% left_join(dadesResPol) %>% left_join(dadesCoord) %>% 
  mutate(num_dona = round(num_dona,0),
         num_home = round(num_home, 0),
         num_total = round(num_total, 0))


#Sense accents per a poder carregar bé a Flourish Studio 
for(i in c(1:ncol(data))){
  data[,i] <- iconv(data[,i],from="UTF-8",to="ASCII//TRANSLIT")
}

#guardar dades generals ----
write.csv(data, file = "data/data_pra.csv")


#Dades per al mapa de 3 anys ----
data_3anys <- data %>% filter(edat == 3, curs =="2020/2021") %>% 
  mutate_at(.vars = c(24:28,30:33),.funs = as.numeric) %>% 
  mutate(`% ocupades` = round(100*assignacions/nombre_places,1),
         `% infants del municipi` = round(100*num_total/assignacions,1),
         `% infants del municipi` = ifelse(`% infants del municipi` > 100, 100, `% infants del municipi`)
         )

write.csv(data_3anys, file = "data/data_3anys.csv")

data_12anys <- data %>% filter(edat == 12, curs =="2020/2021") %>% 
  mutate_at(.vars = c(24:28,30:33),.funs = as.numeric) %>% 
  mutate(`% ocupades` = round(100*assignacions/nombre_places,1),
         `% infants del municipi` = round(100*num_total/assignacions,1),
         `% infants del municipi` = ifelse(`% infants del municipi` > 100, 100, `% infants del municipi`)
  )

write.csv(data_12anys, file = "data/data_12anys.csv")


#dades bombolles ----
data_bombolla <- data %>% filter(curs =="2020/2021") %>% 
  mutate_at(.vars = c(24:28,30:33),.funs = as.numeric) %>%
  mutate(`% ocupades` = round(100*assignacions/nombre_places,1),
         `% infants del municipi` = round(100*num_total/assignacions,1),
         `% infants del municipi` = ifelse(`% infants del municipi` > 100, 100, `% infants del municipi`)
  ) %>% 
  group_by(nom_comarca, nivell, nom_naturalesa, nom_ensenyament, edat) %>% 
  summarise(`places ofertades` = round(sum(`places_ofertades`, na.rm = T),1),
            assignacions = round(sum(assignacions, na.rm = T),1),
            `% ocupades` = round(100*assignacions/`places ofertades`,1),
            dem = "Catalunya",
            `Curs` = paste(nivell, nom_ensenyament, sep = '-' )
            ) %>% 
  unique()

write.csv(data_bombolla, file = "data/data_bombolla.csv")

#dades donut ----
data_donut <- data %>% filter(curs =="2020/2021") %>% 
  mutate_at(.vars = c(24:28,30:33),.funs = as.numeric) %>%
  mutate(`% ocupades` = round(100*assignacions/nombre_places,1),
         `% infants del municipi` = round(100*num_total/assignacions,1),
         `% infants del municipi` = ifelse(`% infants del municipi` > 100, 100, `% infants del municipi`)
  ) %>% 
  group_by(nom_comarca,nom_naturalesa,nom_ensenyament) %>% 
  summarise(`assignacions totals` = round(sum(`assig_1a`, na.rm = T),1),
            prim_peti = round(sum(assignacions, na.rm = T),1),
            `% 1a assig` = round(100*prim_peti/`assignacions totals`,1)#,
            #`Curs` = paste(nivell, nom_ensenyament, sep = '-' )
  ) %>% 
  unique()

write.csv(data_donut, file = "data/data_donut.csv")

#graf barres ----
data_barres <- data %>% 
  mutate_at(.vars = c(24:28,30:33),.funs = as.numeric) %>%
  mutate(`% ocupades` = round(100*assignacions/nombre_places,1),
         `% infants del municipi` = round(100*num_total/assignacions,1),
         `% infants del municipi` = ifelse(`% infants del municipi` > 100, 100, `% infants del municipi`)
  ) %>% 
  group_by(curs, nom_comarca ) %>% 
  summarise(`Places totals` = sum(nombre_places, na.rm = T),
            `Places ofertades` = sum(places_ofertades, na.rm = T),
            `Places assignades` = sum(assignacions, na.rm = T),
            `% places assignades` = round(100*`Places assignades`/`Places ofertades`,1)
            )

write.csv(data_barres, file = "data/data_barres.csv")

#borrar envoirement 
rm(list = ls())
