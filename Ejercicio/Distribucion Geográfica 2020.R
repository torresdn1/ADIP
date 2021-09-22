library(tidyverse)
library(sf)
library(leaflet)
library(spdep)
library(htmlwidgets)

#Datos
ubicacion_c5 <- read.csv("~/Documents/Examen ADIP/Datos/C5 data/2021-05-28-postes-c5-wifi-datos-abiertos10-postes-adicionales.xlsx-hoja1.csv")
colonias <- read_sf("~/Documents/Examen ADIP/Datos/Colonias CDMX/coloniascdmx.shp")
delito <- read.csv("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/carpetas_completa_julio_2021.csv")

## Preparamos datos de ubicacion de cámaras y ubicacion de delitos
## Dado que las bases no comparten id con el .shp de colonias, definiremos una función que asignará a cada punto el id del polígono específico al que pertenece.
## Para ello, reduciremos el número de filas para que sean más económicas las iteraciones

#Primero van las cámaras: 
camarasxcolonia <- ubicacion_c5 %>%
  select(ALCALDIA, COLONIA) %>% 
  group_by(ALCALDIA, COLONIA) %>% 
  summarise(CAMARAS= n()) #Camaras por colonia

puntos_camaras <- ubicacion_c5 %>% 
  mutate(LATITUD= as.numeric(str_replace_all(ubicacion_c5$LATITUD, pattern = ",", replacement = ".")),
         LONGITUD= as.numeric(str_replace_all(ubicacion_c5$LONGITUD, pattern = ",", replacement = "."))) %>% 
  select(ALCALDIA, COLONIA, LATITUD, LONGITUD) %>% 
  distinct(ALCALDIA, COLONIA, .keep_all = T) #Un punto por colonia

puntos_camaras <- puntos_camaras %>% 
  left_join(camarasxcolonia, by= c("ALCALDIA", "COLONIA"))

#Homologamos con names y categorias del .shp
puntos_camaras$ALCALDIA[puntos_camaras$ALCALDIA=="CUAJIMALPA"]<- "CUAJIMALPA DE MORELOS"
puntos_camaras$ALCALDIA[puntos_camaras$ALCALDIA=="MAGDALENA CONTRERAS"]<- "LA MAGDALENA CONTRERAS"
names(puntos_camaras)[names(puntos_camaras)=="LONGITUD"]<- "longitud"
names(puntos_camaras)[names(puntos_camaras)=="LATITUD"]<- "latitud"

rm(camarasxcolonia, ubicacion_c5)

## Ahora van datos de ubicacion de delitos: mismos procedimientos
delitoxcolonia <- delito %>%
  filter(ao_hechos==2020, 
         !is.na(colonia_hechos),
         !is.na(longitud)) %>% 
  select(alcaldia_hechos, colonia_hechos) %>% 
  group_by(alcaldia_hechos, colonia_hechos) %>% 
  summarise(DELITOS= n())

puntos_delito <- delito %>% 
  filter(ao_hechos==2020, 
         !is.na(colonia_hechos), 
         !is.na(longitud)) %>% 
  select(alcaldia_hechos, colonia_hechos, latitud, longitud) %>% 
  distinct(alcaldia_hechos, colonia_hechos, .keep_all = T)

puntos_delito <- puntos_delito %>% 
  left_join(delitoxcolonia, by= c("alcaldia_hechos", "colonia_hechos"))

puntos_delito$alcaldia_hechos[puntos_delito$alcaldia_hechos=="GUSTAVO A MADERO"]<- "GUSTAVO A. MADERO"
rm(delitoxcolonia, delito)

#La función para la asignación será: 
#1. Para cada cámara/delito ubicada en una delegación determinada, la función buscará la colonia a la que pertenece dentro del .shp, acotando la búsqueda a las colonias de esa alcaldia, y le asignará su clave

#CAMARAS C5 PRIMERO: Voy a tener suerte

alcaldias <- unique(puntos_camaras$ALCALDIA) #Vector con nombre de alcaldias

#FUNCION GENERAL: 
mifuncion <- function(i){
puntos <- puntos_camaras %>% filter(ALCALDIA== i) %>%  select(longitud, latitud) #Solo puntos de la alcaldia i
colonias1 <- colonias %>% filter(alcaldi== i) %>%  select(cve_col) #Solo geometría y cve_col de la alcaldia i

#Función particular para asignar clave
puntos$cve_col <- apply(puntos, 1, function(row) {  
  col_pl <- st_transform(colonias1, 4326)   
  coords <- as.data.frame(matrix(row, nrow = 1, 
                                 dimnames = list("", c("longitud", "latitud"))))   
  pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 4326)
  col_pl[which(st_intersects(pnt_sf, col_pl, sparse = FALSE)), ]$cve_col ## cve_col
})

puntos
}

suerte<- lapply(alcaldias, mifuncion)

datalist <- list()
n<- 16
for (i in 1:n) {
  datos <- as_tibble(suerte[[i]])
  datalist[[i]] <- datos
}
camaras_colonias_cdmx <- do.call(rbind, datalist) #Listo el dataframe
camaras_colonias_cdmx<- camaras_colonias_cdmx %>% mutate(cve_col= as.character(cve_col))

#Unimos con el dataframe original para valor de cámaras por colonia
camaras_colonias_cdmx <- camaras_colonias_cdmx %>% 
  left_join(puntos_camaras, by= c("longitud", "latitud"))

rm(datalist, datos, mifuncion, i, n, suerte, puntos_camaras)

camaras_colonias_cdmx<- camaras_colonias_cdmx %>% 
  filter(cve_col!="character(0)") %>% 
  group_by(ALCALDIA, cve_col) %>% 
  summarise(CAMARAS= sum(CAMARAS, na.rm = T))#Lista la matriz con las cve_col del shp

##AHORA RESOLVAMOS EL TEMA DE LOS DELITOS: Mismo procedimiento
mifuncion <- function(i){
  puntos <- puntos_delito %>% filter(alcaldia_hechos== i) %>%  select(longitud, latitud) #Solo puntos
  colonias1 <- colonias %>% filter(alcaldi== i) %>%  select(cve_col) #Solo geometría y cve_col
  
  #Función para asignar clave
  puntos$cve_col <- apply(puntos, 1, function(row) {  
    col_pl <- st_transform(colonias1, 4326)   
    coords <- as.data.frame(matrix(row, nrow = 1, 
                                   dimnames = list("", c("longitud", "latitud"))))   
    pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 4326)
    col_pl[which(st_intersects(pnt_sf, col_pl, sparse = FALSE)), ]$cve_col ## cve_col
  })
  
  puntos
}

suerte<- lapply(alcaldias, mifuncion)

datalist <- list()
n<- 16
for (i in 1:n) {
  datos <- as_tibble(suerte[[i]])
  datalist[[i]] <- datos
}

delitos_colonias_cdmx <- do.call(rbind, datalist) #Listo el dataframe
delitos_colonias_cdmx<- delitos_colonias_cdmx %>% mutate(cve_col= as.character(cve_col))

#Unimos con el dataframe original para valor de cámaras por colonia
delitos_colonias_cdmx <- delitos_colonias_cdmx %>% 
  left_join(puntos_delito, by= c("longitud", "latitud"))

rm(datalist, datos, mifuncion, i, n, suerte, puntos_delito, alcaldias)

delitos_colonias_cdmx<- delitos_colonias_cdmx %>% 
  filter(cve_col!="character(0)") %>% 
  group_by(alcaldia_hechos, cve_col) %>% 
  summarise(DELITOS= sum(DELITOS, na.rm = T))#Lista la matriz con las cve_col del shp

##NOTA ACLARATORIA: DURANTE EL PROCEDIMIENTO HAY DOS FUENTES DE PERDIDA DE INFORMACIÓN: 
# LA PRIMERA SUCEDE CUANDO LOS REGISTROS NO TIENEN NI COLONIA O UNA COORDENADA ESPECIFICA, 
# LA SEGUNDA, POR EL CONTRARIO, SUCEDE PORQUE LA FUNCIÓN NO IDENTIFICA NINGUN POLÍGONO AL CUAL PERTENECE EL DELITO ("character(0)")
# ESTO PUEDE SUCEDER PORQUE CIERTAMENTE EXISTEN CAMARAS DEL C5 FUERA DE LAS COLONIAS DEFINIDAS POR EL SHAPE PERO 
# TAMBIEN PORQUE LOS PUNTOS ESTÁN EXACTAMENTE EN MEDIO DE DOS POLÍGONOS.
# LA FUNCIÓN MINIMIZA ESA ÚLTIMA SITUACIÓN AL UTILIZAR UN PUNTO EN COMÚN PARA TODOS LOS QUE FORMAN PARTE DE LA MISMA COLONIA, 
# PERO NO GARANTIZA QUE EL 100% DE LOS DATOS SE TRASLADEN CORRECTAMENTE. POR ELLO, TAMBIEN SE CLACULARON LOS PROMEDIOS DE COLONIAS VECINAS
# ESO DEBE MEJORARSE. 

##UNIMOS DELITOS Y CAMARAS Y SHAPE
data<- camaras_colonias_cdmx %>% 
  left_join(delitos_colonias_cdmx, by = "cve_col") %>% 
  select(ALCALDIA, cve_col, CAMARAS, DELITOS) %>% 
  mutate(DELITOS= ifelse(is.na(DELITOS), 0, DELITOS))

rm(camaras_colonias_cdmx, delitos_colonias_cdmx)

data<- colonias %>% 
  left_join(data, by = "cve_col") %>% 
  mutate(DELITOS= ifelse(is.na(DELITOS), 0, DELITOS),
         CAMARAS= ifelse(is.na(CAMARAS), 0, CAMARAS))#YA TENEMOS EN UN MISMO OBJETO CAMARAS, DELITOS Y POLIGONOS

##### DATOS POR VECINDAD EN COLONIAS: PARA AJUSTAR

data1 <- data %>% mutate(order= 1:n()) 

#Identificamos colonias vecinas
neighbours <- poly2nb(data1$geometry, row.names = NULL, 
                      snap=sqrt(.Machine$double.eps),
                      queen=TRUE, useC=TRUE, foundInBox=NULL)

#Función para calcular promedios de colonias vecinas.
mi_function <- function(i){
  promedios <- data1 %>% filter(order==i |
                                  order %in% neighbours[[i]]) %>% 
    mutate(`Delitos Promedio Vecinos`= mean(DELITOS, na.rm = T),
           `Cámaras Promedio Vecinos`= mean(CAMARAS, na.rm = T),
           `Total Delitos Vecinos`= sum(DELITOS, na.rm = T),
           `Total Cámaras Vecinos`= sum(CAMARAS, na.rm = T)) %>% 
    filter(order==i)
}

n <- c(1:1808)
promedios_colonia<- lapply(n, mi_function)

datalist <- list()
for (i in n) {
  datos <- promedios_colonia[[i]]
  datalist[[i]] <- datos
}

promedios_colonia <- do.call(rbind, datalist)
rm(datalist, datos, i, n, mi_function, neighbours, data1, data)

##Mapa interactivo: Leaflet

popup <- paste0("<b>Alcaldía: </b>", promedios_colonia$alcaldi, "<br>",
                "<b>Colonia: </b>", promedios_colonia$nombre, "<br>", 
                "<b>Promedio de delitos (Colonias colindantes): </b>", prettyNum(round(promedios_colonia$`Delitos Promedio Vecinos`, digits = 1), big.mark = ","), "<br>", 
                "<b>Promedio de Cámaras (Colonias colindantes): </b>", prettyNum(round(promedios_colonia$`Cámaras Promedio Vecinos`, digits = 1), big.mark = ","), "<br>",
                "<b>Total de Delitos (Colonias colindantes): </b>", prettyNum(promedios_colonia$`Total Delitos Vecinos`, big.mark = ","), "<br>", 
                "<b>Total de Cámaras (Colonias colindantes): </b>", prettyNum(promedios_colonia$`Total Cámaras Vecinos`, big.mark = ","), "<br>")

library(leaflet)
pal <- colorFactor(palette = "Blues",
                   domain = promedios_colonia$`Delitos Promedio Vecinos`, 
                   reverse = FALSE)

## Leaflet
mapacolonias <- leaflet(promedios_colonia) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = pal(promedios_colonia$`Delitos Promedio Vecinos`),
              popup = popup, fillOpacity = 1, weight = 0.6)

saveWidget(mapacolonias, file="mapacolonias.html")
rm(popup, pal, mapacolonias)

write_sf(promedios_colonia, "promedios_colonia.geojson")

