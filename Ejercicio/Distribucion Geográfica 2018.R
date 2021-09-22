library(tidyverse)
library(sf)
library(spdep)

#Datos
colonias <- read_sf("~/Documents/Examen ADIP/Datos/Colonias CDMX/coloniascdmx.shp")
delito <- read.csv("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/carpetas_completa_julio_2021.csv")

## Preparamos datos de ubicacion de cámaras y ubicacion de delitos
## Dado que las bases no comparten id con el .shp de colonias, definiremos una función que asignará a cada punto el id del polígono específico al que pertenece.
## Para ello, reduciremos el número de filas para que sean más económicas las iteraciones

##ubicacion de delitos:

delitoxcolonia <- delito %>%
  filter(ao_hechos==2018, 
         !is.na(colonia_hechos),
         !is.na(longitud)) %>% 
  select(alcaldia_hechos, colonia_hechos) %>% 
  group_by(alcaldia_hechos, colonia_hechos) %>% 
  summarise(DELITOS_2018= n())

puntos_delito <- delito %>% 
  filter(ao_hechos==2018, 
         !is.na(colonia_hechos), 
         !is.na(longitud)) %>% 
  select(alcaldia_hechos, colonia_hechos, latitud, longitud) %>% 
  distinct(alcaldia_hechos, colonia_hechos, .keep_all = T)

puntos_delito <- puntos_delito %>% 
  left_join(delitoxcolonia, by= c("alcaldia_hechos", "colonia_hechos"))

puntos_delito$alcaldia_hechos[puntos_delito$alcaldia_hechos=="GUSTAVO A MADERO"]<- "GUSTAVO A. MADERO"
rm(delitoxcolonia, delito)

#La función para la asignación será: 
#1. Para cada delito ubicado en una delegación determinada, la función buscará la colonia a la que pertenece dentro del .shp, acotando la búsqueda a las colonias de esa alcaldia, y le asignará su clave

alcaldias <- unique(puntos_delito$alcaldia_hechos) #Vector con nombre de alcaldias

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
  summarise(DELITOS_2018= sum(DELITOS_2018, na.rm = T))#Lista la matriz con las cve_col del shp

##NOTA ACLARATORIA: DURANTE EL PROCEDIMIENTO HAY DOS FUENTES DE PERDIDA DE INFORMACIÓN: 
# LA PRIMERA SUCEDE CUANDO LOS REGISTROS NO TIENEN NI COLONIA O UNA COORDENADA ESPECIFICA, 
# LA SEGUNDA, POR EL CONTRARIO, SUCEDE PORQUE LA FUNCIÓN NO IDENTIFICA NINGUN POLÍGONO AL CUAL PERTENECE EL DELITO ("character(0)")
# ESTO PUEDE SUCEDER PORQUE CIERTAMENTE EXISTEN CAMARAS DEL C5 FUERA DE LAS COLONIAS DEFINIDAS POR EL SHAPE PERO 
# TAMBIEN PORQUE LOS PUNTOS ESTÁN EXACTAMENTE EN MEDIO DE DOS POLÍGONOS.
# LA FUNCIÓN MINIMIZA ESA ÚLTIMA SITUACIÓN AL UTILIZAR UN PUNTO EN COMÚN PARA TODOS LOS QUE FORMAN PARTE DE LA MISMA COLONIA, 
# PERO NO GARANTIZA QUE EL 100% DE LOS DATOS SE TRASLADEN CORRECTAMENTE. POR ELLO, TAMBIEN SE CLACULARON LOS PROMEDIOS DE COLONIAS VECINAS
# ESO DEBE MEJORARSE. 

write_excel_csv(delitos_colonias_cdmx, "delitos_colonias_cdmx_2018.csv")

rm(delitos_colonias_cdmx, colonias)
