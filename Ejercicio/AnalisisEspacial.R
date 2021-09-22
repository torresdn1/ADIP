library(tidyverse)
library(sf)
library(spdep)
library(readxl)

#Datos geo generados en el script: Distribución Geográfica
colonias <- read_sf("~/Documents/Examen ADIP/Datos/geojson/promedios_colonia.geojson")
colonias <- colonias %>% filter(nombre!="PARRES EL GUARDA (PBLO)") #Quitamos colonia sin colindancias
delito2018 <- read.csv("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/delitos_colonias_cdmx_2018.csv") %>% select(-c(alcaldia_hechos))
pob <- read_excel("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/Poblacion CDMX.xlsx", sheet = "Alcaldias")
pob$Alcaldia[pob$Alcaldia=="GUSTAVO A MADERO"]<- "GUSTAVO A. MADERO"

colonias <- colonias %>% 
  left_join(delito2018, by= "cve_col")

colonias$DELITOS_2018[is.na(colonias$DELITOS_2018)]<- 0

##Creamos tasas de incidencia por año y diferencia entre tasas de 2018 y 2020
pob <- pob %>% 
  filter(Año %in% c("2018", "2020")) %>% 
  spread(key= Año, value= Poblacion_15_mas) %>% 
  rename(Poblacion_15_mas2018= `2018`, 
         Poblacion_15_mas2020= `2020`, 
         alcaldi= Alcaldia
         )

colonias <- colonias %>% 
  left_join(pob, by = "alcaldi")
rm(delito2018, pob)

##Tasas
colonias <- colonias %>%
  mutate(TasaInc2020= round(100000*(DELITOS/Poblacion_15_mas2020), digits = 2),
         TasaInc2018= round(100000*(DELITOS_2018/Poblacion_15_mas2018), digits = 2), 
         Dif= TasaInc2018- TasaInc2020) 

##Analisis espacial 
neighbours <- poly2nb(colonias$geometry, row.names = NULL, 
                      snap=sqrt(.Machine$double.eps),
                      queen=TRUE, useC=TRUE, foundInBox=NULL)

#I Moran Globales
moran.test(colonias$TasaInc2018, nb2listw(neighbours))
moran.test(colonias$TasaInc2020, nb2listw(neighbours))

#Focos Rojos
#2018

a <- colonias %>% 
  mutate(lmoran = localmoran(x = TasaInc2020, listw = nb2listw(neighbours, style = "B"))[, 1],
         lmoran_pval = localmoran(x = TasaInc2020, listw = nb2listw(neighbours, style = "B"))[, 5]
  )

a <- a %>% 
  mutate(
    # Estandarizar el No Delitos y el Moran local a sus valores medios:
    st_delito = TasaInc2020 - mean(TasaInc2020),
    st_lmoran = lmoran - mean(lmoran),
    # Crear la nueva variable categórica:
    cuadrante = ifelse(lmoran_pval > 0.10, "Insignificante",
                       ifelse(lmoran_pval > 0.05 & lmoran_pval<=0.10, case_when(st_delito > 0 & st_lmoran > 0 ~ "Alto-Alto (10%)",
                                                                                st_delito < 0 & st_lmoran < 0 ~ "Bajo-Bajo (10%)",
                                                                                st_delito < 0 & st_lmoran > 0 ~ "Bajo-Alto (10%)",
                                                                                st_delito > 0 & st_lmoran < 0 ~ "Alto-Bajo (10%)"),
                              ifelse(lmoran_pval<= 0.05 & lmoran_pval> 0.01, case_when(st_delito > 0 & st_lmoran > 0 ~ "Alto-Alto (5%)",
                                                                                        st_delito < 0 & st_lmoran < 0 ~ "Bajo-Bajo (5%)",
                                                                                        st_delito < 0 & st_lmoran > 0 ~ "Bajo-Alto (5%)",
                                                                                        st_delito > 0 & st_lmoran < 0 ~ "Alto-Bajo (5%)"),
                                     ifelse(lmoran_pval>= 0 & lmoran_pval<= 0.01, case_when(st_delito > 0 & st_lmoran > 0 ~ "Alto-Alto (1%)",
                                                                                         st_delito < 0 & st_lmoran < 0 ~ "Bajo-Bajo (1%)",
                                                                                         st_delito < 0 & st_lmoran > 0 ~ "Bajo-Alto (1%)",
                                                                                         st_delito > 0 & st_lmoran < 0 ~ "Alto-Bajo (1%)"), NA)))))
a$cuadrante <- factor(a$cuadrante, levels = c("Alto-Alto (1%)", "Alto-Alto (5%)", "Alto-Alto (10%)"))

ggplot(data= a, aes(fill = cuadrante)) +
  geom_sf()+
  scale_fill_manual(values = c("red", "orange" ,"yellow", "white"), na.translate = F)+
  labs(fill= NULL,
       x="", y="")+
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  theme_void()

a <- a %>% mutate(foco= ifelse(is.na(cuadrante), "No Foco Rojo", "Foco Rojo"))

##Modelo de regresión
library(sjPlot) 

modelo <- lm(data=a, Dif~ Cámaras.Promedio.Vecinos + factor(foco))
plot_model(modelo, type="pred", terms= c("Cámaras.Promedio.Vecinos", "foco")) +
  labs(y= "Diferencia (Tasa 2018- Tasa 2020", x= "Número de Cámaras Promedio entre Colonias Vecinas", 
       title= "")+
  theme_minimal()+ 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text= element_text(family="Times New Roman", size= 12),
        legend.text = element_text(size=12, family="Times New Roman"),
        axis.text.y = element_text(face = "bold", size=10, family="Times New Roman"),
        axis.ticks.x = element_blank())
