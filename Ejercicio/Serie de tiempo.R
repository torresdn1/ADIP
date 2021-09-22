library(tidyverse)
library(readxl)
library(lubridate)

#Datos
pob <- read_excel("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/Poblacion CDMX.xlsx", sheet = "Alcaldias")
delitos <- read.csv("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/carpetas_completa_julio_2021.csv")

#Manejo de datos: Construiremos un nuevo objeto con la tasa de incidencia mensual por delegación
delitos <- delitos %>% 
  filter(alcaldia_hechos %in% c("CUAUHTEMOC","IZTAPALAPA","GUSTAVO A MADERO","BENITO JUAREZ",
                                "ALVARO OBREGON","COYOACAN","MIGUEL HIDALGO","TLALPAN",
                                "VENUSTIANO CARRANZA","AZCAPOTZALCO","IZTACALCO","XOCHIMILCO",
                                "TLAHUAC","LA MAGDALENA CONTRERAS","CUAJIMALPA DE MORELOS","MILPA ALTA"),
         ao_hechos %in% c(2016, 2017, 2018, 2019, 2020), 
         !is.na(alcaldia_hechos)) %>% 
  select(fecha_hechos, ao_hechos, mes_hechos, alcaldia_hechos) %>% 
  mutate(date= as.Date(fecha_hechos, format ="%Y-%m-%d %H:%M:%S"))

#### TOTAL DE DELITOS #### 
delitoxalcaldia<- delitos %>% 
  group_by(ao_hechos, alcaldia_hechos, month= floor_date(date, "month")) %>%
  summarise(Delito=n()) %>% 
  ungroup() %>% 
  mutate(key= paste(ao_hechos, alcaldia_hechos, sep = "/")) %>% 
  select(-c(ao_hechos, alcaldia_hechos))

pob <- pob %>%  mutate(key= paste(Año, Alcaldia, sep = "/")) %>% select(key, Poblacion_15_mas)

data <- merge(delitoxalcaldia, pob, by.x = "key", by.y = "key", all=F)

rm(delitos, delitoxalcaldia, pob)

data %>% separate(key, c("Año", "Alcaldía"), sep = "/") %>% 
  mutate(TasaInc= round(100000*(Delito/Poblacion_15_mas), digits = 2)) %>% 
  ggplot(aes(x= month, y= TasaInc, color=Alcaldía))+ 
  geom_line()+ 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  facet_wrap(~Alcaldía, scales = "free")+
  labs(caption = "\nFuente: Carpetas de investigación FGJ de la Ciudad de México (https://datos.cdmx.gob.mx/)",
       x= "",y= "Tasa de Incidencia")+
  theme_minimal()+ 
  geom_smooth(method = 'loess', level=0.95)+
  theme(legend.position = "none",
        text= element_text(family="Times New Roman", size= 12),
        legend.title = element_text(size=12, family="Times New Roman"),
        legend.text = element_text(size=12, family="Times New Roman"),
        axis.text.y = element_text(face = "bold", size=10, family="Times New Roman"),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"),
        plot.title = element_text(face = "bold", size=20, family="Times New Roman"),
        plot.subtitle = element_text(size=15, family="Times New Roman"), 
        plot.caption = element_text(face= "italic", size=12, family="Times New Roman"))

rm(data)
