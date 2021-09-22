library(tidyverse)
library(readxl)
library(lubridate)
library(wesanderson)

#Datos
delitos <- read.csv("~/Documents/Examen ADIP/Datos/Carpetas Invs FGJ/carpetas_completa_julio_2021.csv")

top20_colonias<- delitos %>% 
  filter(ao_hechos==2020, !is.na(colonia_hechos)) %>% 
  mutate(colonia= paste(alcaldia_hechos, colonia_hechos, sep = " - ")) %>% 
  group_by(colonia) %>% summarise(Value=n()) %>% ungroup() %>% arrange(-Value) %>% 
  mutate(order= 1:n()) %>% filter(order<=20) 

delitos %>% 
  mutate(colonia= paste(alcaldia_hechos, colonia_hechos, sep = " - "),
         Año= ao_hechos) %>% 
  filter(colonia %in% top20_colonias$colonia, 
         Año>=2016) %>% 
  group_by(colonia, Año) %>%
  summarise(`Núm. Delitos`= n()) %>% 
  ggplot(aes(x= Año, y= `Núm. Delitos`, color= factor(Año)))+ 
  geom_segment(aes(x=Año, xend=Año, y=0, yend=`Núm. Delitos`), size=3) +
  geom_point(size=2)+ 
  facet_wrap(~ colonia, scales = "free")+
  geom_label(aes(x=Año, y=`Núm. Delitos`, label = prettyNum(`Núm. Delitos`, big.mark = ",")), size=3, nudge_y = 180)+
  scale_color_manual(values = wes_palette(n= 6, name = "IsleofDogs1", type = "discrete"))+
    labs(caption = "Fuente: Carpetas de investigación FGJ de la Ciudad de México (https://datos.cdmx.gob.mx/)",
       x= "",y= "", fill= NULL, color=NULL)+
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", size=20, family="Times New Roman"),
        plot.subtitle = element_text(size=15, family="Times New Roman"), 
        plot.caption = element_text(face= "italic", size=10, family="Times New Roman"))

rm(delitos, top20_colonias)
  

