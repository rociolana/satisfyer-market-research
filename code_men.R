####LIBRERIAS Y BASES DE DATOS PRINCIPALES####

library(tidyverse)
library(dplyr)
library(ggplot2)
data_hombres <- read_csv("SCH.csv", col_names = TRUE)
datos_hombres <- data_hombres%>%
  filter(Sexo == "Hombre")
datos_hombres <- datos_hombres %>%
  mutate(Edad = as.factor(Edad),
         Nvl_Estudios = as.factor(`Nivel de estudios alcanzados (si eres estudiante marca los que estés cursando actualmente)`),
         Nvl_Socioeconomico = as.factor(`Nivel socioeconómico`))
aliado <- datos_hombres$`Creo que el succionador de clítoris es un aliado`
mean(aliado)
median(aliado)
proporcion_aliado <- as_tibble(table(aliado))
proporcion_aliado <- proporcion_aliado%>%
  mutate(porcentajes = round(n/sum(n)*100,2))

recomend_amigo <- datos_hombres$`Usaría el succionador de clítoris con una chica si un amigo me lo recomendase`
mean(recomend_amigo)
median(recomend_amigo)
prop_recomend <- as_tibble(table(recomend_amigo))
proporcion_recomend <- prop_recomend%>%
  mutate(porcentajes = round(n/sum(n)*100,2))
37.7+35.1
intencion <- datos_hombres$`En el caso de NO haberlo comprado, ¿te lo has planteado alguna vez? (Uso en pareja, regalo...)`
mean(intencion == "Sí", na.rm = T)*100
