####LIBRERIAS ####

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(vcd)
library(stringr)


#### Funciones####
transformar <- function(x){
  ifelse(x %in% c("Ninguno","No me se marcas la vd","-","No uso","Ninguna"),"Ninguna",
         ifelse(x=="Satisyer","Satisfyer",
                ifelse(str_detect(x,"tano|ambo") == TRUE,"Platanomelón",x)))
}

Contingency <- function(x) {
  chi <- chisq.test(x)
  unname(sqrt(chi$statistic / (chi$statistic + sum(x))))
}

#### BASES DE DATOS PRINCIPALES ####
data_mujeres <- read_csv("SCM.csv", col_names = TRUE)
datos_mujeres <- data_mujeres%>%
  mutate(Edad = as.factor(Edad),
         Nvl_Estudios = as.factor(`Nivel de estudios alcanzados si eres estudiante marca los que estés cursando actualmente`),
         `Nivel socieconómico` = as.factor(`Nivel socioeconómico`))

datos_mujeres <- datos_mujeres%>%
  filter(Sexo == "Mujer")

datos_mujeres_si <- datos_mujeres%>%
  filter(`¿Has comprado alguna vez un succionador de clítoris?` == "Sí")

datos_mujeres_no <- datos_mujeres%>%
  filter(`¿Has comprado alguna vez un succionador de clítoris?` == "No")


####PORCENTAJE DE COMPRA####

porcent_comprado <- mean(datos_mujeres$`¿Has comprado alguna vez un succionador de clítoris?`== "Sí")*100
porcent_no_comprado <- 100- porcent_comprado

tabla_porcentajes <- as_tibble(data.frame("Sí" = porcent_comprado, "No" = porcent_no_comprado))

porcent_comprado_pareja <- datos_mujeres%>%
  group_by(`Estado civil`)%>%
  summarize(Compra = mean(`¿Has comprado alguna vez un succionador de clítoris?`== "Sí")*100)
barras_percent_pareja <- porcent_comprado_pareja%>%
  ggplot(aes(`Estado civil`, Compra))+
  geom_bar(stat = "identity", fill ="pink")+
  theme_economist()+
  scale_fill_economist()+
  ggtitle("Porcentaje de Compra según el Estado civil")

porcent_comprado_edad <- datos_mujeres%>%
  group_by(Edad)%>%
  summarize(Compra = mean(`¿Has comprado alguna vez un succionador de clítoris?`== "Sí")*100)

test_compra <- datos_mujeres%>%
  group_by(Edad)%>%
  select(`¿Has comprado alguna vez un succionador de clítoris?`)

tabla_test_compra <- table(test_compra$`¿Has comprado alguna vez un succionador de clítoris?`,test_compra$Edad)
test_chi_compra <- chisq.test(tabla_test_compra)
test_chi_compra
test_chi_compra$residuals
contingencia_compra <- Contingency(tabla_test_compra)
contingencia_compra

compra_grupos <- tabla_test_compra[,c(1,4)]
test_chi_grupos <- chisq.test(compra_grupos)
assocstats(compra_grupos)

####INTENCIÓN DE COMPRA####

intencion_compra <- mean(datos_mujeres_no$`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...` == "Sí")*100
intencion_compra_edad <- datos_mujeres_no%>%
  group_by(Edad)%>%
  summarize(Intención = mean(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...` == "Sí")*100)
graf_intencion_edad <- intencion_compra_edad%>%
  ggplot(aes(Edad, Intención))+
  geom_bar(stat = "identity", fill = "pink")+
  theme_economist()+
  scale_fill_economist()+
  ggtitle("Intención de compra según la Edad")

test <- datos_mujeres_no%>%
  group_by(Edad)%>%
  select(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`)

tabla_test <- table(test$`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`,test$Edad)

test_chi <- chisq.test(tabla_test, simulate.p.value = TRUE)
test_chi$statistic
test_chi$residuals
contingencia <- Contingency(tabla_test)



test_fisher <- datos_mujeres_no%>%
  select(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`,Edad)

tabla_fisher <- table(test_fisher$`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`, test_fisher$Edad)
tabla_fisher_2 <- tabla_fisher[,c(1,4)]


fisher <- fisher.test(tabla_fisher_2)
fisher
assocstats(tabla_fisher_2)


####TOP OF MIND####

datos_marcas <- datos_mujeres%>%
  mutate(Marca = `¿Qué marcas de succionadores de clítoris conoces?`)%>%
  select(Marca)
Satisfyer <- sum(str_count(datos_marcas$Marca,"Satis"))/nrow(datos_marcas)*100
Womanizer <- sum(str_count(datos_marcas$Marca,"Wom"))/nrow(datos_marcas)*100
Lover_Yoyo <- sum(str_count(datos_marcas$Marca,"Lover"))/nrow(datos_marcas)*100
Lelo <- sum(str_count(datos_marcas$Marca,"Lelo"))/nrow(datos_marcas)*100
We_Vibe <- sum(str_count(datos_marcas$Marca,"We"))/nrow(datos_marcas)*100
Platanomelón <- (sum(str_count(datos_marcas$Marca,"tano|ambo"))-6)/nrow(datos_marcas)*100
Ninguna <- sum(str_count(datos_marcas$Marca,"No|Nin"))/nrow(datos_marcas)*100
Amantis <- sum(str_count(datos_marcas$Marca,"Amantis"))/nrow(datos_marcas)*100
Otras <- sum(str_count(datos_marcas$Marca,"Tra|fid|Ku|Rit"))/nrow(datos_marcas)*100

Marcas <- tibble(Satisfyer,Womanizer,Lover_Yoyo,Lelo,We_Vibe,Platanomelón,Ninguna,Amantis,Otras)


####Hipotesis 2####

# Siempre estoy dispuesta a escuchar opiniones aunque sean contrarias a las mías
# Entiendo y respeto puntos de vista que no son iguales que los míos.

personalidad_c <- datos_mujeres_si%>%
  filter(`En el caso de SÍ haberlo comprado, ¿con qué fin?` %in% c("Uso personal", "Ambas opciones"))%>%
  select(`Siempre estoy dispuesta a escuchar opiniones aunque sean contrarias a las mías`,
         `Entiendo y respeto puntos de vista que no son iguales que los míos.`, Edad)%>%
  na.omit()
personalidad_c$Media_per <- apply(personalidad_c[,c(1,2)],1,mean) #para hacer la media por filas

#se puede hacer tambien: personalidad_c$Media_per <- rowSums(personalidad_c[,c(1,2)])
perss <- round(mean(personalidad_c$Media_per),2)
(personalidad_c$Media_per)
mediana1 <- median(personalidad_c$Media_per)
mediana2 <- median(perso_n$media_per)
mediana3 <- median(perso_s$media_per)

personalidad_ <- datos_mujeres_no%>%
  select(`Siempre estoy dispuesta a escuchar opiniones aunque sean contrarias a las mías_1`,
         `Entiendo y respeto puntos de vista que no son iguales que los míos._1`,
         `¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`)%>%
  mutate(intencion = `¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`)
personalidad_$media_per <- apply(personalidad_[,c(1,2)],1,mean)
personalidad_n <- personalidad_%>%
  group_by(intencion)
personalidad_nc <- personalidad_n%>%
  summarize(Media_Personalidad = mean(media_per))
perso_s <- personalidad_n%>%
  filter(intencion == "Sí")
perso_n<-personalidad_n%>%
  filter(intencion=="No")

Tabla_Personalidad <- tibble(Grupo= c("Comprado","Int.No","Int.Fav") ,Personalidad = c(perss,personalidad_nc$Media_Personalidad))
Tabla_Personalidad2 <- tibble(Grupo = c("Comprado","Int.No","Int.Fav"), Personalidad = c(mediana1,mediana2,mediana3))

test_no <- t.test(perso_n$media_per,perso_s$media_per)

test_dif <- t.test(personalidad_c$Media_per,perso_n$media_per)


tabu_s <- datos_mujeres_si$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús.`%>%na.omit

tabu_n <- datos_mujeres_no$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`
tabu_n_n <- datos_mujeres_no%>%
  filter(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`=="No")%>%
  select(`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`)
tabu_n_n<-tabu_n_n$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`

tabu_n_s <- datos_mujeres_no%>%
  filter(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`=="Sí")%>%
  select(`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`)
tabu_n_s<-tabu_n_s$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`
####Hipotesis 2 prueba de normalidad####

##Variables definidas 
personalidad_c <- datos_mujeres_si%>%
  filter(`En el caso de SÍ haberlo comprado, ¿con qué fin?` %in% c("Uso personal", "Ambas opciones"))%>%
  select(`Siempre estoy dispuesta a escuchar opiniones aunque sean contrarias a las mías`,
         `Entiendo y respeto puntos de vista que no son iguales que los míos.`, Edad)%>%
  na.omit()
personalidad_c$Media_per <- apply(personalidad_c[,c(1,2)],1,mean) #para sumar por filas

personalidad_ <- datos_mujeres_no%>%
  select(`Siempre estoy dispuesta a escuchar opiniones aunque sean contrarias a las mías_1`,
         `Entiendo y respeto puntos de vista que no son iguales que los míos._1`,
         `¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`)%>%
  mutate(intencion = `¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`)
personalidad_$media_per <- apply(personalidad_[,c(1,2)],1,mean)

personalidad_n <- personalidad_%>%
  group_by(intencion)
perso_s <- personalidad_n%>%
  filter(intencion == "Sí")
perso_n<-personalidad_n%>%
  filter(intencion=="No")
#supuestos de normalidad personalidad
#personalidad_c
p <- seq(0.05, 0.95, 0.05)
sample_quantile_pc <- quantile(personalidad_c$Media_per,p)
theorical_quantile_pc <- qnorm(p,mean=mean(personalidad_c$Media_per, 
                                           sd = sd(personalidad_c$Media_per)))
qq_personalidad_c <- qplot(theorical_quantile_pc,sample_quantile_pc, color="pink")+
  geom_abline()+
  theme(legend.position = "none")

shapiro_pc <- shapiro.test(personalidad_c$Media_per)
#perso_s
sample_quantile_ps <- quantile(perso_s$media_per,p)
theorical_quantile_ps <- qnorm(p,mean=mean(perso_s$media_per, 
                                           sd = sd(person_s$media_per)))
qq_perso_s <- qplot(theorical_quantile_ps,sample_quantile_ps,color = "pink")+
  geom_abline()+
  theme(legend.position = "none")

shapiro_ps <- shapiro.test(perso_s$media_per)


#perso_n
sample_quantile_pn <- quantile(perso_n$media_per,p)
theorical_quantile_pn <- qnorm(p,mean=mean(perso_n$media_per, 
                                           sd = sd(person_n$media_per)))
qq_perso_n <- qplot(theorical_quantile_pn,sample_quantile_pn,color = "pink")+
  geom_abline()+
  theme(legend.position = "none")
shapiro_pn <- shapiro.test(perso_n$media_per)

#test Mann_Whitney
test_MW_1 <- wilcox.test(personalidad_c$Media_per,perso_n$media_per)
test_MW_2 <- wilcox.test(perso_n$media_per,perso_s$media_per)

# TABUS
tabu_s <- datos_mujeres_si$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús.`%>%na.omit
tabu_n <- datos_mujeres_no$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`
tabu_n_n <- datos_mujeres_no%>%
  filter(`¿Te has planteado alguna vez comprar un succionador de clítoris? Uso personal, regalo...`=="No")%>%
  select(`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`)
tabu_n_n<-tabu_n_n$`Creo que este tipo de productos debería estar más presente en los medios de comunicación eliminando así los tabús._1`

mean(tabu_s)
median(tabu_s)
hist(tabu_s)
mean(tabu_n)
median(tabu_n)
hist(tabu_n)
test_tabu <- t.test(tabu_s,tabu_n,mu=0)
test_tabu2 <- t.test(tabu_s,tabu_n_n)
median(tabu_n_n)


sample_quantile_ts <- quantile(tabu_s,p)
theorical_quantile_ts <- qnorm(p,mean=mean(tabu_s, 
                                           sd = sd(tabu_s)))
qq_ts <-qplot(theorical_quantile_ts,sample_quantile_ts,color = "pink")+
  geom_abline()+
  theme(legend.position = "none")
shapiro_ts <- shapiro.test(tabu_s)

sample_quantile_tn <- quantile(tabu_n,p)
theorical_quantile_tn <- qnorm(p,mean=mean(tabu_n, 
                                           sd = sd(tabu_n)))
qq_tn <-qplot(theorical_quantile_tn,sample_quantile_tn,color = "pink")+
  geom_abline()+
  theme(legend.position = "none")
shapiro_tn <- shapiro.test(tabu_n)

sample_quantile_tnn <- quantile(tabu_n_n,p)
theorical_quantile_tnn <- qnorm(p,mean=mean(tabu_n_n, 
                                            sd = sd(tabu_n_n)))
qq_tnn <-qplot(theorical_quantile_tnn,sample_quantile_tnn,color = "pink")+
  geom_abline()+
  theme(legend.position = "none")
shapiro_tnn <- shapiro.test(tabu_n_n)

#TESTS
test_MW_tabu1 <- wilcox.test(tabu_s,tabu_n)
test_MW_tabu2 <- wilcox.test(tabu_s,tabu_n_n)
test_MW_tabu3 <- wilcox.test(tabu_n_n,tabu_n_s)
####Hipotesis 3####

recomendacion_cerc <- datos_mujeres_si$`Antes de comprarlo tuve en cuenta las recomendaciones de mis amigas/gente cercana`
recomendacion_exp <- datos_mujeres_si$`Tuve en cuenta la opinión de expertos, sexologos, dependientes de sex shops, doctores... a la hora de comprarlo`  
recomendacion_no <- datos_mujeres_no$`No me lo compraría a no ser que alguien me lo recomendara`
recomendacion_inf <- datos_mujeres_no$`Me plantearía comprar el producto si me lo recomendase mi influencer de confianza`

Media_cerc <- mean(recomendacion_cerc)
Media_exp <- mean(recomendacion_exp, na.rm = T)
Media_no <- mean(recomendacion_no)
Media_inf <- mean(recomendacion_inf)
Tabla_medias <- tibble(Media_cerc,Media_exp,Media_no,Media_inf)

Mediana_cerc <- median(recomendacion_cerc)
Mediana_exp <- median(recomendacion_exp, na.rm = T)
Mediana_no <- median(recomendacion_no)
Mediana_inf <- median(recomendacion_inf)

Tabla_medianas <- tibble(Mediana_cerc,Mediana_exp,Mediana_no,Mediana_inf)

hist(recomendacion_cerc)
hist(recomendacion_exp)
hist(recomendacion_no)
hist(recomendacion_inf)

Porcentaje_cercanos <- mean(recomendacion_cerc >= 3)*100




####Hipotesis 5####
# Aproveché una oferta Black Friday, navidades, precio rebajado... para comprarme mi succionador de clítoris
oferta <- datos_mujeres_si$`Aproveché una oferta Black Friday, navidades, precio rebajado... para comprarme mi succionador de clítoris`
media_of <- mean(oferta,na.rm = TRUE)
mediana_ofer <- median(oferta,na.rm = TRUE)

Porcentaje_oferta <- round(mean(oferta > 3 ,na.rm = TRUE)*100,2)

####Hipotesis 6####

preciosmaxs <- na.omit(as.factor(datos_mujeres_si$`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`))
porcentajes_preciosmaxs <- as_tibble(table(preciosmaxs))
porcentajes_preciosmaxs <- porcentajes_preciosmaxs%>%
  mutate(porcentaje = n/sum(n)*100)

grafico_preciomax <- porcentajes_preciosmaxs%>%
  ggplot(aes(preciosmaxs,porcentaje))+
  geom_bar(stat = "identity", fill = "pink")+
  ggtitle("¿Qué precio máximo pagarías?" )+
  theme_economist()+
  coord_flip()


preciosmins <- na.omit(as.factor(datos_mujeres_si$`Por debajo de qué precio no comprarías un succionador de clítoris`))
porcentajes_preciosmins <- as_tibble(table(preciosmins))
porcentajes_preciosmins <- porcentajes_preciosmins%>%
  mutate(porcentaje = n/sum(n)*100)

grafico_preciomin <- porcentajes_preciosmins%>%
  ggplot(aes(preciosmins,porcentaje))+
  geom_bar(stat = "identity", fill = "pink")+
  ggtitle("Por debajo de qué precio No lo comprarías" )+
  theme_economist()+
  coord_flip()


preciomaxn <- na.omit(as.factor(datos_mujeres_no$`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?_1`))
porcentajes_maxn <- as_tibble(table(preciomaxn))
porcentajes_maxn <- porcentajes_maxn%>%
  mutate(porcentaje = n/sum(n)*100)

grafico_preciomaxn <- porcentajes_maxn%>%
  ggplot(aes(preciomaxn,porcentaje))+
  geom_bar(stat = "identity", fill = "pink")+
  ggtitle("¿Qué precio máximo pagarías?" )+
  theme_economist()+
  coord_flip()


preciominn <- na.omit(as.factor(datos_mujeres_no$`Por debajo de qué precio no comprarías un succionador de clítoris_1`))
porcentajes_minn <- as_tibble(table(preciominn))
porcentajes_minn <- porcentajes_minn%>%
  mutate(porcentaje = n/sum(n)*100)

grafico_preciominn <- porcentajes_minn%>%
  ggplot(aes(preciominn,porcentaje))+
  geom_bar(stat = "identity", fill = "pink")+
  ggtitle("Por debajo de qué precio No lo comprarías" )+
  theme_economist()+
  coord_flip()

#### precios-nvl.socioeconomico
bajo <- datos_mujeres_si%>%
  filter(`Nivel socioeconómico` == "Bajo / Medio-bajo")%>%
  select(`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`, 
         `Por debajo de qué precio no comprarías un succionador de clítoris` )
bajo <- na.omit(bajo)
bajo_max <- bajo$`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`
bajo_min <- bajo$`Por debajo de qué precio no comprarías un succionador de clítoris`
bajo_max <- as_tibble(table(bajo_max))
as_tibble(table(bajo_min))


medio <- datos_mujeres_si%>%
  filter(`Nivel socioeconómico` == "Medio")%>%
  select(`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`, 
         `Por debajo de qué precio no comprarías un succionador de clítoris` )
medio <- na.omit(medio)
medio_max <- medio$`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`
medio_min <- medio$`Por debajo de qué precio no comprarías un succionador de clítoris`
as_tibble(table(medio_max))
as_tibble(table(medio_min))


alto <- datos_mujeres_si%>%
  filter(`Nivel socioeconómico` == "Alto / Medio-alto")%>%
  select(`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`, 
         `Por debajo de qué precio no comprarías un succionador de clítoris` )
alto <- na.omit(alto)
alto_max <- alto$`¿Qué precio como máximo estarías dispuesta a pagar por un succionador de clítoris?`
alto_min <- alto$`Por debajo de qué precio no comprarías un succionador de clítoris`
as_tibble(table(alto_max))
as_tibble(table(alto_min))

gbmax <- bajo_max %>%
  ggplot(aes(bajo_max,n))+
  geom_bar(stat = "identity", fill = "pink")+
  theme_economist()+
  ggtitle("Precio más alto que pagarían (Nvl.Bajo)")+
  coord_flip()
