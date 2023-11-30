#options(scipen = 999)

# Carga de librerías
library(ggplot2)
library(readxl)
library(xlsx)
library(readr)
library("dplyr")
library(writexl)


#--------------------AÑO 2020--------------------


#datos_pib_2020

IDH <- read_excel("indice_de_desarrollo_humano.xlsx",1)
IDH <- IDH[-c(83:85),]
IBM <- read_excel("indice_de_desarrollo_humano.xlsx", 2)
IEV <- read_excel("indice_de_desarrollo_humano.xlsx", 4)
IC <- read_excel("indice_de_desarrollo_humano.xlsx", 6)
PIB2020 <- read_excel("pib_cantonal_final.xlsx")
load("datos_prediccion_pobypatentes.Rdata")

#Filtramos el año 2020
IDH_2020 = IDH[,c(1,2,13)]
IEV_2020 = IEV[,c(1,2,13)]  #PENDIENTE: UNIR ESTOS DATAFRAMES (hay que renombrar primero el nom de "2020")
IC_2020 = IC[,c(1,2,13)]
IBM_2020 = IBM[,c(1,2,13)]


#IDH_2020_MANUAL = (IEV_2020[,3]*IC_2020[,3]*IBM_2020[,3])^(1/3)
#round(IDH_2020_MANUAL - IDH_2020[,3],6)


#----ACA PUEDO VER LAS DOS OPCIONES----

#Opción 1: Sustitución del PIBc por IBM
#Opción 2: Ponderación del IBM por PIBc



#OPCION 1-----------------------------------------------------------------------

pib_percapita_2020 = PIB2020[,c(1,2,7)]
pib_percapita_2020 = as.data.frame(pib_percapita_2020)
pib_percapita_2020$pib_cantonal_percapita = pib_percapita_2020[,3]*1000000

media = mean(pib_percapita_2020[,3])
min = min(pib_percapita_2020[,3])
max = max(pib_percapita_2020[,3])

min; media; max

pib_est_2020 = (log(pib_percapita_2020[,3])-log(min)) / (log(max)-log(min)) #aca creo que está el error
pib_est_2020

IDH_2020_PIBCANTONAL = (IEV_2020[,3]*IC_2020[,3]*pib_est_2020)^(1/3)


#OPCION 2-----------------------------------------------------------------------

IBM_ajustado_2020 = (pib_est_2020 * IBM_2020[,3])^(1/2)
IDH_2020_IMBaju = (IEV_2020[,3]*IC_2020[,3]*IBM_ajustado_2020)^(1/3)


#Comparación opcion 1 y 2
comparacion = cbind(IDH_2020, 
                    IDH_2020_PIBCANTONAL, 
                    IDH_2020_IMBaju,
                    IDH_2020[,3]-IDH_2020_PIBCANTONAL,
                    IDH_2020[,3]-IDH_2020_IMBaju)

colnames(comparacion) = c("ID_cantón", "Cantón", "IDH", 
                           "IDH ajustado PIBc", 
                           "IDH ajustado PIBc y BM",
                           "Diferencia ajuste PIB",
                           "Diferencia ajuste PIB Y BM")

#comparacion = comparacion[,-1]
#comparacion
#write.xlsx(comparacion, file = "nuevosIDH2020.xlsx", row.names = FALSE)

datos_finales <- read_excel("nuevosIDH2020.xlsx")
datos_finales <- datos_finales[order(datos_finales$ID_cantón), ]

# Crear la nueva variable "Categoría de desarrollo humano"
datos_finales <- datos_finales %>%
  mutate(Categoria_desarrollo_original = case_when(
    IDH >= 0.800 ~ "Muy alto",
    IDH >= 0.700 & IDH < 0.800 ~ "Alto",
    IDH >= 0.550 & IDH < 0.700 ~ "Medio",
    IDH < 0.550 ~ "Bajo",
    TRUE ~ "No definido"  # En caso de que ninguna de las condiciones anteriores sea verdadera
  ))


datos_finales <- datos_finales %>%
  mutate(Categoria_desarrollo_PIBc = case_when(
    `IDH ajustado PIBc` >= 0.800 ~ "Muy alto",
    `IDH ajustado PIBc` >= 0.700 & `IDH ajustado PIBc` < 0.800 ~ "Alto",
    `IDH ajustado PIBc` >= 0.550 & `IDH ajustado PIBc` < 0.700 ~ "Medio",
    `IDH ajustado PIBc` < 0.550 ~ "Bajo",
    TRUE ~ "No definido"  # En caso de que ninguna de las condiciones anteriores sea verdadera
  ))


datos_finales <- datos_finales %>%
  mutate(Categoria_desarrollo_PIBc_BM = case_when(
    `IDH ajustado PIBc y BM` >= 0.800 ~ "Muy alto",
    `IDH ajustado PIBc y BM` >= 0.700 & `IDH ajustado PIBc y BM` < 0.800 ~ "Alto",
    `IDH ajustado PIBc y BM` >= 0.550 & `IDH ajustado PIBc y BM` < 0.700 ~ "Medio",
    `IDH ajustado PIBc y BM` < 0.550 ~ "Bajo",
    TRUE ~ "No definido"  # En caso de que ninguna de las condiciones anteriores sea verdadera
  ))

table(datos_finales$Categoria_desarrollo_original)
table(datos_finales$Categoria_desarrollo_PIBc)
table(datos_finales$Categoria_desarrollo_PIBc_BM)


minimos = apply(datos_finales[,c(3:5)], 2, min)
promedios = apply(datos_finales[,c(3:5)], 2, mean)
maximos = apply(datos_finales[,c(3:5)], 2, max)

minimos; promedios; maximos

#write.xlsx(datos_finales, file = "nuevosIDH2020.xlsx", row.names = FALSE)


#____________________________________________________________________________________________________________

#datos_finales <- datos_finales[order(datos_finales$`IDH ajustado PIBc`, decreasing = TRUE), ]
#write.xlsx(datos_finales, file = "ordenadosnuevosIDH2020.xlsx", row.names = FALSE)

