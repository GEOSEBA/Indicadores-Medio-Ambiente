# Cosumo agua potable por comuna
#Librerias 
library(tidyverse); library(dplyr); library(glue); library(readxl)
#Definir directorio de trabajo
home_dir <- "D:/ownCloud/Indicadores_de_Sustentabilidad_Expansion/Consumo agua potable/"
input_dir <- glue("{home_dir}Input")
output <- glue("{home_dir}Output")
#Ingresar codigo de ciudades
Consumo_agua_potable <- read_xlsx(glue("{input_dir}/consumos_clientes_2016a_2018.xlsx"))
#Filtrar consumos
Consumo_agua_potable <- filter(Consumo_agua_potable, `Año` == 2018 & `Tipo Cliente` == "Residencial") %>%
  filter(!is.na(`M3 Ap`))%>%
  group_by(Comuna) %>%
  summarise("M3_comuna" = round(sum(`M3 Ap`)))
#Ingresar codigo ciudades
codes_ciudades <- read.csv(glue("{input_dir}/Ciudades_expansion.csv")) %>%
  mutate(NOM_COMUNA = toupper(Comuna))
#Ingresar población por comuna
POB_2017 <- read.csv2(glue("{input_dir}/censo_urb2017.csv")) 
#Filtro poblacion
POB_2017 <- filter(POB_2017, Sexo == "total") %>%
  group_by(cod_com) %>%
  summarise(tot_pob = sum(Poblacion))
#Merge codigos comuna con poblacion
Pob_comuna <- merge(POB_2017, codes_ciudades, by.x = "cod_com", by.y = "Codigo") %>%
  mutate(NOM_COMUNA = toupper(Comuna))
#Merge consumo de agua ciudad y poblacion y division de consumo de litros diarios por la poblacion comunal
consumo_habitante <- merge(Pob_comuna, Consumo_agua_potable, by.x = "NOM_COMUNA", by.y = "Comuna") %>%
  select(NOM_COMUNA, M3_comuna, Ciudad, tot_pob) %>%
  group_by(NOM_COMUNA) %>%
  summarise("Litros per capita al dia" = round(sum(M3_comuna)*1000/365/mean(tot_pob),2))
#Guardar resultado
write.csv(consumo_habitante, paste({output}, "91._Consumo_agua_expansion_comunas.csv", sep = "/"), row.names = F)




