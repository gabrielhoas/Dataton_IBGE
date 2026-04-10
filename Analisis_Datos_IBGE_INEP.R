################################################################################

# Flujo de trabajo optimizado en R para el análisis de los datos de referencia

################################################################################

# 1. Instalación de paquetes necesarios

################################################################################

rm(list = ls(all = TRUE))
options(scipen = 999)
setwd(".../Dataton") # Introduzca la ruta al directorio de referencia
if ("curl" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "curl", dependencies = TRUE)
}
library(package = "curl", verbose = TRUE)
if ("data.table" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "data.table", dependencies = TRUE)
}
library(package = "data.table", verbose = TRUE)
if ("dplyr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "dplyr", dependencies = TRUE)
}
library(package = "dplyr", verbose = TRUE)
if ("furrr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "furrr", dependencies = TRUE)
}
library(package = "furrr", verbose = TRUE)
if ("future" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "future", dependencies = TRUE)
}
library(package = "future", verbose = TRUE)
if ("httr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "httr", dependencies = TRUE)
}
library(package = "httr", verbose = TRUE)
if ("progressr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "progressr", dependencies = TRUE)
}
library(package = "progressr", verbose = TRUE)
if ("purrr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "purrr", dependencies = TRUE)
}
library(package = "purrr", verbose = TRUE)
if ("qgisprocess" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "qgisprocess", dependencies = TRUE)
}
library(package = "qgisprocess", verbose = TRUE)
if ("readr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "readr", dependencies = TRUE)
}
library(package = "readr", verbose = TRUE)
if ("rvest" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "rvest", dependencies = TRUE)
}
library(package = "rvest", verbose = TRUE)
if ("sf" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "sf", dependencies = TRUE)
}
library(package = "sf", verbose = TRUE)
if ("stringr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "stringr", dependencies = TRUE)
}
library(package = "stringr", verbose = TRUE)
if ("xlsx" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "xlsx", dependencies = TRUE)
}
library(package = "xlsx", verbose = TRUE)

################################################################################

# 2. Definir los municipios que se incluirán en el proceso de selección de datos

################################################################################

cd_municipio_referencia <- "3550308"
nm_municipio_referencia <- "São Paulo"

################################################################################

# 3. Preparación de datos del Censo Escolar del INEP

################################################################################

{
  if(file.exists(paste0(getwd(),"/censo_escolar_2022.xlsx")))
  {
    censo_escolar_2022 <- read.xlsx(file=paste0(getwd(),"/censo_escolar_2022.xlsx"), sheetName="censo_escolar_2022")
  }
  else
  {
    censo_escolar_2022 <- read.csv2(file=paste0(getwd(),"/INEP_CENSO/Microdados do Censo Escolar da Educao Bsica 2022/dados/microdados_ed_basica_2022.csv"), encoding="latin1")
    censo_escolar_2025_escola <- read.csv2(file=paste0(getwd(),"/INEP_CENSO/microdados_censo_escolar_2025/dados/Tabela_Escola_2025.csv"), encoding="latin1")
    censo_escolar_2025_escola <- censo_escolar_2025_escola[!is.na(censo_escolar_2025_escola$LATITUDE) & !is.na(censo_escolar_2025_escola$LONGITUDE) & censo_escolar_2025_escola$LATITUDE!="" & censo_escolar_2025_escola$LONGITUDE != "",]
    censo_escolar_2025_matricula <- read.csv2(file=paste0(getwd(),"/INEP_CENSO/microdados_censo_escolar_2025/dados/Tabela_Matricula_2025.csv"), encoding="latin1")
    censo_escolar_2025 <- left_join(x=censo_escolar_2025_escola, y=censo_escolar_2025_matricula, by=c("CO_ENTIDADE"="CO_ENTIDADE"))
    rm(censo_escolar_2025_escola,censo_escolar_2025_matricula)
    censo_escolar_2022 <- censo_escolar_2022[!is.na(censo_escolar_2022$CO_ENTIDADE) & !is.na(censo_escolar_2022$QT_MAT_INF) & censo_escolar_2022$QT_MAT_INF>0,c("CO_ENTIDADE","NO_ENTIDADE")]
    censo_escolar_2022 <- unique(censo_escolar_2022[,c("CO_ENTIDADE","NO_ENTIDADE")])
    censo_escolar_2025 <- censo_escolar_2025[!is.na(censo_escolar_2025$CO_ENTIDADE),c("CO_ENTIDADE","NO_REGIAO","CO_REGIAO","NO_UF","SG_UF","CO_UF","NO_MUNICIPIO","CO_MUNICIPIO","LATITUDE","LONGITUDE")]
    censo_escolar_2025 <- unique(censo_escolar_2025[,c("CO_ENTIDADE","NO_REGIAO","CO_REGIAO","NO_UF","SG_UF","CO_UF","NO_MUNICIPIO","CO_MUNICIPIO","LATITUDE","LONGITUDE")])
    censo_escolar_2022 <- left_join(x=censo_escolar_2022, y=censo_escolar_2025, by=c("CO_ENTIDADE"="CO_ENTIDADE"))
    rm(censo_escolar_2025)
    censo_escolar_2022 <- censo_escolar_2022[!is.na(censo_escolar_2022$LATITUDE) & !is.na(censo_escolar_2022$LONGITUDE) & censo_escolar_2022$LATITUDE!="" & censo_escolar_2022$LONGITUDE != "",]
    censo_escolar_2022 <- transform(censo_escolar_2022, LATITUDE=as.numeric(LATITUDE))
    censo_escolar_2022 <- transform(censo_escolar_2022, LONGITUDE=as.numeric(LONGITUDE))
    censo_escolar_2022 <- censo_escolar_2022[!is.na(censo_escolar_2022$CO_MUNICIPIO) & censo_escolar_2022$CO_MUNICIPIO==cd_municipio_referencia,]
    write.xlsx(x=censo_escolar_2022, file=paste0(getwd(),"/censo_escolar_2022.xlsx"), sheetName="censo_escolar_2022", col.names=TRUE, row.names=FALSE)
  }
}

################################################################################

# 4. Preparación de datos del Censo Demográfico del IBGE

################################################################################

{
  if(file.exists(paste0(getwd(),"/censo_demografico_2022.xlsx")))
  {
    censo_demografico_2022 <- read.xlsx(file=paste0(getwd(),"/censo_demografico_2022.xlsx"), sheetName="censo_demografico_2022")
  }
  else
  {
    list.files(path=paste0(getwd(),"/IBGE_CENSO"), pattern="Agregados_por_setores")
    setores_alfabetizacao <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_alfabetizacao_BR.csv"), encoding="latin1")
    setores_alfabetizacao <- setores_alfabetizacao[substr(x=setores_alfabetizacao$CD_setor, start=1, stop=7)==cd_municipio_referencia,]
    setores_basico <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_basico_BR_20250417.csv"), encoding="latin1")
    setores_basico <- setores_basico[substr(x=setores_basico$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_caracteristicas_domicilio1 <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_caracteristicas_domicilio1_BR.csv"), encoding="latin1")
    setores_caracteristicas_domicilio1 <- setores_caracteristicas_domicilio1[substr(x=setores_caracteristicas_domicilio1$CD_setor, start=1, stop=7)==cd_municipio_referencia,]
    setores_caracteristicas_domicilio2 <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_caracteristicas_domicilio2_BR_20250417.csv"), encoding="latin1")
    setores_caracteristicas_domicilio2 <- setores_caracteristicas_domicilio2[substr(x=setores_caracteristicas_domicilio2$setor, start=1, stop=7)==cd_municipio_referencia,]
    setores_caracteristicas_domicilio3 <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_caracteristicas_domicilio3_BR_20250417.csv"), encoding="latin1")
    setores_caracteristicas_domicilio3 <- setores_caracteristicas_domicilio3[substr(x=setores_caracteristicas_domicilio3$setor, start=1, stop=7)==cd_municipio_referencia,]
    setores_cor_raca <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_cor_ou_raca_BR.csv"), encoding="latin1")
    setores_cor_raca <- setores_cor_raca[substr(x=setores_cor_raca$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_demografia <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_demografia_BR.csv"), encoding="latin1")
    setores_demografia <- setores_demografia[substr(x=setores_demografia$CD_setor, start=1, stop=7)==cd_municipio_referencia,]
    setores_domicilios_indigenas <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_domicilios_indigenas_BR.csv"), encoding="latin1")
    setores_domicilios_indigenas <- setores_domicilios_indigenas[substr(x=setores_domicilios_indigenas$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_domicilios_quilombolas <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_domicilios_quilombolas_BR.csv"), encoding="latin1")
    setores_domicilios_quilombolas <- setores_domicilios_quilombolas[substr(x=setores_domicilios_quilombolas$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_obitos <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_obitos_BR.csv"), encoding="latin1")
    setores_obitos <- setores_obitos[substr(x=setores_obitos$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_parentesco <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_parentesco_BR.csv"), encoding="latin1")
    setores_parentesco <- setores_parentesco[substr(x=setores_parentesco$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_pessoas_indigenas <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_pessoas_indigenas_BR.csv"), encoding="latin1")
    setores_pessoas_indigenas <- setores_pessoas_indigenas[substr(x=setores_pessoas_indigenas$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_pessoas_quilombolas <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_pessoas_quilombolas_BR.csv"), encoding="latin1")
    setores_pessoas_quilombolas <- setores_pessoas_quilombolas[substr(x=setores_pessoas_quilombolas$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_renda_responsavel <- read.csv2(file=paste0(getwd(),"/IBGE_CENSO/Agregados_por_setores_renda_responsavel_BR.csv"), encoding="latin1")
    setores_renda_responsavel <- setores_renda_responsavel[substr(x=setores_renda_responsavel$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    censo_demografico_2022 <- full_join(x=setores_alfabetizacao, y=setores_basico, by=c("CD_setor"="CD_SETOR"))
    rm(setores_alfabetizacao,setores_basico)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_caracteristicas_domicilio1, by=c("CD_setor"="CD_setor"))
    rm(setores_caracteristicas_domicilio1)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_caracteristicas_domicilio2, by=c("CD_setor"="setor"))
    rm(setores_caracteristicas_domicilio2)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_caracteristicas_domicilio3, by=c("CD_setor"="setor"))
    rm(setores_caracteristicas_domicilio3)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_cor_raca, by=c("CD_setor"="CD_SETOR"))
    rm(setores_cor_raca)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_demografia, by=c("CD_setor"="CD_setor"))
    rm(setores_demografia)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_domicilios_indigenas, by=c("CD_setor"="CD_SETOR"))
    rm(setores_domicilios_indigenas)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_domicilios_quilombolas, by=c("CD_setor"="CD_SETOR"))
    rm(setores_domicilios_quilombolas)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_obitos, by=c("CD_setor"="CD_SETOR"))
    rm(setores_obitos)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_parentesco, by=c("CD_setor"="CD_SETOR"))
    rm(setores_parentesco)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_pessoas_indigenas, by=c("CD_setor"="CD_SETOR"))
    rm(setores_pessoas_indigenas)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_pessoas_quilombolas, by=c("CD_setor"="CD_SETOR"))
    rm(setores_pessoas_quilombolas)
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_renda_responsavel, by=c("CD_setor"="CD_SETOR"))
    rm(setores_renda_responsavel)
    censo_demografico_2022 <- transform(censo_demografico_2022, CD_SETOR=CD_setor)
    censo_demografico_2022 <- censo_demografico_2022[,c("CD_SETOR","V00001","V00002","V00003","V01500","V03000","V01197","V01198","V01199","V01200","V01201","V01202","V01203","V01204","V01205","V01206","V01207","V01208","V01317","V01318","V01319","V01320","V01321","V01031","V01006","V06004")]
    setores_malha <- st_read(dsn=paste0(getwd(),"/IBGE_CENSO/BR_setores_CD2022.shp"))
    setores_malha <- setores_malha[substr(x=setores_malha$CD_SETOR, start=1, stop=7)==cd_municipio_referencia,]
    setores_proj <- st_transform(x=setores_malha, crs=31983)
    pontos <- st_point_on_surface(x=setores_proj)
    pontos <- st_transform(x=pontos, crs=4326)
    coords <- st_coordinates(x=pontos)
    setores_malha$LONGITUDE <- coords[,1]
    setores_malha$LATITUDE <- coords[,2]
    rm(setores_proj,pontos,coords)
    setores_malha <- st_drop_geometry(x=setores_malha)
    setores_malha <- setores_malha[!is.na(setores_malha$CD_SETOR),c("CD_SETOR","LATITUDE","LONGITUDE")]
    setores_malha <- transform(setores_malha, CD_SETOR=as.numeric(CD_SETOR))
    censo_demografico_2022 <- full_join(x=censo_demografico_2022, y=setores_malha, by=c("CD_SETOR"="CD_SETOR"))
    rm(setores_malha)
    write.xlsx(x=censo_demografico_2022, file=paste0(getwd(),"/censo_demografico_2022.xlsx"), sheetName="censo_demografico_2022", col.names=TRUE, row.names=FALSE)
  }
}

# | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
# | Variable | Descripción                                                                                                                                                                                       |
# | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
# | V00001   | Viviendas particulares permanentes ocupadas                                                                                                                                                       |
# | V00002   | Viviendas particulares improvisadas ocupadas                                                                                                                                                      |
# | V00003   | Unidades de vivienda en viviendas colectivas con residente                                                                                                                                        |
# | V01500   | Vivienda con al menos un residente indígena                                                                                                                                                       |
# | V03000   | Vivienda con al menos un residente quilombola                                                                                                                                                     |
# | V01197   | Vivienda con persona responsable y cónyuge(s) sin hijos, sexo de la persona responsable de la vivienda es masculino                                                                               |
# | V01198   | Vivienda con persona responsable y cónyuge(s) sin hijos, sexo de la persona responsable de la vivienda es femenino                                                                                |
# | V01199   | Vivienda con persona responsable y cónyuge con hijo(s) de ambos únicamente, sexo de la persona responsable de la vivienda es masculino                                                            |
# | V01200   | Vivienda con persona responsable y cónyuge con hijo(s) de ambos únicamente, sexo de la persona responsable de la vivienda es femenino                                                             |
# | V01201   | Vivienda con persona responsable y cónyuge(s) con al menos un hijo únicamente de la persona responsable o únicamente de un(a) cónyuge, sexo de la persona responsable de la vivienda es masculino |
# | V01202   | Vivienda con persona responsable y cónyuge(s) con al menos un hijo únicamente de la persona responsable o únicamente de un(a) cónyuge, sexo de la persona responsable de la vivienda es femenino  |
# | V01203   | Vivienda con persona responsable sin cónyuge con hijo(s) y/o hijastro(s), sexo de la persona responsable de la vivienda es masculino                                                              |
# | V01204   | Vivienda con persona responsable sin cónyuge con hijo(s) y/o hijastro(s), sexo de la persona responsable de la vivienda es femenino                                                               |
# | V01205   | Vivienda con otros tipos de composición, sexo de la persona responsable de la vivienda es masculino                                                                                               |
# | V01206   | Vivienda con otros tipos de composición, sexo de la persona responsable de la vivienda es femenino                                                                                                |
# | V01207   | Unidad en vivienda colectiva con residente, sexo de la persona responsable de la vivienda es masculino                                                                                            |
# | V01208   | Unidad en vivienda colectiva con residente, sexo de la persona responsable de la vivienda es femenino                                                                                             |
# | V01317   | Color o raza es blanca                                                                                                                                                                            |
# | V01318   | Color o raza es negra                                                                                                                                                                             |
# | V01319   | Color o raza es amarilla                                                                                                                                                                          |
# | V01320   | Color o raza es mestiza                                                                                                                                                                           |
# | V01321   | Color o raza es indígena                                                                                                                                                                          |
# | V01031   | 0 a 4 años                                                                                                                                                                                        |
# | V01006   | Cantidad de residentes                                                                                                                                                                            |
# | V06004   | Valor del ingreso nominal medio mensual de las personas responsables con ingresos por viviendas particulares permanentes ocupadas                                                                 |
# | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |

################################################################################

# 5. Creación de las proporciones de variables de referencia del proyecto

################################################################################

{
  if(file.exists(paste0(getwd(),"/censo_demografico_2022_clases.xlsx")))
  {
    censo_demografico_2022_clases <- read.xlsx(file=paste0(getwd(),"/censo_demografico_2022_clases.xlsx"), sheetName="censo_demografico_2022_clases")
  }
  else
  {
    censo_demografico_2022_clases <- read.xlsx(file=paste0(getwd(),"/censo_demografico_2022.xlsx"), sheetName="censo_demografico_2022")
    censo_demografico_2022_clases <- censo_demografico_2022_clases[(!is.na(censo_demografico_2022_clases$V00001) & censo_demografico_2022_clases$V00001>0 & censo_demografico_2022_clases$V00001!="X") | (!is.na(censo_demografico_2022_clases$V00002) & censo_demografico_2022_clases$V00002>0 & censo_demografico_2022_clases$V00002!="X") | (!is.na(censo_demografico_2022_clases$V00003) & censo_demografico_2022_clases$V00003>0 & censo_demografico_2022_clases$V00003!="X"),]
    censo_demografico_2022_clases <- censo_demografico_2022_clases[(!is.na(censo_demografico_2022_clases$V01500) & censo_demografico_2022_clases$V01500=="0") | (is.na(censo_demografico_2022_clases$V01500)),]
    censo_demografico_2022_clases <- censo_demografico_2022_clases[(!is.na(censo_demografico_2022_clases$V03000) & censo_demografico_2022_clases$V03000=="0") | (is.na(censo_demografico_2022_clases$V03000)),]
    censo_demografico_2022_clases <- censo_demografico_2022_clases %>% mutate(across(c("V00001","V00002","V00003","V01500","V03000","V01197","V01198","V01199","V01200","V01201","V01202","V01203","V01204","V01205","V01206","V01207","V01208","V01317","V01318","V01319","V01320","V01321","V01031","V01006","V06004"), ~parse_number(na_if(na_if(na_if(str_replace_all(str_trim(.x), ",", "."), ""), "X"), "."))))
    censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01204=ifelse(is.na(V01204),0,V01204)/max((ifelse(is.na(V01197),0,V01197)+ifelse(is.na(V01198),0,V01198)+ifelse(is.na(V01199),0,V01199)+ifelse(is.na(V01200),0,V01200)+ifelse(is.na(V01201),0,V01201)+ifelse(is.na(V01202),0,V01202)+ifelse(is.na(V01203),0,V01203)+ifelse(is.na(V01204),0,V01204)+ifelse(is.na(V01205),0,V01205)+ifelse(is.na(V01206),0,V01206)+ifelse(is.na(V01207),0,V01207)+ifelse(is.na(V01208),0,V01208)),1))
    censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01318=(ifelse(is.na(V01318),0,V01318)+ifelse(is.na(V01320),0,V01320))/max((ifelse(is.na(V01317),0,V01317)+ifelse(is.na(V01318),0,V01318)+ifelse(is.na(V01319),0,V01319)+ifelse(is.na(V01320),0,V01320)+ifelse(is.na(V01321),0,V01321)),1))
    setores_creches <- read.csv(file=paste0(getwd(),"/No_creches_setores_2022_v3.csv"))
    censo_demografico_2022_clases <- left_join(x=censo_demografico_2022_clases, y=setores_creches, by=c("CD_SETOR"="CD_SETOR"))
    rm(setores_creches)
    censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01031=ifelse(is.na(V01031),0,V01031)/(ifelse(is.na(NUMPOINTS),0,NUMPOINTS)+1))
  }
}

################################################################################

# 6. Presentación de los diagramas de caja de las variables de referencia del proyecto

################################################################################

summary(object=censo_demografico_2022_clases$V06004)
x11()
boxplot(x=censo_demografico_2022_clases$V06004, main="Diagrama de caja del ingreso nominal mensual promedio de las personas responsables")
summary(object=censo_demografico_2022_clases$MV01204)
x11()
boxplot(x=censo_demografico_2022_clases$MV01204, main="Diagrama de caja de la proporción de hogares encabezados por una mujer sin cónyuge, con hijo(s) y/o hijastro(s)")
summary(object=censo_demografico_2022_clases$MV01318)
x11()
boxplot(x=censo_demografico_2022_clases$MV01318, main="Diagrama de caja de la proporción de personas de color o de raza negra o mestiza")
summary(object=censo_demografico_2022_clases$MV01031)
x11()
boxplot(x=censo_demografico_2022_clases$MV01031, main="Diagrama de caja de la razón entre personas de 0 a 4 años y el número de guarderías")

################################################################################

# 7. Creación de las clases de variables de referencia del proyecto

################################################################################

print(x=clases <- c(2*1212,4.5*1212))
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, V06004_clases=ifelse(V06004<=clases[[1]],1,ifelse(V06004>clases[[1]] & V06004<=clases[[2]],2,ifelse(V06004>clases[[2]],3,NA))))
table(censo_demografico_2022_clases$V06004_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_clases$MV01204, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01204_clases=ifelse(MV01204<=clases[[1]],1,ifelse(MV01204>clases[[1]] & MV01204<=clases[[2]],2,ifelse(MV01204>clases[[2]],3,NA))))
table(censo_demografico_2022_clases$MV01204_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_clases$MV01318, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01318_clases=ifelse(MV01318<=clases[[1]],1,ifelse(MV01318>clases[[1]] & MV01318<=clases[[2]],2,ifelse(MV01318>clases[[2]],3,NA))))
table(censo_demografico_2022_clases$MV01318_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_clases$MV01031, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, MV01031_clases=ifelse(MV01031<=clases[[1]],1,ifelse(MV01031>clases[[1]] & MV01031<=clases[[2]],2,ifelse(MV01031>clases[[2]],3,NA))))
table(censo_demografico_2022_clases$MV01031_clases, useNA="always")
rm(clases)
censo_demografico_2022_clases <- censo_demografico_2022_clases[,c("CD_SETOR","LATITUDE","LONGITUDE","V06004","V06004_clases","V01204","MV01204","MV01204_clases","V01318","V01320","MV01318","MV01318_clases","V01031","NUMPOINTS","MV01031","MV01031_clases")]
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, ID_CLASES_INGRESO=as.numeric(paste0(V06004_clases,MV01204_clases,MV01031_clases)))
censo_demografico_2022_clases <- transform(censo_demografico_2022_clases, ID_CLASES_COLOR=as.numeric(paste0(MV01318_clases,MV01204_clases,MV01031_clases)))
table(censo_demografico_2022_clases$ID_CLASES_INGRESO, useNA="always")
table(censo_demografico_2022_clases$ID_CLASES_COLOR, useNA="always")
write.xlsx(x=censo_demografico_2022_clases, file=paste0(getwd(),"/censo_demografico_2022_clases.xlsx"), sheetName="censo_demografico_2022_clases", col.names=TRUE, row.names=FALSE)

################################################################################

# 8. Recopilación de información sectorial a nivel de distritos

################################################################################

{
  if(file.exists(paste0(getwd(),"/censo_demografico_2022_distrito.xlsx")))
  {
    censo_demografico_2022_distrito <- read.xlsx(file=paste0(getwd(),"/censo_demografico_2022_distrito.xlsx"), sheetName="censo_demografico_2022_distrito")
  }
  else
  {
    censo_demografico_2022_distrito <- read.xlsx(file=paste0(getwd(),"/censo_demografico_2022.xlsx"), sheetName="censo_demografico_2022")
    censo_demografico_2022_distrito <- censo_demografico_2022_distrito[(!is.na(censo_demografico_2022_distrito$V00001) & censo_demografico_2022_distrito$V00001>0 & censo_demografico_2022_distrito$V00001!="X") | (!is.na(censo_demografico_2022_distrito$V00002) & censo_demografico_2022_distrito$V00002>0 & censo_demografico_2022_distrito$V00002!="X") | (!is.na(censo_demografico_2022_distrito$V00003) & censo_demografico_2022_distrito$V00003>0 & censo_demografico_2022_distrito$V00003!="X"),]
    censo_demografico_2022_distrito <- censo_demografico_2022_distrito[(!is.na(censo_demografico_2022_distrito$V01500) & censo_demografico_2022_distrito$V01500=="0") | (is.na(censo_demografico_2022_distrito$V01500)),]
    censo_demografico_2022_distrito <- censo_demografico_2022_distrito[(!is.na(censo_demografico_2022_distrito$V03000) & censo_demografico_2022_distrito$V03000=="0") | (is.na(censo_demografico_2022_distrito$V03000)),]
    censo_demografico_2022_distrito <- censo_demografico_2022_distrito %>% mutate(across(c("V00001","V00002","V00003","V01500","V03000","V01197","V01198","V01199","V01200","V01201","V01202","V01203","V01204","V01205","V01206","V01207","V01208","V01317","V01318","V01319","V01320","V01321","V01031","V01006","V06004"), ~parse_number(na_if(na_if(na_if(str_replace_all(str_trim(.x), ",", "."), ""), "X"), "."))))
    censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, CD_DIST=as.numeric(substr(x=CD_SETOR, start=1, stop=9)))
    censo_demografico_2022_distrito <- censo_demografico_2022_distrito %>%
      group_by(CD_DIST) %>%
      summarise(V00001 = sum(V00001, na.rm=TRUE),
                V00002 = sum(V00002, na.rm=TRUE),
                V00003 = sum(V00003, na.rm=TRUE),
                V01500 = sum(V01500, na.rm=TRUE),
                V03000 = sum(V03000, na.rm=TRUE),
                V01197 = sum(V01197, na.rm=TRUE),
                V01198 = sum(V01198, na.rm=TRUE),
                V01199 = sum(V01199, na.rm=TRUE),
                V01200 = sum(V01200, na.rm=TRUE),
                V01201 = sum(V01201, na.rm=TRUE),
                V01202 = sum(V01202, na.rm=TRUE),
                V01203 = sum(V01203, na.rm=TRUE),
                V01204 = sum(V01204, na.rm=TRUE),
                V01205 = sum(V01205, na.rm=TRUE),
                V01206 = sum(V01206, na.rm=TRUE),
                V01207 = sum(V01207, na.rm=TRUE),
                V01208 = sum(V01208, na.rm=TRUE),
                V01317 = sum(V01317, na.rm=TRUE),
                V01318 = sum(V01318, na.rm=TRUE),
                V01319 = sum(V01319, na.rm=TRUE),
                V01320 = sum(V01320, na.rm=TRUE),
                V01321 = sum(V01321, na.rm=TRUE),
                V01031 = sum(V01031, na.rm=TRUE),
                V01006 = sum(V01006, na.rm=TRUE),
                V06004 = mean(V06004, na.rm=TRUE))
    distritos_malha <- st_read(dsn=paste0(getwd(),"/IBGE_CENSO/BR_distritos_CD2022.shp"))
    distritos_proj <- st_transform(x=distritos_malha, crs=31983)
    pontos <- st_point_on_surface(x=distritos_proj)
    pontos <- st_transform(x=pontos, crs=4326)
    coords <- st_coordinates(x=pontos)
    distritos_malha$LONGITUDE <- coords[,1]
    distritos_malha$LATITUDE <- coords[,2]
    rm(distritos_proj,pontos,coords)
    distritos_malha <- st_drop_geometry(x=distritos_malha)
    distritos_malha <- distritos_malha[!is.na(distritos_malha$CD_DIST),c("CD_DIST","NM_DIST","LATITUDE","LONGITUDE")]
    distritos_malha <- transform(distritos_malha, CD_DIST=as.numeric(CD_DIST))
    censo_demografico_2022_distrito <- left_join(x=censo_demografico_2022_distrito, y=distritos_malha, by=c("CD_DIST"="CD_DIST"))
    rm(distritos_malha)
    setores_creches <- read.csv(file=paste0(getwd(),"/No_creches_setores_2022_v3.csv"))
    setores_creches <- transform(setores_creches, CD_DIST=as.numeric(substr(x=CD_SETOR, start=1, stop=9)))
    distritos_creches <- setores_creches %>%
      group_by(CD_DIST) %>%
      summarise(NUMPOINTS = sum(NUMPOINTS, na.rm=TRUE))
    rm(setores_creches)
    censo_demografico_2022_distrito <- left_join(x=censo_demografico_2022_distrito, y=distritos_creches, by=c("CD_DIST"="CD_DIST"))
    rm(distritos_creches)
    censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01204=ifelse(is.na(V01204),0,V01204)/max((ifelse(is.na(V01197),0,V01197)+ifelse(is.na(V01198),0,V01198)+ifelse(is.na(V01199),0,V01199)+ifelse(is.na(V01200),0,V01200)+ifelse(is.na(V01201),0,V01201)+ifelse(is.na(V01202),0,V01202)+ifelse(is.na(V01203),0,V01203)+ifelse(is.na(V01204),0,V01204)+ifelse(is.na(V01205),0,V01205)+ifelse(is.na(V01206),0,V01206)+ifelse(is.na(V01207),0,V01207)+ifelse(is.na(V01208),0,V01208)),1))
    censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01318=(ifelse(is.na(V01318),0,V01318)+ifelse(is.na(V01320),0,V01320))/max((ifelse(is.na(V01317),0,V01317)+ifelse(is.na(V01318),0,V01318)+ifelse(is.na(V01319),0,V01319)+ifelse(is.na(V01320),0,V01320)+ifelse(is.na(V01321),0,V01321)),1))
    censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01031=ifelse(is.na(V01031),0,V01031)/ifelse(is.na(NUMPOINTS),0,NUMPOINTS))
  }
}

################################################################################

# 9. Presentación de los diagramas de caja de las variables de referencia del proyecto

################################################################################

summary(object=censo_demografico_2022_distrito$V06004)
x11()
boxplot(x=censo_demografico_2022_distrito$V06004, main="Diagrama de caja del ingreso nominal mensual promedio de las personas responsables")
summary(object=censo_demografico_2022_distrito$MV01204)
x11()
boxplot(x=censo_demografico_2022_distrito$MV01204, main="Diagrama de caja de la proporción de hogares encabezados por una mujer sin cónyuge, con hijo(s) y/o hijastro(s)")
summary(object=censo_demografico_2022_distrito$MV01318)
x11()
boxplot(x=censo_demografico_2022_distrito$MV01318, main="Diagrama de caja de la proporción de personas de color o de raza negra o mestiza")
summary(object=censo_demografico_2022_distrito$MV01031)
x11()
boxplot(x=censo_demografico_2022_distrito$MV01031, main="Diagrama de caja de la razón entre personas de 0 a 4 años y el número de guarderías")

################################################################################

# 10. Creación de las clases de variables de referencia del proyecto

################################################################################

print(x=clases <- c(2.5*1212,5*1212))
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, V06004_clases=ifelse(V06004<=clases[[1]],1,ifelse(V06004>clases[[1]] & V06004<=clases[[2]],2,ifelse(V06004>clases[[2]],3,NA))))
table(censo_demografico_2022_distrito$V06004_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_distrito$MV01204, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01204_clases=ifelse(MV01204<=clases[[1]],1,ifelse(MV01204>clases[[1]] & MV01204<=clases[[2]],2,ifelse(MV01204>clases[[2]],3,NA))))
table(censo_demografico_2022_distrito$MV01204_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_distrito$MV01318, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01318_clases=ifelse(MV01318<=clases[[1]],1,ifelse(MV01318>clases[[1]] & MV01318<=clases[[2]],2,ifelse(MV01318>clases[[2]],3,NA))))
table(censo_demografico_2022_distrito$MV01318_clases, useNA="always")
print(x=clases <- quantile(x=censo_demografico_2022_distrito$MV01031, probs=c(0.33,0.67), na.rm=TRUE))
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, MV01031_clases=ifelse(MV01031<=clases[[1]],1,ifelse(MV01031>clases[[1]] & MV01031<=clases[[2]],2,ifelse(MV01031>clases[[2]],3,NA))))
table(censo_demografico_2022_distrito$MV01031_clases, useNA="always")
rm(clases)
censo_demografico_2022_distrito <- censo_demografico_2022_distrito[,c("CD_DIST","NM_DIST","LATITUDE","LONGITUDE","V06004","V06004_clases","V01204","MV01204","MV01204_clases","V01318","V01320","MV01318","MV01318_clases","V01031","NUMPOINTS","MV01031","MV01031_clases")]
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, ID_CLASES_INGRESO=as.numeric(paste0(V06004_clases,MV01204_clases,MV01031_clases)))
censo_demografico_2022_distrito <- transform(censo_demografico_2022_distrito, ID_CLASES_COLOR=as.numeric(paste0(MV01318_clases,MV01204_clases,MV01031_clases)))
table(censo_demografico_2022_distrito$ID_CLASES_INGRESO, useNA="always")
table(censo_demografico_2022_distrito$ID_CLASES_COLOR, useNA="always")
write.xlsx(x=censo_demografico_2022_distrito, file=paste0(getwd(),"/censo_demografico_2022_distrito.xlsx"), sheetName="censo_demografico_2022_distrito", col.names=TRUE, row.names=FALSE)

################################################################################