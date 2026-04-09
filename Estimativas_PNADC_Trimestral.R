################################################################################

# Flujo de trabajo optimizado en R para obtener estimaciones de relevancia para el proyecto

################################################################################

# 1. Instalación de paquetes necesarios

################################################################################

rm(list = ls(all = TRUE))
options(scipen = 999)
setwd(".../Dataton")
if ("PNADcIBGE" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "PNADcIBGE", dependencies = TRUE)
}
library(package = "PNADcIBGE", verbose = TRUE)
if ("survey" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "survey", dependencies = TRUE)
}
library(package = "survey", verbose = TRUE)
if ("xlsx" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "xlsx", dependencies = TRUE)
}
library(package = "xlsx", verbose = TRUE)

################################################################################

# 2. Obtención de microdatos de la encuesta PNAD Contínua y creación de variables auxiliares

################################################################################

pnadc_trimestral <- PNADcIBGE::get_pnadc(year=2025, quarter=4, vars=c("UF","Capital","UPA","Estrato","V1008","V1014","V1022","V2003","V2005","V2007","V2009","V2010","VD3004","VD4001","VD4002","VD4019","VD4020","VD4030"))
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MV2010=as.factor(ifelse(V2010=="Branca","Branca",ifelse(V2010=="Preta" | V2010=="Parda","Preta ou parda",NA))))
pnadc_trimestral$variables$MV2010 <- factor(x=pnadc_trimestral$variables$MV2010, levels=c("Branca","Preta ou parda"))
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, VD4019real=VD4019*Habitual)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, VD4020real=VD4020*Efetivo)
pnadc_trimestral$variables <- transform(pnadc_trimestral$variables, MVD4002=as.factor(ifelse(is.na(VD4002),"Não aplicável",as.character(VD4002))))
pnadc_trimestral$variables$MVD4002 <- factor(x=pnadc_trimestral$variables$MVD4002, levels=c("Pessoas ocupadas","Pessoas desocupadas","Não aplicável"))
str(object=pnadc_trimestral)

################################################################################

# 3. Cálculo de estimaciones de referencia y exportación de resultados

################################################################################

print(x=rendimento_medio <- survey::svyby(formula=~VD4019real, by=~V2007, design=subset(pnadc_trimestral, Capital=="Município de São Paulo (SP)"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=nivel_ocupacao <- survey::svyby(formula=~(MVD4002=="Pessoas ocupadas"), denominator=~(VD4001=="Pessoas na força de trabalho" | VD4001=="Pessoas fora da força de trabalho"), by=~V2007, design=subset(pnadc_trimestral, Capital=="Município de São Paulo (SP)"), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=taxa_desocupacao <- survey::svyby(formula=~(MVD4002=="Pessoas desocupadas"), denominator=~(VD4001=="Pessoas na força de trabalho"), by=~V2007, design=subset(pnadc_trimestral, Capital=="Município de São Paulo (SP)"), FUN=svyratio, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
print(x=motivo_desalento <- survey::svyby(formula=~VD4030, by=~V2007, design=subset(pnadc_trimestral, Capital=="Município de São Paulo (SP)"), FUN=svymean, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
xlsx::write.xlsx(x=rendimento_medio, file=paste0(getwd(),"/estimativas_pnadc_trimestral.xlsx"), sheetName="rendimento_medio", append=TRUE, col.names=TRUE, row.names=FALSE)
xlsx::write.xlsx(x=nivel_ocupacao, file=paste0(getwd(),"/estimativas_pnadc_trimestral.xlsx"), sheetName="nivel_ocupacao", append=TRUE, col.names=TRUE, row.names=FALSE)
xlsx::write.xlsx(x=taxa_desocupacao, file=paste0(getwd(),"/estimativas_pnadc_trimestral.xlsx"), sheetName="taxa_desocupacao", append=TRUE, col.names=TRUE, row.names=FALSE)
xlsx::write.xlsx(x=motivo_desalento, file=paste0(getwd(),"/estimativas_pnadc_trimestral.xlsx"), sheetName="motivo_desalento", append=TRUE, col.names=TRUE, row.names=FALSE)

################################################################################
