library(data.table)
library(geobr)
library(dplyr)
library(sf)
library(survey)
library(stringr)

cadunico <- fread("Base/base_amostra_pessoa_201812.csv")
#municipios <- read_municipality(year=2018,showProgress=TRUE)
#estados <- read_state(year=2018,showProgress=TRUE)

# Adjusting geobr data for merging
#municipios$cd_ibge <- municipios$code_muni
#municipios$code_state <- as.numeric(municipios$code_state)
#municipios = subset(municipios, select= c(cd_ibge,code_state))
#municipios <- st_drop_geometry(municipios)
#estados = subset(estados, select =c(code_state,abbrev_state,name_state,name_region))
#estados <- st_drop_geometry(estados)


#base_merged <- merge(cadunico, municipios, by = "cd_ibge")
#base_merged <- merge(base_merged, estados, by = c("code_state"))

# Aplicar as transformações
cadunico <- cadunico %>%
  mutate(peso.pes = str_pad(peso.pes, 15, side = "right", pad = "0"), # Preenchendo a string
         peso.pes = as.numeric(peso.pes) * 1e-14)  # Convertendo para numérico e multiplicando


svy_design <- svydesign(
  id = ~id_familia,                # coluna para unidade primária de amostragem (PSU)
  strata = ~estrato,        # coluna para o estrato
  weights = ~peso.pes,          # coluna para os pesos amostrais
  data = cadunico,              # o dataframe de dados
  nest = TRUE               # se é um desenho aninhado (múltiplos estágios)
)


# Variable for Counting Observations
svy_design <- transform(svy_design, contagem=1)
svy_design <- subset(svy_design, ind_frequenta_escola_memb==1 & cod_curso_frequenta_memb == 7 &
                     idade >= 14 & idade <= 24)

#svy_design <- transform(svy_design, Pais="Brasil")
#svy_design <- transform(svy_design, Pais=as.factor("Brasil"))
#svy_design$Pais <- factor(x=svy_design$Pais, levels=c("Brasil"))


# Population of Beneficiaries Estimation
print(survey::svytotal(x=~contagem, design=svy_design, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
