library(data.table)
library(dplyr)
library(survey)
library(stringr)

cadunico <- fread("C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/Desafio Eco II/Base/base_amostra_pessoa_201812.csv")
#municipios <- read_municipality(year=2018,showProgress=TRUE)
#estados <- read_state(year=2018,showProgress=TRUE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
cadunico <- transform(cadunico, cod_ibge=paste0(cd_ibge))
cadunico <- transform(cadunico, Pais=as.factor("Brasil"))
cadunico$Pais <- factor(x=cadunico$Pais, levels=c("Brasil"))
cadunico <- transform(cadunico, GR=as.factor(ifelse(substr(cod_ibge, start=1, stop=1)=="1","Norte",ifelse(substr(cod_ibge, start=1, stop=1)=="2","Nordeste",ifelse(substr(cod_ibge, start=1, stop=1)=="3","Sudeste",ifelse(substr(cod_ibge, start=1, stop=1)=="4","Sul",ifelse(substr(cod_ibge, start=1, stop=1)=="5","Centro-Oeste",NA)))))))
cadunico$GR <- factor(x=cadunico$GR, levels=c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"))




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



# Population of Beneficiaries Estimation
print(survey::svytotal(x=~contagem, design=svy_design, vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))

#print(survey::svybys(formula=~contagem, design=svy_design, bys = ~Pais+GR, FUN = svytotal vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
#print(survey::svybys(formula=~contagem, design=svy_design, bys = ~Pais+GR, FUN = svytotal vartype=c("se","cv"), keep.names=FALSE, na.rm=TRUE))
