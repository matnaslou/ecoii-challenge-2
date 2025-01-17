##########################################################################
rm(list=ls(all=TRUE))
library(xtable)
library(dplyr)
library(survey)
library(PNADcIBGE)

# Definindo os anos para o loop
anos <- c(2016,2017,2018,2019,2022,2023)

# Lista para armazenar os resultados
resultados <- list()

ano <- 2016
# Loop para cada ano
for (ano in anos) {  
  # Construir o caminho do arquivo
  arquivo <- file.path("dados", paste0("pnad", ano, ".rds"))
  
  # Carregar a base de dados do ano
  dados <- readRDS(arquivo)
  
  # Variáveis de Interesse
  dados$nao_branco <- ifelse(dados$V2010 != "Branca" &
                             dados$V2010 != "Ignorado",1,0)
  design <- pnadc_design(dados)
  
  # Calcular o percentual de mulheres
  prop_mulheres <- svymean(~I(V2007 == "Mulher"), design = design)
  prop_raca <- svymean(~I(nao_branco == 1), design = design)
  # Armazenar os resultados
  resultados[[as.character(ano)]] <- data.frame(
    ano = ano,
    perc_mulheres = round(coef(prop_mulheres)[2],3) * 100, # Convertendo para %
    se_perc_mulheres = round(SE(prop_mulheres)[2],4),
    perc_raca = round(coef(prop_raca)[2],3) * 100
    
    )
}