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
  
  if (ano == 2023) {
    dados$salariominimo_proprioano <- 1320
    
  }
  
  if (ano == 2017) {
    dados$salariominimo_proprioano <- 937
  }
  
  if (ano == 2016) {
    dados$salariominimo_proprioano <- 880
  }
  
  if (ano == 2018) {
    dados$salariominimo_proprioano <- 954
  }
  if (ano == 2019) {
    dados$salariominimo_proprioano <- 998
  }
  if (ano == 2020) {
    dados$salariominimo_proprioano <- 1039
  }
  if (ano == 2021) {
    dados$salariominimo_proprioano <- 1100
  }
  if (ano == 2022) {
    dados$salariominimo_proprioano <- 1212
  }
  
  
  dados$nao_branco <- ifelse(dados$V2010 != "Branca" &
                             dados$V2010 != "Ignorado",1,0)
  dados$contagem <- 1
  
  dados$PdM <- ifelse((dados$V2009 >= 14 & dados$V2009 <= 24) &
                         dados$V3002 == "Sim" &
                         dados$V3002A == "Rede pública" &
                         dados$V3003A == "Regular do ensino médio" &
      (
        (
          dados$VD5008real_proprioano <= dados$salariominimo_proprioano / 2 &
           !is.na(dados$VD5008real_proprioano)
        ) |
          dados$V5001A == "Sim" | dados$V5002A == "Sim" | dados$V5003A == "Sim"
      ) &
      dados$VD2004 != "Unipessoal",
    1,  # Valor se as condições forem satisfeitas
    0   # Valor se as condições não forem satisfeitas
  )
  
  design <- pnadc_design(dados)
  
  # Calcular o percentual de mulheres
  totais <- svybys(formula = ~PdM, by = ~Pais + GR + UF, design = design,
                   FUN = svytotal)
  prop_mulheres <- svymean(~I(V2007 == "Mulher"), design = design)
  prop_raca <- svymean(~I(nao_branco == 1), design = design)
  # Armazenar os resultados
  resultados[[as.character(ano)]] <- data.frame(
    ano = ano,
    perc_mulheres = round(coef(prop_mulheres)[2],3) * 100, # Convertendo para %
    se_perc_mulheres = round(SE(prop_mulheres)[2],4),
    perc_raca = round(coef(prop_raca)[2],3) * 100,
    total_br = coef(totais[[1]])[1],
    se_tot_br = SE(totais[[1]])[1],
    total_norte = coef(totais[[2]])[1],
    se_tot_norte = round(SE(totais[[2]])[1],4),
    total_nordeste = coef(totais[[2]])[2],
    se_tot_nordeste = round(SE(totais[[2]])[2],4),
    total_sudeste = coef(totais[[2]])[3],
    se_tot_sudeste = round(SE(totais[[2]])[3],4),
    total_sul = coef(totais[[2]])[4],
    se_tot_sul = round(SE(totais[[2]])[4],4),
    total_co = coef(totais[[2]])[5],
    se_tot_co = round(SE(totais[[2]])[5],4)
  )
  
}