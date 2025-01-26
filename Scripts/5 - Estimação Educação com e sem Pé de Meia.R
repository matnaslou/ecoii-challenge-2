# Estimação da Educação:
library(dplyr)
library(fastDummies)
library(stringr)
# Vetor de nomes de arquivo
arquivos <- c("G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2016.rds", 
              "G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2017.rds", 
              "G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2018.rds", 
              "G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2019.rds",
              "G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2022.rds",
              "G:/Meu Drive/ecoii-challenge-2/base_unica/pnad2023.rds")

# Ler cada arquivo e empilhar tudo em um único data frame
base_unica <- arquivos |>
  lapply(readRDS) |>    # lê cada .rds e devolve lista de data frames
  bind_rows()           # empilha todos em uma única tabela

# Salário último ano
base_unica <- base_unica %>%
  mutate(salariominimo_proprioano = case_when(
    Ano == 2023 ~ 1320,
    Ano == 2017 ~ 937,
    Ano == 2016 ~ 880,
    Ano == 2018 ~ 954,
    Ano == 2019 ~ 998,
    Ano == 2020 ~ 1039,
    Ano == 2021 ~ 1100,
    Ano == 2022 ~ 1212,
    TRUE ~ NA_real_ # Define NA para anos não listados
  ))


base_unica$PB <- ifelse((base_unica$VD2006 == "14 a 19 anos" | base_unica$VD2006 == "20 a 24 anos") &
                     (as.numeric(base_unica$VD3004)== 3 | as.numeric(base_unica$VD3004)== 4) &
                     (
                       (base_unica$VD5008real_proprioano <= base_unica$salariominimo_proprioano/2 &
                          !is.na(base_unica$VD5008real_proprioano)|
                          base_unica$V5001A=="Sim"|
                          base_unica$V5002A=="Sim"|
                          base_unica$V5003A=="Sim")),
                   1,
                   0)

base_unica$nao_branco <- ifelse(base_unica$V2010 != "Branca" &
                                  base_unica$V2010 != "Ignorado",1,0)

# Criar a dummy com as condições especificadas
base_unica <- base_unica %>%
  mutate(trab = ifelse(
    VD4002 %in% c("Pessoas ocupadas", "Pessoas desocupadas"), 1,
    ifelse(is.na(VD4002), 0, NA_real_)
  ))

base_unica$prog_social <- ifelse(base_unica$V5001A == "Sim" | # BPC
                                 base_unica$V5002A == "Sim" | # BF
                                 base_unica$V5003A == "Sim", 1 ,0)  # Outros programas sociais

# Criar dummies diretamente da combinação de Ano e Trimestre
base_unica <- base_unica %>%
  dummy_cols(select_columns = c("Ano", "Trimestre", "GR", "UF"), 
             remove_selected_columns = FALSE,
             remove_first_dummy = FALSE) # Inclui todas as dummies sem excluir a primeira

base_unica$mulher <- ifelse(base_unica$V2007 == "Mulher",1,0)
base_unica$rural <- ifelse(base_unica$V1022 == "Rural",1,0)

# Substitua 'df' pelo nome do seu dataframe
base_unica <- base_unica %>%
  mutate(
    # Converter factor para texto
    VD3005 = as.character(VD3005),
    
    # Extrair números e tratar casos especiais
    VD3005_numerica = case_when(
      VD3005 == "Sem instrução e menos de 1 ano de estudo" ~ 0,
      is.na(VD3005) ~ NA_real_,
      TRUE ~ as.numeric(str_extract(VD3005, "\\d+"))
    )
  )


# Filtrar observações onde PB == 1
base_filtrada <- base_unica %>% filter(PB == 1 & VD3005_numerica < 12
                                         )

base_filtrada$anos_para_completar_em <- 12 - base_filtrada$VD3005_numerica

base_filtrada$valor_PdM_mensalidades <- 200

base_filtrada$valor_PdM_conclusao <- 83.33

base_filtrada$PdM_tot <- 283.33
  
modelo <- lm(VD3005_numerica ~ log(VD5008real_ef_proprioano + 1) + V2009 + mulher + nao_branco + rural + 
               `GR_Centro-Oeste` +
               GR_Nordeste + GR_Norte + GR_Sul + UF_Acre + UF_Amazonas + UF_Rondônia +
               UF_Roraima + UF_Pará + UF_Amapá + UF_Tocantins + UF_Maranhão + 
               UF_Piauí + UF_Ceará + `UF_Rio Grande do Norte` + UF_Paraíba + 
               UF_Pernambuco + UF_Alagoas + UF_Sergipe + UF_Bahia +
               `UF_Minas Gerais` + `UF_Espírito Santo` + `UF_Rio de Janeiro` + 
               UF_Paraná + `UF_Santa Catarina` + `UF_Rio Grande do Sul` + 
               `UF_Mato Grosso do Sul` + `UF_Mato Grosso` + UF_Goiás + 
               `UF_Distrito Federal` + max_educacao_pais +
               moradores_rendimento + trab + prog_social +
               Ano_2017 + Ano_2018 + Ano_2019 + Ano_2022 + Ano_2023 + 
                Trimestre_2 + Trimestre_3 + Trimestre_4, 
             data = base_filtrada)

# Criar um vetor de NAs com o mesmo tamanho do dataframe original
base_filtrada$anos_estud_predito <- NA

# Preencher apenas as linhas usadas pelo modelo com as previsões
linhas_do_modelo <- as.integer(rownames(modelo$model))  # Índices das linhas usadas
base_filtrada$anos_estud_predito[linhas_do_modelo] <- predict(modelo)

# Criar uma cópia do dataframe com VD5008real_proprioano ajustado
base_modificada <- base_filtrada %>%
  mutate(VD5008real_ef_proprioano = VD5008real_ef_proprioano + PdM_tot)

# Gerar as previsões com o valor ajustado
base_filtrada$VD3005_estimada_PdM <- predict(modelo, newdata = base_modificada)

base_filtrada$diff <- base_filtrada$VD3005_estimada_PdM - base_filtrada$anos_estud_predito

base_filtrada$diff_int <- as.integer(base_filtrada$VD3005_estimada_PdM) - as.integer(base_filtrada$anos_estud_predito)

base_filtrada$nova_serie <- base_filtrada$VD3005_numerica + base_filtrada$diff_int

base_filtrada$conclui <- ifelse(base_filtrada$VD3005_numerica != 12 &
                                base_filtrada$nova_serie == 12,1,0)