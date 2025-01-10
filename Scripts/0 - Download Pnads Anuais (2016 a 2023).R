# Download & Saving PNAD data for efficiency
library(PNADcIBGE)
library(dplyr)

ano <- 2016

# Obtendo microdados anuais por visita da PNAD Contínua (PNADcIBGE >= 0.6.0)
if (ano == 2023) {
  pnadc_anual_visita <- PNADcIBGE::get_pnadc(year=ano, interview=1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                             vars=c("UPA","V1008","V1014","Ano","Trimestre","UF","Capital","RM_RIDE","V1022","V2005",
                                                    "V2007","V2009","V2010","V3002","V3002A","V3003A","V3010","V3013","VD2002","VD2006","VD2004","VD3004","VD4019",
                                                    "VD4048","VD3005","VD4001","VD4002","VD4003","VD4014","VD4009","VD4020","VD4047","V5001A","V5002A","V5003A"))
} else {                             
  pnadc_anual_visita <- PNADcIBGE::get_pnadc(year=ano, interview=1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                             vars=c("UPA","V1008","V1014","Ano","Trimestre","UF","Capital","RM_RIDE","V1022","V2005",
                                                    "V2007","V2009","V2010","V3002","V3002A","V3003A","V3010","V3013","VD2002","VD2006","VD2004","VD3004","VD4019",
                                                    "VD4048","VD3005","VD4001","VD4002","VD4003","VD4014","VD4009","VD4020","VD4047","V5001A","V5002A","V5003A",
                                                    "S01001","S01002","S01003","S01004","S01005","S01006","S01007A","S01010","S01011A","S01013",
                                                    "S010141","S01024","S01028","S010311","S010312"))
}
# Realizando coleta de lixo acumulada durante a obtenção dos microdados
gc(verbose=FALSE, reset=FALSE, full=TRUE)

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnadc_anual_visita <- pnadc_anual_visita %>%
  transform(
    ID_DOMICILIO = paste0(UPA, V1008, V1014),
    Pais = factor("Brasil", levels = "Brasil"),
    GR = factor(
      case_when(
        substr(UPA, 1, 1) == "1" ~ "Norte",
        substr(UPA, 1, 1) == "2" ~ "Nordeste",
        substr(UPA, 1, 1) == "3" ~ "Sudeste",
        substr(UPA, 1, 1) == "4" ~ "Sul",
        substr(UPA, 1, 1) == "5" ~ "Centro-Oeste",
        TRUE ~ NA_character_
      ),
      levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
    )
  )

# Realizando processo de obtenção da estimativa do rendimento domiciliar real
pnadc_anual_visita <- transform(pnadc_anual_visita, V2001_rendimento=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_proprioano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO1))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020real_proprioano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*CO1e))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_proprioano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO1e))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4019real_ultimoano=ifelse(is.na(VD4019) | is.na(V2001_rendimento),NA,VD4019*CO2))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4020real_ultimoano=ifelse(is.na(VD4020) | is.na(V2001_rendimento),NA,VD4020*CO2e))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD4048real_ultimoano=ifelse(is.na(VD4048) | is.na(V2001_rendimento),NA,VD4048*CO2e))
pnadc_anual_visita_rendimento <- pnadc_anual_visita %>% dplyr::group_by(ID_DOMICILIO) %>% dplyr::summarise(moradores_rendimento=sum(V2001_rendimento, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_proprioano=sum(VD4019real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_ef_proprioano=sum(VD4020real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_proprioano=sum(VD4048real_proprioano, na.rm=TRUE),
                                                                                                           rendimento_todos_trabalhos_ultimoano=sum(VD4019real_ultimoano, na.rm=TRUE),
                                                                                                           rendimento_outras_fontes_ultimoano=sum(VD4048real_ultimoano, na.rm=TRUE))
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, moradores_rendimento=moradores_rendimento)  
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_proprioano=rendimento_todos_trabalhos_proprioano+rendimento_outras_fontes_proprioano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_proprioano=VD5007real_proprioano/moradores_rendimento)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ef_proprioano=rendimento_todos_trabalhos_ef_proprioano+rendimento_outras_fontes_proprioano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ef_proprioano=VD5007real_ef_proprioano/moradores_rendimento)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5007real_ultimoano=rendimento_todos_trabalhos_ultimoano+rendimento_outras_fontes_ultimoano)
pnadc_anual_visita_rendimento <- transform(pnadc_anual_visita_rendimento, VD5008real_ultimoano=VD5007real_ultimoano/moradores_rendimento)
pnadc_anual_visita <- pnadc_anual_visita[,!(names(pnadc_anual_visita) %in% c("V2001_rendimento","VD4019real_proprioano","VD4020real_proprioano","VD4048real_proprioano","VD4019real_ultimoano","VD4020real_ultimoano","VD4048real_ultimoano"))]
pnadc_anual_visita_rendimento <- pnadc_anual_visita_rendimento[,!(names(pnadc_anual_visita_rendimento) %in% c("rendimento_todos_trabalhos_proprioano","rendimento_todos_trabalhos_ef_proprioano","rendimento_outras_fontes_proprioano","rendimento_todos_trabalhos_ultimoano","rendimento_outras_fontes_ultimoano"))]
pnadc_anual_visita <- merge(x=pnadc_anual_visita, y=pnadc_anual_visita_rendimento, by.x="ID_DOMICILIO", by.y="ID_DOMICILIO", all.x=TRUE, all.y=FALSE)
rm(pnadc_anual_visita_rendimento)
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ultimoano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_ultimoano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ultimoano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5007real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5007real_ef_proprioano))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5008real_proprioano=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",NA,VD5008real_ef_proprioano))

# Criando variável de faixa de rendimento domiciliar per capita em valores reais
salariominimo_ultimoano <- 1320

if (ano == 2023) {
  salariominimo_proprioano <- 1320
  
}

if (ano == 2017) {
  salariominimo_proprioano <- 937
}

if (ano == 2016) {
  salariominimo_proprioano <- 880
}

if (ano == 2018) {
  salariominimo_proprioano <- 954
}
if (ano == 2019) {
  salariominimo_proprioano <- 998
}
if (ano == 2020) {
  salariominimo_proprioano <- 1039
}
if (ano == 2021) {
  salariominimo_proprioano <- 1100
}
if (ano == 2022) {
  salariominimo_proprioano <- 1212
}
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5009real_proprioano=as.factor(x=ifelse(VD5008real_proprioano>=0 & VD5008real_proprioano<=salariominimo_proprioano/4,"Até ¼ salário mínimo",
                                                                                             ifelse(VD5008real_proprioano>salariominimo_proprioano/4 & VD5008real_proprioano<=salariominimo_proprioano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                    ifelse(VD5008real_proprioano>salariominimo_proprioano/2 & VD5008real_proprioano<=salariominimo_proprioano,"Mais de ½ até 1 salário mínimo",
                                                                                                           ifelse(VD5008real_proprioano>salariominimo_proprioano & VD5008real_proprioano<=salariominimo_proprioano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                  ifelse(VD5008real_proprioano>salariominimo_proprioano*2 & VD5008real_proprioano<=salariominimo_proprioano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                         ifelse(VD5008real_proprioano>salariominimo_proprioano*3 & VD5008real_proprioano<=salariominimo_proprioano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                                ifelse(VD5008real_proprioano>salariominimo_proprioano*5,"Mais de 5 salários mínimos",NA)))))))))
pnadc_anual_visita <- transform(pnadc_anual_visita, VD5009real_ultimoano=as.factor(x=ifelse(VD5008real_ultimoano>=0 & VD5008real_ultimoano<=salariominimo_ultimoano/4,"Até ¼ salário mínimo",
                                                                                            ifelse(VD5008real_ultimoano>salariominimo_ultimoano/4 & VD5008real_ultimoano<=salariominimo_ultimoano/2,"Mais de ¼ até ½ salário mínimo",
                                                                                                   ifelse(VD5008real_ultimoano>salariominimo_ultimoano/2 & VD5008real_ultimoano<=salariominimo_ultimoano,"Mais de ½ até 1 salário mínimo",
                                                                                                          ifelse(VD5008real_ultimoano>salariominimo_ultimoano & VD5008real_ultimoano<=salariominimo_ultimoano*2,"Mais de 1 até 2 salários mínimos",
                                                                                                                 ifelse(VD5008real_ultimoano>salariominimo_ultimoano*2 & VD5008real_ultimoano<=salariominimo_ultimoano*3,"Mais de 2 até 3 salários mínimos",
                                                                                                                        ifelse(VD5008real_ultimoano>salariominimo_ultimoano*3 & VD5008real_ultimoano<=salariominimo_ultimoano*5,"Mais de 3 até 5 salários mínimos",
                                                                                                                               ifelse(VD5008real_ultimoano>salariominimo_ultimoano*5,"Mais de 5 salários mínimos",NA)))))))))
pnadc_anual_visita$VD5009real_proprioano <- factor(x=pnadc_anual_visita$VD5009real_proprioano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))
pnadc_anual_visita$VD5009real_ultimoano <- factor(x=pnadc_anual_visita$VD5009real_ultimoano, levels=c("Até ¼ salário mínimo","Mais de ¼ até ½ salário mínimo","Mais de ½ até 1 salário mínimo","Mais de 1 até 2 salários mínimos","Mais de 2 até 3 salários mínimos","Mais de 3 até 5 salários mínimos","Mais de 5 salários mínimos"))

# Variável de Ensino Médio, interação entre essas duas, se tem Fund. Completo
pnadc_anual_visita <- pnadc_anual_visita %>%
  transform(
    # Ensino Médio
    em = factor(
      case_when(
        V3002 == "Sim" & V3003A == "Regular do ensino médio" ~ "Estuda EM", 
        V3002 == "Não" ~ "Não Estuda EM",
        TRUE ~ NA_character_
      ),
      levels = c("Estuda EM","Não Estuda EM")
    ),
    # Unipessoal
    unip = factor(
      case_when(
        VD2004 == "Unipessoal" ~ "Unipessoal",
        VD2004 == "Nuclear" | VD2004 == "Estendida" | VD2004 == "Composta" ~ "Não é Unipessoal",
        TRUE ~ NA_character_
      ),
      levels = c("Unipessoal", "Não é Unipessoal")
    ),
    # Tem EF Completo?
    ef_comp = case_when(
      VD3004 == "Fundamental completo ou equivalente" ~ 1,
      .default = 0
    )
  )

# Educação dos Pais (Máximo entre Eles, assim como no CADÚNICO)
# Mother's education
pnadc_anual_visita <- pnadc_anual_visita %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
  )) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_mae
  group_by(ID_DOMICILIO) %>%
  mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
  
  # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
  select(-is_mae) %>%
  ungroup()

# Father Educ and Maximimum between Mother and Father
pnadc_anual_visita <- pnadc_anual_visita %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
  )) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_pai
  group_by(ID_DOMICILIO) %>%
  mutate(educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)) %>%
  
  # Passo 3: Criar a coluna max_educacao_pais com o valor máximo entre educacao_mae e educacao_pai
  mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  
  # Passo 4: Remover a coluna auxiliar is_pai e desagrupar
  select(-is_pai) %>%
  ungroup()

saveRDS(pnadc_anual_visita, file = "dados/pnad2016.rds")