##########################################################################
rm(list=ls(all=TRUE))
library(xtable)
library(dplyr)

# Defina o caminho para armazenar
caminho <- "C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/"


# Definindo os anos para o loop
anos <- c(2016,2017,2018,2019,2022,2023)
ano <- 2016 # Selecione um único ano, se quiser rodar para teste


# Loop para cada ano
for (ano in anos) {  
  
  # Definindo limite de memória para compilação do programa
  aviso <- getOption("warn")
  options(warn=-1)
  memory.limit(size=50000)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo tempo de espera para obtenção de resposta do servidor
  aviso <- getOption("warn")
  options(warn=-1)
  options(timeout=600)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de codificação dos caracteres e linguagem
  aviso <- getOption("warn")
  options(warn=-1)
  options(encoding="latin1")
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de exibição de números sem representação em exponencial
  aviso <- getOption("warn")
  options(warn=-1)
  options(scipen=999)
  options(warn=aviso)
  rm(aviso)
  
  # Definindo opção de repositório para instalação dos pacotes necessários
  aviso <- getOption("warn")
  options(warn=-1)
  options(repos=structure(c(CRAN="https://cran.r-project.org/")))
  options(warn=aviso)
  rm(aviso)
  
  # Definindo diretório de trabalho
  caminho <- getwd()
  setwd(dir=caminho)
  
  # Carregando pacotes necessários para obtenção da estimativa desejada
  if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="PNADcIBGE", dependencies=TRUE)
  }
  library(package="PNADcIBGE", verbose=TRUE)
  if("dplyr" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="dplyr", dependencies=TRUE)
  }
  library(package="dplyr", verbose=TRUE)
  if("tibble" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="tibble", dependencies=TRUE)
  }
  library(package="tibble", verbose=TRUE)
  if("survey" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="survey", dependencies=TRUE)
  }
  library(package="survey", verbose=TRUE)
  if("convey" %in% rownames(installed.packages())==FALSE)
  {
    install.packages(pkgs="convey", dependencies=TRUE)
  }
  library(package="convey", verbose=TRUE)
  
  # Verificando manual de utilização da função para obtenção dos microdados
  help(topic="get_pnadc", package="PNADcIBGE")
  
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
  
  # Realizando processo de incorporação do desenho amostral nos microdados
  pnadc_anual_visita <- tibble::as_tibble(x=pnadc_anual_visita)
  pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)
  str(object=pnadc_anual_visita)
  
  
  ####################### Minha Parte #############################
  # Subset of Pé-De-Meia beneficiaries (14 to 24 years,Goes to School,Public School,High School,HIpc less than half minimum wage and not null OR Received some social cash transfer,Not an single-person household )
  pnad_PdM <- subset(pnadc_anual_visita, (VD2006 == "14 a 19 anos" | VD2006 == "20 a 24 anos")
                           & V3002 == "Sim"  
                           & V3002A == "Rede pública" 
                           & V3003A == "Regular do ensino médio"  
                           & ((VD5008real_proprioano <= salariominimo_proprioano/2 & !is.na(VD5008real_proprioano)|V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")) 
                           & VD2004 != "Unipessoal")
  
  # Potential Beneficiaries: Subset of people with 14 to 24 years that receives some cash transfer, and have Primary School Completed
  pnad_yk <- subset(pnadc_anual_visita, (VD2006 == "14 a 19 anos" | VD2006 == "20 a 24 anos") & (as.numeric(VD3004)== 3 | as.numeric(VD3004)== 4)
                           & ((VD5008real_proprioano <= salariominimo_proprioano/2 & !is.na(VD5008real_proprioano)|V5001A=="Sim"|V5002A=="Sim"|V5003A=="Sim")) 
                           )
  
  # Removendo base da pnad para limpar o espaço um pouco
  rm(pnadc_anual_visita)
    # Variable for Counting Observations
  pnad_PdM <- transform(pnad_PdM, contagem=1)
  pnad_yk <- transform(pnad_yk, contagem=1)

    # Criando o loop para cálculo das duas bases de dados
    bases <- list("pnad_PdM", "pnad_yk")
    
    for (base in bases) {
    # Nomes da variável que será criada para cada ano
    var_name1 <- paste0("contagem_", ano, "_", base)
    
    # Criando a variável `contagem_beneficiados` para cada ano usando `survey::svybys`
    assign(var_name1, survey::svybys(
      formula = ~contagem,
      by = ~Pais + GR + UF,
      design = get(base), # Obtenha o design do survey para cada ano
      FUN = svytotal,
      keep.var = FALSE,
      keep.names = FALSE,
      na.rm = TRUE
    ))
    
    var_name2 <- paste0("quantil_renda_dom_pc", ano, "_", base)
    
    assign(var_name2, svyquantile(x=~VD5008real_ef_proprioano, design=get(base), 
                                  quantiles=c(0.1,0.25,0.5,0.75,0.9), ci=TRUE, na.rm=TRUE))
    
    var_name3 <- paste0("estats_hab_", ano, "_", base)
    
    if (ano == 2019 | ano == 2022) {
      assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01007A+S01010+S01011A+S01013+S010141+S01024+S01028+S010311+S010312, 
                                   by = ~Pais + GR + UF, 
                                   design = get(base), 
                                   FUN = svymean, 
                                   keep.var = FALSE,
                                   keep.names = FALSE,
                                   na.rm = TRUE))
    }
    
    if (ano == 2017 | ano == 2018) {
      assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01010+S01011A+S01013+S010141+S01024+S01028+S010311+S010312, 
                                        by = ~Pais + GR + UF, 
                                        design = get(base), 
                                        FUN = svymean, 
                                        keep.var = FALSE,
                                        keep.names = FALSE,
                                        na.rm = TRUE))
    }
    
    if (ano == 2016) {
      assign(var_name3,svybys(formula=~S01001+S01002+S01003+S01004+S01005+S01006+S01010+S01013+S010141+S01024+S01028+S010311+S010312, 
                              by = ~Pais + GR + UF, 
                              design = get(base), 
                              FUN = svymean, 
                              keep.var = FALSE,
                              keep.names = FALSE,
                              na.rm = TRUE))
    }
    #variaveis <- c("V2007", "V2010", "VD4014","VD2006","VD5008real_ef_proprioano")
       
      var_name4a <- paste0("estats_a_", ano, "_", base)
    
      # Calcula a proporção para cada variável usando svyby
      assign(var_name4a, svybys(formula = ~V2007+V2009+V2010+VD4014+VD2006+VD5008real_ef_proprioano+VD4009, 
                                   by = ~Pais + GR + UF, 
                                   design = get(base), 
                                   FUN= svymean,
                                   keep.names = FALSE,
                                   keep.var = FALSE,
                                   na.rm = TRUE))
      
      var_name4b <- paste0("estats_b_", ano, "_", base)
      
      assign(var_name4b, svybys(formula = ~moradores_rendimento+max_educacao_pais+unip+V1022+em+interaction(em,V3002A), 
                               by = ~Pais + GR + UF, 
                               design = get(base), 
                               FUN= svymean,
                               keep.names = FALSE,
                               keep.var = FALSE,
                               na.rm = TRUE))
      
      
      # Loop para exportação
      numeros <- c(1,2,3)
      for (x in numeros) {
        var_name5 <- paste0("contagem_",ano, "_", base)
        var_name5A <- paste0(var_name5,"_",x)
        var_name6 <- paste0("estats_a_",ano, "_", base)
        var_name6A <- paste0(var_name6,"_",x)
        var_name7 <- paste0("estats_b_",ano, "_", base)
        var_name7A <- paste0(var_name7,"_",x)
        var_name8 <- paste0("estats_hab_",ano, "_", base)
        var_name8A <- paste0(var_name8,"_",x)
        
        assign(var_name5A, as.data.frame(get(var_name5)[[x]]))
        assign(var_name6A, as.data.frame(get(var_name6)[[x]]))
        assign(var_name7A, as.data.frame(get(var_name7)[[x]]))
        if (ano != 2023) {
          assign(var_name8A, as.data.frame(get(var_name8)[[x]]))
        }
      }
    }
    
    rm(list = paste0("contagem_", ano, "_pnad_PdM"))
    rm(list = paste0("contagem_", ano, "_pnad_yk"))
    rm(list = paste0("estats_a_", ano, "_pnad_PdM"))
    rm(list = paste0("estats_a_", ano, "_pnad_yk"))
    rm(list = paste0("estats_b_", ano, "_pnad_PdM"))
    rm(list = paste0("estats_b_", ano, "_pnad_yk"))
    rm(list = paste0("estats_hab_", ano, "_pnad_PdM"))
    rm(list = paste0("estats_hab_", ano, "_pnad_yk"))
    rm(list = "pnad_PdM")
    rm(list = "pnad_yk")
    
    }

for (base in bases) {
  
  contagem_1 <- bind_rows(get(paste0("contagem_2016_",base,"_1")),get(paste0("contagem_2017_",base,"_1")),
                          get(paste0("contagem_2018_",base,"_1")),get(paste0("contagem_2019_",base,"_1")),
                          get(paste0("contagem_2022_",base,"_1")),get(paste0("contagem_2023_",base,"_1")))
  
  contagem_2 <- bind_rows(get(paste0("contagem_2016_",base,"_2")),get(paste0("contagem_2017_",base,"_2")),
                          get(paste0("contagem_2018_",base,"_2")),get(paste0("contagem_2019_",base,"_2")),
                          get(paste0("contagem_2022_",base,"_2")),get(paste0("contagem_2023_",base,"_2")))
  
  contagem_3 <- bind_rows(get(paste0("contagem_2016_",base,"_3")),get(paste0("contagem_2017_",base,"_3")),
                          get(paste0("contagem_2018_",base,"_3")),get(paste0("contagem_2019_",base,"_3")),
                          get(paste0("contagem_2022_",base,"_3")),get(paste0("contagem_2023_",base,"_3")))
  
  contagem <- bind_rows(contagem_1,contagem_2,contagem_3)
  
  estats_1a <- bind_rows(get(paste0("estats_a_2016_",base,"_1")),get(paste0("estats_a_2017_",base,"_1")),
                        get(paste0("estats_a_2018_",base,"_1")),get(paste0("estats_a_2019_",base,"_1")),
                        get(paste0("estats_a_2022_",base,"_1")),get(paste0("estats_a_2023_",base,"_1")))
  
  estats_2a <- bind_rows(get(paste0("estats_a_2016_",base,"_2")),get(paste0("estats_a_2017_",base,"_2")),
                        get(paste0("estats_a_2018_",base,"_2")),get(paste0("estats_a_2019_",base,"_2")),
                        get(paste0("estats_a_2022_",base,"_2")),get(paste0("estats_a_2023_",base,"_2")))
  
  estats_3a <- bind_rows(get(paste0("estats_a_2016_",base,"_3")),get(paste0("estats_a_2017_",base,"_3")),
                         get(paste0("estats_a_2018_",base,"_3")),get(paste0("estats_a_2019_",base,"_3")),
                         get(paste0("estats_a_2022_",base,"_3")),get(paste0("estats_a_2023_",base,"_3")))
  
  estatsa <- bind_rows(estats_1a,estats_2a,estats_3a)
  
  estats_1b <- bind_rows(get(paste0("estats_b_2016_",base,"_1")),get(paste0("estats_b_2017_",base,"_1")),
                        get(paste0("estats_b_2018_",base,"_1")),get(paste0("estats_b_2019_",base,"_1")),
                        get(paste0("estats_b_2022_",base,"_1")),get(paste0("estats_b_2023_",base,"_1")))
  
  estats_2b <- bind_rows(get(paste0("estats_b_2016_",base,"_2")),get(paste0("estats_b_2017_",base,"_2")),
                        get(paste0("estats_b_2018_",base,"_2")),get(paste0("estats_b_2019_",base,"_2")),
                        get(paste0("estats_b_2022_",base,"_2")),get(paste0("estats_b_2023_",base,"_2")))
  
  estats_3b <- bind_rows(get(paste0("estats_b_2016_",base,"_3")),get(paste0("estats_b_2017_",base,"_3")),
                         get(paste0("estats_b_2018_",base,"_3")),get(paste0("estats_b_2019_",base,"_3")),
                         get(paste0("estats_b_2022_",base,"_3")),get(paste0("estats_b_2023_",base,"_3")))
  
  estatsb <- bind_rows(estats_1b,estats_2b,estats_3b)
  
  estats_hab_1 <- bind_rows(get(paste0("estats_hab_2016_",base,"_1")),get(paste0("estats_hab_2017_",base,"_1")),
                            get(paste0("estats_hab_2018_",base,"_1")),get(paste0("estats_hab_2019_",base,"_1")),
                            get(paste0("estats_hab_2022_",base,"_1")))
  
  estats_hab_2 <- bind_rows(get(paste0("estats_hab_2016_",base,"_2")),get(paste0("estats_hab_2017_",base,"_2")),
                            get(paste0("estats_hab_2018_",base,"_2")),get(paste0("estats_hab_2019_",base,"_2")),
                            get(paste0("estats_hab_2022_",base,"_2")))
  
  estats_hab_3 <- bind_rows(get(paste0("estats_hab_2016_",base,"_3")),get(paste0("estats_hab_2017_",base,"_3")),
                            get(paste0("estats_hab_2018_",base,"_3")),get(paste0("estats_hab_2019_",base,"_3")),
                            get(paste0("estats_hab_2022_",base,"_3")))
  
  estats_hab <- bind_rows(estats_hab_1,estats_hab_2,estats_hab_3)
  
  write.csv(contagem,paste0(caminho, "contagem_", base, ".csv"),row.names = FALSE)
  write.csv(estatsa,paste0(caminho, "estats_a_", base, ".csv"),row.names = FALSE)
  write.csv(estatsb,paste0(caminho, "estats_b_", base, ".csv"),row.names = FALSE)
  write.csv(estats_hab,paste0(caminho, "estats_hab_", base, ".csv"),row.names = FALSE)
  
}