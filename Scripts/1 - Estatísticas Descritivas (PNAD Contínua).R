##########################################################################
rm(list=ls(all=TRUE))
library(xtable)
library(dplyr)

# Defina o caminho para armazenar
caminho <- "C:/Users/rosan/OneDrive/Área de Trabalho/Eco II/Desafio/"


# Definindo os anos para o loop
anos <- c(2016,2017,2018,2019,2022,2023)
#ano <- 2016 # Selecione um único ano, se quiser rodar para teste


# Loop para cada ano
for (ano in anos) {  
  
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
  
  # School Students
  pnad_stud <- subset(pnadc_anual_visita, (VD2006 == "14 a 19 anos" | VD2006 == "20 a 24 anos")
                      & V3002 == "Sim" & V3002A == "Rede pública"
                        )
  
  
  # General Population
  pnad <- pnadc_anual_visita
  
  # Removendo base da pnad para limpar o espaço um pouco
  rm(pnadc_anual_visita)
    # Variable for Counting Observations
  pnad_PdM <- transform(pnad_PdM, contagem=1)
  pnad_yk <- transform(pnad_yk, contagem=1)
  pnad_stud <- transform(pnad_stud, contagem=1)
  pnad <- transform(pnad, contagem=1)
  
    # Criando o loop para cálculo das duas bases de dados
    bases <- list("pnad_PdM", "pnad_yk", "pnad_stud", "pnad")
    
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
      assign(var_name4a, svybys(formula = ~V2007+V2009+V2010+VD5008real_ef_proprioano, 
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