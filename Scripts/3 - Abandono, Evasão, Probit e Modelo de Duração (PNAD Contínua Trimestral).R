library(PNADcIBGE)
library(dplyr)
library(stringr)
library(lmtest)
library(margins)
################################ Abandonment Rate ################################
ano <- 2023
t <- 1
var <- c("UPA","V1008","V1014","V1016","V1022","V2003","V2009","Ano","Trimestre","UF","Capital",
         "RM_RIDE","V2005","V2007","V2010","V2008","V20081","V20082","V3002",
         "V3002A","V3003A","V3004","V3005A","V3006","V3009A","V3013","V3014","VD2002","VD2004","VD3005","VD4002","VD4020"
         )

# Salário Mínimo
if (ano == 2023) {
  sm <- 1320
}
if (ano == 2017) {
  sm <- 937
}

if (ano == 2016) {
  sm <- 880
}

if (ano == 2018) {
  sm <- 954
}
if (ano == 2019) {
  sm <- 998
}
if (ano == 2020) {
  sm <- 1039
}
if (ano == 2021) {
  sm <- 1100
}
if (ano == 2022) {
  sm <- 1212
}
# Quarterly PNAD:
pnad_q1 <- get_pnadc(year=ano, quarter=t, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                           vars=var
                     )

# Filtrar usando dplyr
#pnad_q1 <- pnad_q1 %>% 
#  filter(V20082 != 9999)

pnad_q2 <- get_pnadc(year=ano, quarter=t+1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                            vars=var
                     )

# Filtrar usando dplyr
#pnad_q2 <- pnad_q2 %>% 
#  filter(V20082 != 9999)

pnad_q3 <- get_pnadc(year=ano, quarter=t+2, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                     vars=var
)

# Filtrar usando dplyr
#pnad_q3 <- pnad_q3 %>% 
#  filter(V20082 != 9999)

pnad_q4 <- get_pnadc(year=ano, quarter=t+3, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                     vars=var
)

# Filtrar usando dplyr
#pnad_q4 <- pnad_q4 %>% 
#  filter(V20082 != 9999)


# Usar bind_rows para juntar as duas bases
pnad <- bind_rows(pnad_q1, pnad_q2,pnad_q3,pnad_q4)

# Criar a coluna 'id_pessoa' concatenando os valores das colunas com "_"
pnad <- pnad %>%
  mutate(id_pessoa = str_c(UPA, V1008, V1014, V2003, V2007, V2010, V2008, V20081, V20082, sep = "_"))

# Criar a coluna 'id_domicilio' concatenando os valores das colunas com "_"
pnad <- pnad %>%
  mutate(id_domicilio = str_c(UPA, V1008, V1014, sep = "_"))

# Criando variáveis auxiliares para obtenção da estimativa desejada
pnad <- pnad %>%
  transform(
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

# Moradores do Domicílio e Rendas para considerar no domicílio
pnad <- transform(pnad, morador=ifelse(V2005=="Pensionista" | V2005=="Empregado(a) doméstico(a)" | V2005=="Parente do(a) empregado(a) doméstico(a)",0,1))

# Criar a coluna 'numero_moradores' com a soma da coluna 'morador' para cada 'id_domicilio'
pnad <- pnad %>%
  group_by(Trimestre,id_domicilio) %>%  # Agrupar por id_domicilio
  mutate(n_moradores = sum(morador, na.rm = TRUE)) %>%  # Somar a coluna 'morador'
  ungroup()  # Remover o agrupamento

# Criar a coluna 'rendimento_efetivo'
pnad <- pnad %>%
  mutate(VD4020_ef = VD4020 * Efetivo)

# Criar a coluna 'renda_total_domicilio'
pnad <- pnad %>%
  group_by(Trimestre,id_domicilio) %>%
  mutate(
    VD4020_tot = sum(VD4020_ef[morador == 1], na.rm = TRUE)
  ) %>%
  ungroup()

# Criar a coluna 'renda per capita'
pnad <- pnad %>%
  mutate(rpc = VD4020_tot / n_moradores)

# Educação dos Pais (Máximo entre Eles, assim como no CADÚNICO)
# Mother's education
pnad <- pnad %>%
  # Passo 1: Identificar a mãe em cada família
  mutate(is_mae = (as.numeric(V2007) == 2 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
  )) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_mae
  group_by(Trimestre,id_domicilio) %>%
  mutate(educacao_mae = ifelse(any(is_mae), VD3005[is_mae][1], NA)) %>%
  
  # Passo 3: Remover a coluna auxiliar is_mae e desagrupar
  select(-is_mae) %>%
  ungroup()

# Father Educ and Maximimum between Mother and Father
pnad <- pnad %>%
  # Passo 1: Identificar o pai em cada família
  mutate(is_pai = (as.numeric(V2007) == 1 & (as.numeric(VD2002) == 1 | as.numeric(VD2002) == 2 | as.numeric(VD2002) == 6)
  )) %>%
  
  # Passo 2: Agrupar pela família e criar a coluna educacao_pai
  group_by(Trimestre,id_domicilio) %>%
  mutate(educacao_pai = ifelse(any(is_pai), VD3005[is_pai][1], NA)) %>%
  
  # Passo 3: Criar a coluna max_educacao_pais com o valor máximo entre educacao_mae e educacao_pai
  mutate(max_educacao_pais = pmax(educacao_mae, educacao_pai, na.rm = TRUE)) %>%
  
  # Passo 4: Remover a coluna auxiliar is_pai e desagrupar
  select(-is_pai) %>%
  ungroup()


# Contar as ocorrências de cada id_pessoa
id_counts <- pnad %>%
  count(id_pessoa)

# Criar as dummies
pnad <- pnad %>%
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(
    uma = ifelse(n() == 1, 1, 0)   # Dummy para apenas 1 aparição
  ) %>%
  ungroup()  # Remover agrupamento

# Identificando Gêmeos (pessoas que não consigo identificar ao longo do tempo)

# Passo 1: Contar id_pessoa em cada id_domicilio e Trimestre
counts <- pnad %>%
  group_by(Trimestre, id_domicilio, id_pessoa) %>%
  summarise(count_id_pessoa = n(), .groups = "drop") %>%  # Conta as aparições por id_pessoa
  group_by(Trimestre, id_domicilio, id_pessoa) %>%
  summarise(gemeos = as.integer(any(count_id_pessoa >= 2)), .groups = "drop")  # Verifica se há gemeos

# Passo 2: Adicionar a variável gemeos de volta à base original
pnad <- pnad %>%
  left_join(counts, by = c("Trimestre", "id_domicilio","id_pessoa"))

# Contar quantos aparecem exatamente uma vez e quantos aparecem duas vezes
contagem <- id_counts %>%
  filter(n <= 4) %>%
  count(n)

# Armazenar os resultados
uma_vez <- contagem %>% filter(n == 1) %>% pull(nn)  # Quantos aparecem 1 vez
duas_vezes <- contagem %>% filter(n == 2) %>% pull(nn)  # Quantos aparecem 2 vezes
tres_vezes <- contagem %>% filter(n == 3) %>% pull(nn)  # Quantos aparecem 3 vezes
quatro_vezes <- contagem %>% filter(n == 4) %>% pull(nn)  # Quantos aparecem 4 vezes

# Dummys
pnad$estuda <- ifelse(pnad$V3002 == "Sim", 1, 0)
pnad$publica <- ifelse(pnad$estuda == 1 & 
                       as.numeric(pnad$V3002A) == 2, 1, 0
                       )
pnad$privada <- ifelse(pnad$estuda == 1 & 
                          as.numeric(pnad$V3002A) == 1, 1, 0
                       )
pnad$em <- ifelse(pnad$estuda == 1 &
                  (pnad$publica == 1 | pnad$privada == 1) &
                   as.numeric(pnad$V3003A) == 6, 1, 0 
                  )
pnad$empub <- ifelse(pnad$em == 1 & pnad$publica == 1, 1, 0)

# Criar a variável 'conclusão'
pnad <- pnad %>%
  arrange(id_pessoa, Trimestre) %>%  # Ordenar por pessoa e trimestre
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(
    conclusao = ifelse(as.numeric(V3009A) == 10 & lag(as.numeric(V3003A)) == 6 
                       & as.numeric(V3013) == lag(as.numeric(V3006)) & V3014 == "Sim",
                       1, 0)  # Verifica se estuda passou de 1 para 0
  ) %>%
  ungroup()  # Remover agrupamento

# Criar a variável 'abandono'
pnad <- pnad %>%
  arrange(id_pessoa, Trimestre) %>%  # Ordenar por pessoa e trimestre
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(
    abandono = ifelse(estuda == 0 & lag(estuda) == 1 & conclusao != 1, 1, 0)  # Verifica se estuda passou de 1 para 0
  ) %>%
  ungroup()  # Remover agrupamento

# Filtrar pessoas entrevistadas pela primeira vez no primeiro Trimestre (Apenas as identificáveis (gemeos==0))
primeira_entrevista <- pnad %>%
  filter(Trimestre == 1 & V1016 == 1 & gemeos == 0) %>%
  select(id_pessoa)

# Total de pessoas entrevistadas pela primeira vez no primeiro Trimestre
total_primeira <- nrow(primeira_entrevista)

# Verificar quantas dessas pessoas aparecem na segunda entrevista no segundo Trimestre
segunda_entrevista <- pnad %>%
  filter(Trimestre == 2 & V1016 == 2 & gemeos == 0 & id_pessoa %in% primeira_entrevista$id_pessoa) %>%
  nrow()

# Verificar quantas aparecem na terceira entrevista no terceiro Trimestre
terceira_entrevista <- pnad %>%
  filter(Trimestre == 3 & V1016 == 3 & gemeos == 0 & id_pessoa %in% primeira_entrevista$id_pessoa) %>%
  nrow()

# Verificar quantas aparecem na quarta entrevista no quarto Trimestre
quarta_entrevista <- pnad %>%
  filter(Trimestre == 4 & V1016 == 4 & gemeos == 0 & id_pessoa %in% primeira_entrevista$id_pessoa) %>%
  nrow()

# Calcular os percentuais
percentuais <- c(
  "2ª Entrevista" = (segunda_entrevista / total_primeira) * 100,
  "3ª Entrevista" = (terceira_entrevista / total_primeira) * 100,
  "4ª Entrevista" = (quarta_entrevista / total_primeira) * 100
)

# Base dos que abandonaram (Observáveis Pré abandono para probit)
# Filtrar os dados do trimestre anterior ao abandono
pnad_com_abandono <- pnad %>%
  arrange(id_pessoa, Trimestre) %>%  # Ordenar por id_pessoa e Trimestre
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(abandono_seguinte = lead(abandono)) %>%  # Variável que indica abandono no trimestre seguinte
  filter(abandono_seguinte == 1) %>%  # Filtrar linhas cujo próximo trimestre é abandono
  ungroup()  # Remover o agrupamento

pnad_q12 <- pnad %>%
  filter(Trimestre == 1 | Trimestre == 2)

# Filtrar id_individuo que aparecem nos dois Trimestres
pnad_q12a <- pnad_q12 %>%
  group_by(id_pessoa) %>%
  filter(all(c(1, 2) %in% Trimestre)) %>%
  ungroup()

pnad_q12_14a24 <- pnad_q12a %>%
  # Filtrar pessoas de 14 a 24 anos
  filter(V2009 >= 14 & V2009 <= 24)

pnad_q12_em <- pnad_q12_14a24 %>%
  group_by(id_pessoa) %>% # Substitua pelo identificador único do indivíduo na base
  filter(any(em == 1 & (Trimestre == 1 | Trimestre == 2))) %>%
  ungroup()

pnad_q12_empub <- pnad_q12_14a24 %>%
  group_by(id_pessoa) %>% # Substitua pelo identificador único do indivíduo na base
  filter(any(empub == 1 & (Trimestre == 1 | Trimestre == 2))) %>%
  ungroup()



# Probit
probit12 <- glm(abandono ~ V2007+V2010+V2009+V1022+max_educacao_pais+rpc+n_moradores, 
                  family = binomial(link = "probit"), 
                  data = pnad_q12a)

coeftest(probit12, type = "HC1")

#margins_probit <- margins(probit12)
#summary(margins_probit)

#pnadc_anual_visita <- PNADcIBGE::pnadc_design(data_pnadc=pnadc_anual_visita)