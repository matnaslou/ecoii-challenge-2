library(PNADcIBGE)
library(dplyr)
library(stringr)

################################ Abandonment Rate ################################
ano <- 2023
t <- 1

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

# Quarterly PNAD:
pnad_q1 <- get_pnadc(year=ano, quarter=t, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                           vars=c("UPA","V1008","V1014","V2003","Ano","Trimestre","UF","Capital",
                                                  "RM_RIDE","V2007","V2010","V2008","V20081","V20082","V3002",
                                                  "V3002A","V3003A","V3006","VD4020"
                                                  )
                     )

# Filtrar usando dplyr
pnad_q1 <- pnad_q1 %>% 
  filter(V20082 != 9999)

pnad_q2 <- get_pnadc(year=ano, quarter=t+1, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                                            vars=c("UPA","V1008","V1014","V2003","Ano","Trimestre","UF","Capital",
                                                   "RM_RIDE","V2007","V2010","V2008","V20081","V20082","V3002",
                                                   "V3002A","V3003A","V3006","VD4020")
                     )

# Filtrar usando dplyr
pnad_q2 <- pnad_q2 %>% 
  filter(V20082 != 9999)

pnad_q3 <- get_pnadc(year=ano, quarter=t+2, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                     vars=c("UPA","V1008","V1014","V2003","Ano","Trimestre","UF","Capital",
                            "RM_RIDE","V2007","V2010","V2008","V20081","V20082","V3002",
                            "V3002A","V3003A","V3006","VD4020")
)

# Filtrar usando dplyr
pnad_q3 <- pnad_q3 %>% 
  filter(V20082 != 9999)

pnad_q4 <- get_pnadc(year=ano, quarter=t+3, defyear=2023, labels=TRUE, deflator=TRUE, design=FALSE,
                     vars=c("UPA","V1008","V1014","V2003","Ano","Trimestre","UF","Capital",
                            "RM_RIDE","V2007","V2010","V2008","V20081","V20082","V3002",
                            "V3002A","V3003A","V3006","VD4020")
)

# Filtrar usando dplyr
pnad_q4 <- pnad_q4 %>% 
  filter(V20082 != 9999)


# Usar bind_rows para juntar as duas bases
pnad <- bind_rows(pnad_q1, pnad_q2,pnad_q3,pnad_q4)

# Criar a coluna 'id_pessoa' concatenando os valores das colunas com "_"
pnad <- pnad %>%
  mutate(id_pessoa = str_c(UPA, V1008, V1014, V2003, V2007, V2010, V2008, V20081, V20082, sep = "_"))

# Criar a coluna 'id_domicilio' concatenando os valores das colunas com "_"
pnad <- pnad %>%
  mutate(id_domicilio = str_c(UPA, V1008, V1014, sep = "_"))

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

# Criar a coluna 'rendimento_efetivo'
pnad <- pnad %>%
  mutate(rpc = VD4020_tot / n_moradores)

# Contar as ocorrências de cada id_pessoa
id_counts <- pnad %>%
  count(id_pessoa)

# Criar as dummies
pnad <- pnad %>%
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(
    gemeos = ifelse(n() > 4, 1, 0),  # Dummy para mais de 4 aparições
    uma = ifelse(n() == 1, 1, 0)   # Dummy para apenas 1 aparição
  ) %>%
  ungroup()  # Remover agrupamento

# Criar a nova coluna
pnad <- pnad %>%
  mutate(
    entrevista = case_when(
      Trimestre == 1 & V1016 == 1 ~ 1,  # Primeiro trimestre: apenas entrevista == 1
      Trimestre %in% 2:4 & (V1016 == 1 | V1016 == 2) ~ 2,  # Segundo ao quarto: entrevista == 1 ou 2
      TRUE ~ 0  # Caso contrário, atribui 0
    )
  )

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
pnad$empub <- ifelse(pnad$em == 1 & pnad$publica == 1)

# Criar a variável 'abandono'
pnad <- pnad %>%
  arrange(id_pessoa, Trimestre) %>%  # Ordenar por pessoa e trimestre
  group_by(id_pessoa) %>%  # Agrupar por id_pessoa
  mutate(
    abandono = ifelse(estuda == 0 & lag(estuda) == 1, 1, 0)  # Verifica se estuda passou de 1 para 0
  ) %>%
  ungroup()  # Remover agrupamento

