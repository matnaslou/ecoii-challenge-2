library(xtable)

# Juntando os dataframes em um único dataframe
df <- bind_rows(resultados)
df2 <- bind_rows(resultados_cadu)

df <- df %>%
  rename(se_total_br = se_tot_br)

df2 <- df2 %>%
  rename(se_total_br = se_tot_br)

# Função para calcular teste t com estatísticas agregadas
calcular_t_test <- function(media1, se1, n1, media2, se2, n2) {
  # Variâncias estimadas
  var1 <- se1^2 * n1
  var2 <- se2^2 * n2
  
  # Estatística t
  t_stat <- (media1 - media2) / sqrt(var1 / n1 + var2 / n2)
  
  # Graus de liberdade de Welch
  df <- (var1 / n1 + var2 / n2)^2 / 
    ((var1 / n1)^2 / (n1 - 1) + (var2 / n2)^2 / (n2 - 1))
  
  # p-valor bilateral
  p_value <- 2 * pt(-abs(t_stat), df)
  
  return(list(t_stat = t_stat, p_value = p_value))
}

# Variáveis de interesse
variaveis <- c("total_br","perc_mulheres", "perc_raca","rpc_br") # Substitua pelas variáveis desejadas

# Calcular testes para todos os anos e variáveis
resultados <- lapply(unique(df$ano), function(ano) {
  # Filtrar dados do ano
  data1 <- df[df$ano == ano, ]
  data2 <- df2[df2$ano == ano, ]
  
  # Para cada variável
  lapply(variaveis, function(var) {
    media1 <- data1[[var]]
    se1 <- data1[[paste0("se_", var)]]
    n1 <- data1$total_PdM
    
    media2 <- data2[[var]]
    se2 <- data2[[paste0("se_", var)]]
    n2 <- data2$total_PdM
    
    # Calcular teste t
    teste <- calcular_t_test(media1, se1, n1, media2, se2, n2)
    
    # Adicionar significância
    sig <- ifelse(teste$p_value < 0.01, "***", 
                  ifelse(teste$p_value < 0.05, "**", 
                         ifelse(teste$p_value < 0.1, "*", "")))
    
    # Retornar resultados
    data.frame(
      Ano = ano,
      Variável = var,
      Média_DF1 = media1,
      Média_DF2 = media2,
      Diferença = media1 - media2,
      t_stat = teste$t_stat,
      p_valor = teste$p_value,
      Significância = sig
    )
  }) %>% bind_rows()
}) %>% bind_rows()

# Organizar a tabela para LaTeX
tabela <- resultados %>%
  select(Variável, Ano, Média_DF1, Média_DF2, Diferença, t_stat, p_valor, Significância) %>%
  mutate(
    Média_DF1 = sprintf("%.2f", Média_DF1),
    Média_DF2 = sprintf("%.2f", Média_DF2),
    Diferença = sprintf("%.2f", Diferença),
    t_stat = sprintf("%.2f%s", t_stat, Significância)
  )

tabela = subset(tabela, select = -c(p_valor,Significância))

tabela <- tabela[order(tabela$Variável),]

# Criar tabela LaTeX com variável centralizada
tabela_latex <- tabela %>%
  group_by(Variável) %>%
  mutate(
    Variável_Latex = ifelse(row_number() == 1, 
                            paste0("\\multirow{", n(), "}{*}{", Variável, "}"), 
                            "")
  ) %>%
  ungroup() %>%
  select(Variável_Latex, Ano, Média_DF1, Média_DF2, Diferença, t_stat)

# Converter para LaTeX
xtable_latex <- xtable(
  tabela_latex, 
  caption = "Comparação de Médias por Ano e Variável, PNAD Contínua e CADÚNICO",
  label = "tab:comparacao_medias"
)

# Imprimir a tabela em LaTeX com ajustes
print(xtable_latex, 
      include.rownames = FALSE, 
      sanitize.text.function = identity, 
)