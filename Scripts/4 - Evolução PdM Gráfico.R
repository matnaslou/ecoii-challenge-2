# Carregar o pacote ggplot2
library(ggplot2)
library(dplyr)

# Adicionar uma coluna de identificação para cada dataframe
df4 <- df4 %>%
  mutate(Base = "PNAD Contínua")

df2 <- df2 %>%
  mutate(Base = "CADUNICO")

# Combinar os dois dataframes
df_combinado <- bind_rows(df4, df2)

# Criar o gráfico
ggplot(df_combinado, aes(x = ano, y = total_br, color = Base, group = Base)) +
  geom_line(size = 1.2) +             # Linhas conectando os pontos
  geom_point(size = 3) +             # Adicionar os pontos no gráfico
  labs(
    #title = "Evolução de Possíveis Beneficiados pelo Pé de Meia",
    x = "Ano",
    y = "Beneficiados Pé de Meia",
    color = "Base de Dados"
  ) +
  scale_x_continuous(breaks = unique(df_combinado$ano)) + # Ajustar os anos no eixo x
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) + # Escala em milhões
  theme_minimal(base_size = 14) +    # Tema minimalista com fonte maior
  theme(
    legend.position = "bottom",         # Colocar legenda abaixo
    panel.grid.major.x = element_blank(), # Remove linhas verticais principais
    panel.grid.minor.x = element_blank()  # Remove linhas verticais menores
  )