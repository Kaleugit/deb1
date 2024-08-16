#limpar environment:
#rm(list = ls())

#Lista de pacotes
#install.packages("tidyverse")
#install.packages("ggplot2")
install.packages("ggrepel")

#install.packages("dplyr")  # Run this if you haven't installed the package yet
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)


#Ler o arquivo .csv uma vez que o diretório está setado
tabela <- read.csv("DEB1_encerrada_10_08_24.csv")


#Traz o resumo da tabela
summary(tabela)


#Transforma uma coluna em fator para poder analisar o dado descritivo, existem niveis/categorias/opções especificas
tabela$genero <- as.factor(tabela$genero)
tabela$tempo_esc <- as.factor(tabela$tempo_esc)
tabela$modalidade <- as.factor(tabela$modalidade)

#Idenfica os levls do fator
levels(tabela$genero)

#Trabalhar essa informação em grupos
mulher <- tabela %>%
  filter(genero == "Mulher")

summary(mulher)

homem <- tabela %>%
  filter(genero == "Homem")

summary(homem)

#Cria uma coluna dentro da tabela
tabela$idade_factor <- as.factor(tabela$idade)

#exportar csv
write.csv(tabela, "tabela.csv")


# Calculate counts and percentages criar tabela_with_counts
tabela_with_counts <- tabela %>%
  count(genero, modalidade) %>%
  group_by(genero) %>%
  mutate(Percentage = n / sum(n) * 100)



#modifica a tabela_with_counts *era dividido em 4 partes, homem/mulher/indoor/outdoor
#agora esta dividido entre tempo_esc, genero, numero de pessoas
#e a porcentagem que representa
tabela_with_counts <- tabela %>%
  count(genero, tempo_esc) %>%
  group_by(genero) %>%
  mutate(Percentage = n / sum(n) * 100)

#deixa só 2 digitos apos a virgula na coluna de porcentagem
tabela_with_counts$Percentage2 <- round(tabela_with_counts$Percentage, 2)

#apaga coluna da tabela
tabela_with_counts$Percentage <- NULL

# Ensure tempo_esc in tabela_with_counts is a factor with the desired levels in ascending order
tabela_with_counts$tempo_esc <- factor(tabela_with_counts$tempo_esc, 
                                       levels = c("Menos de um ano", 
                                                  "1 - 2 anos", 
                                                  "3 - 5 anos", 
                                                  "6 - 10 anos", 
                                                  "11 - 20 anos", 
                                                  "Mais de 20 anos", 
                                                  "Mais de 30 anos"))

# Ensure tempo_esc in tabela is a factor with the desired levels in ascending order
tabela$tempo_esc <- factor(tabela$tempo_esc, 
                           levels = c("Menos de um ano", 
                                      "1 - 2 anos", 
                                      "3 - 5 anos", 
                                      "6 - 10 anos", 
                                      "11 - 20 anos", 
                                      "Mais de 20 anos", 
                                      "Mais de 30 anos"))



#excluir a linha participante outlier duplicado
tabela <- tabela[-75, ]



# Calcular a proporção de escaladores por modalidade
tabela_summary <- tabela %>%
  group_by(modalidade) %>%
  summarise(count = n()) %>%
  mutate(Percentage = count / sum(count))




###PLOTS:




# Gráfico de barras verticais Número de Escaladores por Modalidade e Gênero
ggplot(tabela, aes(x = modalidade, fill = genero)) +
  geom_bar(position = "dodge") +  # Barras lado a lado para cada gênero
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = 1.5, size = 5, color = "black") +  # Ajusta o vjust para colocar o texto dentro da barra
  labs(title = "Número de Escaladores por Modalidade e Gênero",
       x = "",
       y = "",
       fill = "Gênero") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove as linhas de grade
        axis.text = element_blank(),    # Remove os textos dos eixos
        axis.ticks = element_blank())   # Remove as marcações dos eixos


# Gráfico de barras com a Proporção de Escaladores por Tempo de Escalada
ggplot(tabela, aes(x = tempo_esc, fill = tempo_esc)) +
  geom_bar(aes(y = ..count../sum(..count..)), stat = "count") +
  geom_text(aes(y = ..count../sum(..count..), 
                label = scales::percent(..count../sum(..count..), accuracy = 1)),
            stat = "count", 
            position = position_stack(vjust = 0.5),  # Centraliza o texto verticalmente dentro da barra
            color = "black", size = 4) +  # Ajusta a cor e o tamanho do texto
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporção de Escaladores por Tempo de Escalada", 
       x = "", 
       y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove as linhas de grade
        axis.text = element_blank(),    # Remove os textos dos eixos
        axis.ticks = element_blank())   # Remove as marcações dos eixos


# Gráfico de barras verticais com a Proporção de Escaladores por Faixa Etária
ggplot(tabela, aes(x = as.factor(idade), fill = as.factor(idade))) +
  geom_bar(aes(y = ..count../sum(..count..)), stat = "count") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporção de Escaladores por Faixa Etária", 
       x = "", 
       y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),   # Remove as linhas de grade
        axis.ticks = element_blank(),   # Remove as marcações dos eixos
        legend.position = "none")       # Remove a legenda



# Gráfico de barras com a Proporção de Escaladores por Modalidade
ggplot(tabela, aes(x = modalidade, fill = modalidade)) +
  geom_bar(aes(y = ..count../sum(..count..)), stat = "count") +
  geom_text(aes(y = ..count../sum(..count..), 
                label = scales::percent(..count../sum(..count..), accuracy = 1)),
            stat = "count", 
            position = position_stack(vjust = 0.5),  # Centraliza o texto verticalmente dentro da barra
            color = "black", size = 4) +  # Ajusta a cor e o tamanho do texto
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proporção de Escaladores por Modalidade", 
       x = "", 
       y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank(),   # Remove as linhas de grade
        axis.ticks = element_blank(),   # Remove as marcações dos eixos
        axis.text.y = element_text(size = 10),  # Ajusta o tamanho do texto do eixo y
        legend.position = "none")       # Remove a legenda



# Gráfico de barras verticais Número de Escaladores por Modalidade e Gênero
ggplot(tabela, aes(x = modalidade, fill = genero)) +
  geom_bar(position = "dodge") +  # Barras lado a lado para cada gênero
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = 10, size = 5 ) +  # Adiciona os rótulos com a contagem
  labs(title = "Número de Escaladores por Modalidade e Gênero",
       x = "",
       y = "",
       fill = "Gênero") +
  theme_minimal()  


# Gráfico de barras Número de Escaladores por Gênero e Tempo de Escalada
ggplot(tabela, aes(x = genero, fill = tempo_esc)) +
  geom_bar(position = "dodge") +  # Barras lado a lado para cada categoria de 'tempo_esc'
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4, color = "black") +  # Ajusta o vjust para colocar o texto dentro da barra
  labs(title = "Número de Escaladores por Gênero e Tempo de Escalada",
       x = "Gênero",
       y = "",
       fill = "Tempo de Escalada") +
  theme_minimal()  # Usando o tema minimalista para clareza



# Gráfico de barras com porcentagem dentro das barras
ggplot(tabela_summary, aes(x = modalidade, y = Percentage, fill = modalidade)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = scales::percent(Percentage, accuracy = 1)),
            vjust = 1.5, color = "black", size = 6) +  # Adiciona rótulos de porcentagem dentro das barras
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Proporção de Escaladores por Modalidade", 
       x = "Modalidade", 
       y = "Proporção") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove a legenda já que as barras estão coloridas pelo mesmo fator




# Supondo que 'tabela' tenha as colunas 'modalidade' e 'tempo_esc'
# Contar o número de ocorrências para cada combinação de modalidade e tempo_esc
tabela_summary <- tabela %>%
  group_by(modalidade, tempo_esc) %>%
  summarise(count = n(), .groups = 'drop')

# Gráfico de barras horizontais Distribuição de Modalidade vs Tempo de Escalada
ggplot(tabela_summary, aes(x = reorder(modalidade, count), y = count, fill = tempo_esc)) +
  geom_bar(stat = "identity", width = 0.7) +  # Ajusta a largura das barras
  coord_flip() +  # Inverte os eixos para criar barras horizontais
  labs(title = "Distribuição de Modalidade vs Tempo de Escalada",
       x = "",
       y = "Número de Escaladores",
       fill = "Tempo de Escalada") +
  scale_fill_brewer(palette = "PuOr") +  # Paleta de cores pastel
  theme(axis.text.x = element_text(angle = 0, hjust = 4))  # Ajusta a orientação do texto, se necessário







