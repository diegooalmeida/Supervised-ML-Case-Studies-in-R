
library(tidyverse)
library(rsample)
library(caret)
library(randomForest)
library(yardstick)

# ------------ Lendo dados ------------ 
getwd()
setwd("C:/Users/diego/Desktop/Curso SML/Capítulo 2/Dados")
stackoverflow <- read_csv("stackoverflow.csv") %>%
  mutate(Remote = factor(Remote, levels = c("Remote", "Not remote")))


# ------------ Visualizando a distribuição  ------------ 
# Print stackoverflow
glimpse(stackoverflow)

# Contagem para Remote
stackoverflow %>% 
  count(Remote, sort = TRUE)

# Agrupamento dos Países
stackoverflow %>% 
  count(Country, sort = TRUE)

# Diagrama com a experiencia dos desenvolvedores, categorizada pelo local onde trabalham
ggplot(stackoverflow, aes(x = Remote, y = YearsCodedJob)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")


# ------------ Construíndo um modelo linear simples ------------ 
# Deselect as colunas irrelevantes pra o modelo (de identificação das linhas)
simple_glm <- stackoverflow %>%
  select(-Respondent) %>%
  # Ajustando o modelo linear a partir da informação sobre trabalho remoto
  # Usando regressao logística 
  glm(Remote ~ .,
      family = "binomial",
      data = .)

# A regressão logística é uma técnica estatística que tem como objetivo produzir, 
# a partir de um conjunto de observações, um modelo que permita a predição de 
# valores tomados por uma variável categórica, frequentemente binária, a partir
# de uma série de variáveis explicativas contínuas e/ou binárias.


# Print do sumário do modelo.
summary(simple_glm)


# ------------ Splitting the data into training and testing sets ------------ 
# Criam-se subconjuntos dos dados para reduzir o overfitting e obter uma estimativa
# mais precisa de como seu modelo será executado em novos dados.

# Criando o dataset a ser separado (Excluindo as informações irrelevantes)
stack_select <- stackoverflow %>%
  select(-Respondent)

# Separando os dados em training and testing sets
set.seed(1234)
stack_split <- stack_select %>%
  initial_split(p = 0.8,
                strata = "Remote")

stack_training <- training(stack_split)
stack_testing <- testing(stack_split)


# ------------ Lidando com dados desbalanceados ------------ 
# Criando o upsampled training set
# No x usa-se o select pra dizer quais serão os preditores(todos exceto o remote)
up_training <- upSample(x = select(stack_training, -Remote),
                     y = stack_training$Remote,
                     yname = "Remote") %>%
  as_tibble() # Uma tibble nada mais é do que um data.frame, 
              # mas com um método de impressão mais adequado.

# Contando o número de cada tipo de empregados remotos
up_training %>%
  count(Remote)

# Agora tem-se um conjunto de dados com classes balanceadas, 
# prontas para o aprendizado de máquina!




