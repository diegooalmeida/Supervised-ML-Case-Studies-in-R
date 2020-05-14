
library(tidyverse)
library(rsample)
library(caret)
library(randomForest)
library(yardstick)

# ------------ Lendo dados ------------ 
getwd()
setwd("C:/Users/diego/Desktop/Curso SML/Capítulo 3/Dados")
voters <- read_csv("voters.csv") %>%
  mutate(turnout16_2016 = factor(turnout16_2016))


# ------------ Explorando os dados ------------ 
# Print voters
glimpse(voters)

# Quantas pessoas votaram?
voters %>%
  count(turnout16_2016)

# Como as respostas à pesquisa variam com o comportamento de voto (se votou ou não)?
voters %>%
  group_by(turnout16_2016) %>%
  summarise(`Elections don't matter` = mean(RIGGED_SYSTEM_1_2016 <= 2),
            `Economy is getting better` = mean(econtrend_2016 == 1),
            `Crime is very important` = mean(imiss_a_2016 == 2))


# ------------ Visualizando os dados ------------ 

# Visualizando a diferença por comportamento de voto
voters %>%
  ggplot(aes(econtrend_2016, ..density.., fill = turnout16_2016)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  labs(title = "Overall, is the economy getting better or worse?")


# ------------ Ajustando um modelo simples ------------ 
# Removendo a coluna case_identifier
voters_select <- voters %>%
  select(-case_identifier)

# Construindo um simples logistic regression model
simple_glm <- glm(turnout16_2016 ~ .,  family = "binomial", 
                  data = voters_select)

# Print do sumário               
summary(simple_glm)


# ------------ Splitting the data into training and testing sets ------------ 
set.seed(1234)
vote_split <- voters_select %>%
  initial_split(p = 0.8,
                strata = "turnout16_2016")
vote_training <- training(vote_split)
vote_testing <- testing(vote_split)


# ------------ Lidando com dados desbalanceados ------------ 
# Executando logistic regression com upsampling e sem resampling
vote_glm <- train(turnout16_2016 ~ ., 
                  method = "glm", family = "binomial",
                  data = vote_training,
                  trControl = trainControl(method = "none",
                                           sampling = "up"))

vote_glm

# Outra forma de balanceamento: Cross-validation
# Validação cruzada significa pegar seu conjunto de treinamento e dividi-lo 
# aleatoriamente e igualmente em subconjuntos, às vezes chamados de "dobras".
# Uma dobra aqui significa um grupo ou subconjunto ou partição.

# Usa-se uma das dobras para validação e as demais para treinamento, depois 
# repete-se essas etapas com todos os subconjuntos e combina os resultados, 
# geralmente fazendo a média. O motivo para isso é o mesmo motivo pelo qual 
# usaríamos a reamostragem de bootstrap; a validação cruzada permite reduzir
# o sobreajuste e obter uma estimativa mais precisa do desempenho do seu 
# modelo em novos dados.

# Logistic regression com upsampling e cross-validation
vote_glm <- train(turnout16_2016 ~ ., 
                  method = "glm", family = "binomial",
                  data = vote_training,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 2,
                                           sampling = "up"))

vote_glm

# Random forest com upsampling e cross-validation
vote_rf <- train(turnout16_2016 ~ ., method = "rf", 
                 data = vote_training,
                 trControl = trainControl(method = "repeatedcv", 
                                          repeats = 2,
                                          sampling = "up"))

vote_rf

# Modelos de aprendizado de máquina treinados 10-fold cross-validation 
# repetida 5 vezes, geralmente têm desempenho ideal.

# Descrição dos algorítmos: Treinamento com logistic regression models e random forest
# models com oversampling para solucionar o desbalanceamento da classe e 
# cross-validation pra obter uma estimativa mais precisa na performance do modelo.


# ------------ Confusion matrix nos dados de treino ------------ 

# logistic regression model 
vote_training %>%
  mutate(`Logistic regression` = predict(vote_glm, vote_training)) %>%
  conf_mat(truth = turnout16_2016, estimate = "Logistic regression")


# random forest model
vote_training %>%
  mutate(`Random forest` = predict(vote_rf, vote_training)) %>%
  conf_mat(truth = turnout16_2016, estimate = "Random forest")


# ------------ Confusion matrix nos dados de teste ------------ 

# logistic regression model
vote_testing %>%
  mutate(`Logistic regression` = predict(vote_glm, vote_testing)) %>%
  conf_mat(truth = turnout16_2016, estimate = "Logistic regression")


# random forest model
vote_testing %>%
  mutate(`Random forest` = predict(vote_rf, vote_testing)) %>%
  conf_mat(truth = turnout16_2016, estimate = "Random forest")

# Logistic regression é um modelo mais simples, mas, nesse caso, performou melhor
# nos dados de teste, e espera-se que faça um trabalho melhor predizendo em novos dados.