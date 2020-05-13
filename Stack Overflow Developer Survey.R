
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


#  ------------ Treinandos os modelos de classificação ------------ 
# Dois modelos serão usados: logistic regression e random forest, e pra ambos
# serão implementados bootstrap resampling e upsampling, pra lidar com o 
# desbalanceamento da classe de desenvolvedores remotos no dataset.
# Logistic regression model
stack_glm <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = stack_training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print do objeto do modelo
stack_glm

# Random forest model
stack_rf <- train(Remote ~ ., method = "rf", 
                  data = stack_training,
                  trControl = trainControl(method = "boot",
                                           sampling = "up",
                                           n_jobs))

# Print the model object
stack_rf


#  ------------ Avaliando os modelos ------------
# Here we can see the overall accuracy, as well as the positive and negative 
# predictive values, for the logistic regression model evaluated on the testing data. 
# Confusion matrix para logistic regression model
stack_testing %>%
  mutate(`Logistic regression` = predict(stack_glm, stack_testing)) %>%
  conf_mat(truth = Remote, estimate = "Logistic regression")

# Confusion matrix para random forest model
stack_testing %>%
  mutate(`Random forest` = predict(stack_rf, stack_testing)) %>%
  conf_mat(truth = Remote, estimate = "Random forest")

# A confusion matrix descreve quão bem um modelo de classificação performou. A confusion
# A confusion matrix tabula quantos exemplos em cada classe foram corretamente 
# classificados pelo modelo. Neste caso, ela mostrará quantos desenvolvedores remotos 
# foram classificados como remotos, e quantos não-remotos foram classificados como tal.
# Ela também mostrará quantos foram classificados nas categorias erradas.
# A confusion matrix é usada para avaliar a performance de modelos, logo, deve-se 
# usá-la no set de testes.


# ------------ Métricas de Modelos de Classificação ------------
# A função conf_mat() é útil, mas geralmente deseja-se armazenar estimativas específicas
# de performance, possivelmente em uma forma dataframe-friendly. O pacote yardstick foi
# construido pra lidar com essa necessidade. Para esse tipo de modelo de classificação
# deve-se olhar  para o positive or negative predictive value e overall accuracy.

# Predizendo os valores
testing_results <- stack_testing %>%
  mutate(`Logistic regression` = predict(stack_glm, stack_testing),
         `Random forest` = predict(stack_rf, stack_testing))

# Calculando precisão
accuracy(testing_results, truth = Remote, estimate = `Logistic regression`)
accuracy(testing_results, truth = Remote, estimate = `Random forest`)

# Calculando positive predictive value
ppv(testing_results, truth = Remote, estimate = `Logistic regression`)
ppv(testing_results, truth = Remote, estimate = `Random forest`)

# Em termos de overall accuracy e positive predictive value, o random forest model
# supera a performance do logistic regression model. Pode-se, então, predizer o 
# status de desenvolvedor remoto mais precisamente com um random forest model.