
# ------------ instalando/habilitando pacotes ------------ 
install.packages("tidyverse")
install.packages("rsample")
install.packages("caret")
install.packages("randomForest")
install.packages("yardstick")

library(tidyverse)
library(rsample)
library(caret)
library(randomForest)
library(yardstick)

# ------------ Lendo dados ------------ 
getwd()
setwd("C:/Users/diego/Desktop/Curso SML/Capítulo 1/Dados")
cars2018 <- read_csv("cars2018.csv")

# ------------ Visualizando a distribuição de Eficiencia de Combustível ------------ 
# Print do objeto
glimpse(cars2018)

# Plot do histograma 
ggplot(cars2018, aes(x = MPG)) +
  geom_histogram(bins = 25) +
  labs(x = "Fuel efficiency (mpg)",
       y = "Number of cars")


# ------------ Construíndo um modelo linear simples ------------ 
# Deselect as duas colunas irrelevantes pra o modelo (de identificação das linhas)
car_vars <- cars2018 %>%
  select(-Model, -`Model Index`)

# Ajustando o modelo linear, a partir da eficiencia de combustível
fit_all <- lm(MPG ~ ., data = car_vars)

# Print do sumário do modelo.
summary(fit_all)


# ------------ Testando um split nos dados (em testing e training) ------------ 
car_split <- car_vars %>%
  # a função initial_split separa, binariamente, o dataset, balanceadamente com relacao ao parametro
  initial_split(prop = 0.8,  # define a porcentagem em 80%
                strata = "Aspiration")  # define a coluna parametro 

# aqui é feita a distinção entre os conjuntos
car_training <- training(car_split) 
car_testing <- testing(car_split)


# ------------ Treinando um modelo ------------ 
# Com Linear Regression Model
fit_lm <- train(log(MPG) ~ ., # em caret especifica-se modelos usando train()
                method = "lm", 
                data = car_training,
                trControl = trainControl(method = "none")) # Isso significa: Apenas treine o modelo uma vez, no conjunto de treino inteiro.

# Print do objeto do modelo.
fit_lm

# Com Random Forest Model
fit_rf <- train(log(MPG) ~ ., # em caret especifica-se modelos usando train()
                method = "rf", 
                data = car_training,
                trControl = trainControl(method = "none")) # Isso significa: Apenas treine o modelo uma vez, no conjunto de treino inteiro.

# Print do objeto do modelo.
fit_rf


# ------------ Avaliando os Modelos ------------ 
# Criam-se as novas colunas, com os valores estimados nos modelos
results <- car_training %>%
  mutate(MPG = log(MPG),
         `Linear regression` = predict(fit_lm, car_training),
         `Random forest` = predict(fit_rf, car_training))

# Avalia-se a performance, a partir do valor real e dos valores estimados
metrics(results, truth = "MPG", estimate = `Linear regression`)
metrics(results, truth = "MPG", estimate = `Random forest`)


# ------------ Avaliando os Dados de Teste ------------ 
# Criam-se as novas colunas, com os valores estimados nos modelos
results <- car_testing %>%
  mutate(MPG = log(MPG),
         `Linear regression` = predict(fit_lm, car_testing),
         `Random forest` = predict(fit_rf, car_testing))

# Avalia-se a performance, a partir do valor real e dos valores estimados
metrics(results, truth = "MPG", estimate = `Linear regression`)
metrics(results, truth = "MPG", estimate = `Random forest`)

# Massa! As métricas não são piores na avaliação dos dados de testes em
# ambos os modelos, indicando que não tem-se overfitting em nenhum dos casos.

# Overfitting é um termo usado em estatística para descrever quando um modelo
# estatístico se ajusta muito bem ao conjunto de dados anteriormente observado,
# mas se mostra ineficaz para prever novos resultados. É comum que a amostra 
# apresente desvios causados por erros de medição ou fatores aleatórios.



# ------------ Treinando um Modelo com Resample ------------ 
# Resampling pode aprimorar a precisão de modelos de aprendizado de máquina e
# reduzir o overfitting.

# Ajustando os modelos com bootstrap resampling
cars_lm_bt <- train(log(MPG) ~ ., 
                    method = "lm", 
                    data = car_training,
                    trControl = trainControl(method = "boot"))

cars_rf_bt <- train(log(MPG) ~ ., 
                    method = "rf", 
                    data = car_training,
                    trControl = trainControl(method = "boot"))

# Uma olhada nos modelos
cars_lm_bt
cars_rf_bt


# ------------ Avaliando como performaram os modelos e comparando-os ------------ 

results <- car_testing %>%
  mutate(MPG = log(MPG),
         `Linear regression` = predict(cars_lm_bt, car_testing),
         `Random forest` = predict(cars_rf_bt, car_testing))

metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)


# ------------ Plotagem das predições do modelo, para inspecioná-las visualmente ------------ 
results %>%
  gather(Method, Result, `Linear regression`:`Random forest`) %>%
  ggplot(aes(MPG, Result, color = Method)) +
  geom_point(size = 1.5, alpha = 0.5) +
  facet_wrap(~Method) +
  geom_abline(lty = 2, color = "gray50") +
  geom_smooth(method = "lm")

# Ambos: as métricas dos modelos e as plotagens mostram que random forest model está
# performando melhor. Pode-se predizer eficiencia de conbustível mais precisamente
# com um random forest model.