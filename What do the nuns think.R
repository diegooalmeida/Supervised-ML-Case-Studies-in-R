
# ------------ instalando/habilitando pacotes ------------ 
install.packages("tidyverse")
install.packages("rsample")
install.packages("caret")
install.packages("randomForest")
install.packages("yardstick")
install.packages("tidymodels")

library(tidyverse)
library(rsample)
library(caret)
library(randomForest)
library(yardstick)
library(tidymodels)

# ------------ Lendo dados ------------ 
getwd()
setwd("C:/Users/diego/Desktop/Curso SML/Capítulo 4/Dados")
sisters67 <- read_csv("sisters.csv")

glimpse(sisters67)


# ------------ Visualizando a distribuição de idade ------------
ggplot(sisters67, aes(x = age)) +
  geom_histogram(binwidth = 10)


# ------------ "Arrumando" os dados ------------ 
# Estrutura original
glimpse(sisters67)

# Tidy the data set
tidy_sisters <- sisters67 %>%
  select(-sister) %>%
  pivot_longer(-age, names_to = "question", values_to = "rating")

# Estrutura "arrumada"
glimpse(tidy_sisters)

# Concordância geral com as questões, por idade
tidy_sisters %>%
  group_by(age) %>%
  summarize(rating = mean(rating, na.rm = TRUE))

# Número de concordancias e discordancias, no geral
tidy_sisters %>%
  count(rating)

# Visualizando concordância, por idade
tidy_sisters %>%
  filter(question %in% paste0("v", 153:170)) %>%
  group_by(question, rating) %>%
  summarise(age = mean(age)) %>%
  ggplot(aes(rating, age, color = question)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point(size = 2) +
  facet_wrap(~question)


# ------------ Training, validation and testing data ------------ 
sisters_select <- sisters67 %>%
  select(-sister)

# Split off the testing set
set.seed(123)
sisters_split <- initial_split(sisters_select, strata = age)

sisters_other <- training(sisters_split)
sisters_test <- testing(sisters_split)

# Create the validation split
set.seed(123)
sisters_val <- validation_split(sisters_other, strata = age)

glimpse(sisters_val)


# ------------ identificando parâmetros de ajuste ------------ 
# Vamos começar com nosso ajuste de pré-processamento
sisters_recipe <- recipe(age ~ ., data = sisters_other) %>% 
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = tune())

sisters_recipe

# Em seguida, vamos construir nossa especificação de modelo com o ajuste
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_spec

# Por fim, reunimos nossas receitas e especificações de modelo em um fluxo
# de trabalho, para facilitar o uso.
tree_wf <- workflow() %>%
  add_recipe(sisters_recipe) %>%
  add_model(tree_spec)

tree_wf


# ------------ Criando uma grade de ajuste ------------
tree_grid <- grid_regular(num_comp(c(3, 12)),
                          cost_complexity(),
                          tree_depth(),
                          levels = 5)

glimpse(tree_grid)


# ------------ Hora de ajustar ------------
set.seed(123)
tree_res <- tune_grid(
  tree_wf,
  resamples = sisters_val,
  grid = tree_grid
)

glimpse(tree_res)


# ------------ Visualizando o resultado dos ajustes ------------
tree_metrics <- tree_res %>%
  collect_metrics() 

glimpse(tree_metrics)

tree_metrics %>%
  mutate(tree_depth = factor(tree_depth),
         num_comp = paste("num_comp =", num_comp),
         num_comp = fct_inorder(num_comp)) %>%
  ggplot(aes(cost_complexity, mean, color = tree_depth)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_x_log10() +
  facet_grid(.metric ~ num_comp, scales = "free")


# ------------ Encontrando os melhores parâmetros ------------ 
best_tree <- tree_res %>%
  select_best("rmse")

best_tree

# Atualizando o fluxo de trabalho com esses valores.
final_wf <- tree_wf %>% 
  finalize_workflow(best_tree)

final_wf


# ------------ Usando os dados de teste ------------ 
final_tree <- final_wf %>%
  last_fit(sisters_split) 

final_tree %>%
  collect_metrics()