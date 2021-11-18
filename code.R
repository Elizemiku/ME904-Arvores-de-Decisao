library(tidymodels)
library(caret)
library(tidyverse)

seguro <- read_csv("insurance.csv")

set.seed(123)
rnd_ind <- sample(1:nrow(seguro))
seguro_treino <- seguro[rnd_ind[1:round(nrow(seguro) * 0.7)],]
seguro_teste <- seguro[-rnd_ind[1:round(nrow(seguro) * 0.7)],]

(g_idade <-
  ggplot(seguro, aes(age)) +
  geom_histogram())

(g_sex <-
    seguro %>%
    group_by(sex) %>%
    summarise(n = n()) %>%
    ggplot(aes(sex, n)) +
    geom_bar(stat = "identity"))

(g_bmi <-
    ggplot(seguro, aes(bmi)) +
    geom_histogram())

(g_fumante <-
    seguro %>%
    group_by(smoker) %>%
    summarise(n = n()) %>%
    ggplot(aes(smoker, n)) +
    geom_bar(stat = "identity"))

(g_regiao <-
    seguro %>%
    group_by(region) %>%
    summarise(n = n()) %>%
    ggplot(aes(region, n)) +
    geom_bar(stat = "identity"))

(g_valor <-
    ggplot(seguro, aes(charges)) +
    geom_histogram())

(c_idade <-
    ggplot(seguro, aes(charges, age)) +
    geom_point())

(c_sex_hist <-
    seguro %>%
    ggplot(aes(charges, fill = sex)) +
    geom_histogram() +
    facet_wrap(. ~ sex))

(c_sex_box <-
    seguro %>%
    ggplot(aes(y = charges, fill = sex)) +
    geom_boxplot() +
    facet_wrap(. ~ sex))

(c_bmi <-
    ggplot(seguro, aes(charges, bmi)) +
    geom_point())

(c_fumante_hist <-
    seguro %>%
    ggplot(aes(charges, fill = smoker)) +
    geom_histogram() +
    facet_wrap(. ~ smoker))

(c_regiao_hist <-
    seguro %>%
    ggplot(aes(charges, fill = region)) +
    geom_histogram() +
    facet_wrap(. ~ region))

(c_regiao_box <-
    seguro %>%
    ggplot(aes(y = charges, fill = region)) +
    geom_boxplot() +
    facet_wrap(. ~ region))

mod_bag <-
  caret::train(charges ~ ., data = seguro_treino,
               method = 'treebag',
               trControl = trainControl('cv', number = 10))

mod_boost <-
  caret::train(charges ~ ., data = seguro_treino,
               method = 'blackboost',
               trControl = trainControl('cv', number = 10))
