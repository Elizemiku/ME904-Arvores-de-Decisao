library(tidymodels)
library(caret)
library(tidyverse)
library(ggpubr)

seguro <- read_csv("insurance.csv")

set.seed(123)
rnd_ind <- sample(1:nrow(seguro))
seguro_treino <- seguro[rnd_ind[1:round(nrow(seguro) * 0.8)],]
seguro_teste <- seguro[-rnd_ind[1:round(nrow(seguro) * 0.8)],]

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

(c_fumante_reg <-
    seguro %>%
    ggplot(aes(charges, fill = smoker)) +
    geom_histogram() +
    facet_wrap(. ~ region))

(c_fumante_bmi <-
    seguro %>%
    ggplot(aes(charges, bmi)) +
    geom_point() +
    facet_wrap(. ~ smoker))

(c_fumante_idade <-
    seguro %>%
    ggplot(aes(charges, age)) +
    geom_point() +
    facet_wrap(. ~ smoker))

ggsave("Valor_idade.jpg", c_idade, width = 16.5, height = 8, units = "cm")
ggsave("Valor_sexo.jpg", c_sex_box, width = 16.5, height = 8, units = "cm")
ggsave("Valor_bmi.jpg", c_bmi, width = 16.5, height = 8, units = "cm")
ggsave("Valor_regiao.jpg", c_regiao_box, width = 16.5, height = 8, units = "cm")
ggsave("Valor_fumo.jpg", c_fumante_hist, width = 16.5, height = 8, units = "cm")

mod_bag <-
  caret::train(charges ~ ., data = seguro_treino,
               method = 'treebag')

bag_teste <- predict(mod_bag, newdata = seguro_teste)

bag_teste_eqm <- mean((bag_teste - seguro_teste$charges)^2)

(g_bag_err <-
    tibble(Erro = (bag_teste - seguro_teste$charges),
           `Valor original` = seguro_teste$charges) %>%
    ggplot(aes(`Valor original`, Erro)) +
    geom_point() +
    geom_hline(yintercept = 0, colour = 'red') +
    scale_y_continuous(breaks = seq(20000, -20000, by = -5000)) +
    coord_cartesian(ylim = c(-20000, 20000)) +
    theme_bw())

mod_boost <-
  caret::train(charges ~ ., data = seguro_treino,
               method = 'blackboost',
               trControl = trainControl('cv', number = 10))

boost_teste <- predict(mod_boost, newdata = seguro_teste)

boost_teste_eqm <- mean((boost_teste - seguro_teste$charges)^2)

(g_boost_err <-
    tibble(Erro = (boost_teste - seguro_teste$charges),
           `Valor original` = seguro_teste$charges) %>%
    ggplot(aes(`Valor original`, Erro)) +
    geom_point() +
    geom_hline(yintercept = 0, colour = 'red') +
    scale_y_continuous(breaks = seq(20000, -20000, by = -5000)) +
    coord_cartesian(ylim = c(-20000, 20000)) +
    theme_bw())

(g_err <- ggarrange(g_bag_err, g_boost_err,
                    labels = c("A", "B"),
                    ncol = 2))

ggsave("Erro_prev_regressao.jpg", g_err, width = 16.5, height = 8, units = "cm")
