rm(list = ls())

##### PACKAGES ####

library(tidyverse)
library(Hmisc)
library(lme4)
library(sjPlot)
library(DHARMa)
library(bestNormalize)
library(lmerTest)
library(effects)
library(ggeffects)
library(car)
library(emmeans)

#### 2D - SIMPLE TEST ####

simp_data <- read.csv("recmem_2D_test.csv")

##data used for player description:

descriptive_data <- simp_data %>%
  distinct(id, .keep_all = TRUE)

#Gender

descriptive_data %>% 
  group_by(platform) %>% 
  count(gender) %>% 
  mutate(f = n/sum(n)) %>% 
  ungroup()

#Age

descriptive_data %>% 
  mutate(age = ifelse(age == 17, 18, age)) %>% 
  group_by(platform, gender) %>% 
  summarise(mean = mean(age), median = median(age), 
            infCI = quantile(age, 0.025), 
            supCI = quantile(age, 0.975)) %>% 
  ungroup()

descriptive_data %>% slice_max(age) %>% select(age)
descriptive_data %>% slice_min(age) %>% select(age)
mean(descriptive_data$age)

### ANALYSIS OF SUCCESS 

## BOTH SETUPS TOGETHER

simp_data <- simp_data %>% 
  mutate(type = relevel(as_factor(type),ref = "No collision")) #ref category

mm_1 <- glmer(success ~ gender*type*showBefore + (1|id),
               data = simp_data, family = binomial,
               control = glmerControl("bobyqa"))
summary(mm_1)
tab_model(mm_1)

sim.out <- simulateResiduals(fittedModel = mm_1, 
                             plot = FALSE)
plot(sim.out)

mm_2 <- glmer(success ~ showBefore + (1|id),
               data = simp_data, family = binomial,
               control = glmerControl("bobyqa"))
summary(mm_2)
tab_model(mm_2)

sim.out <- simulateResiduals(fittedModel = mm_2, 
                             plot = FALSE)
plot(sim.out)

#CONTROL MIXED MODELS: only stimulus not shown

notshown_data <- simp_data %>% 
  filter(showBefore == "New")

modelo_1n <- glmer(success ~ type + gender + (1|id),
                   data = notshown_data, family = binomial)
summary(modelo_1n)
tab_model(modelo_1n)

sim.out <- simulateResiduals(fittedModel = modelo_1n, 
                             plot = FALSE)
plot(sim.out)

#subsetting by showBefore

shown_data <- simp_data %>% 
  filter(showBefore == "Old")

modelshown_1 <- glmer(success ~ type*gender  + (1|id),
                      data = shown_data, family = binomial)
summary(modelshown_1)
tab_model(modelshown_1)

sim.out <- simulateResiduals(fittedModel = modelshown_1, 
                             plot = FALSE)
plot(sim.out)

#subsetting by gender: effect of type

men_data <- shown_data %>% 
  filter(gender == "Men")

modelmen <- glmer(success ~ type + (1|id),
                  data = men_data, family = binomial)
summary(modelmen)
tab_model(modelmen)


sim.out <- simulateResiduals(fittedModel = modelmen, 
                             plot = FALSE)
plot(sim.out)

women_data <- shown_data %>% 
  filter(gender == "Women")

modelwomen <- glmer(success ~ type + (1|id),
                    data = women_data, family = binomial)
summary(modelwomen)
tab_model(modelwomen)


sim.out <- simulateResiduals(fittedModel = modelwomen, 
                             plot = FALSE)
plot(sim.out)

#setup differences


modelo <- glmer(success ~ platform + gender*type*showBefore + (1|id), 
                data = simp_data,
                family = binomial, control = glmerControl("bobyqa"))
summary(modelo)
tab_model(modelo)

sim.out <- simulateResiduals(fittedModel = modelo, 
                             plot = FALSE)
plot(sim.out)

#graphic results:

ggeffect(modelshown_1, terms = c("type [all]"))

ggeffect(model = modelshown_1, terms = c("gender [all]", "type"), ci_level = 0.68)

#TRIAL

modeloTR4 <- glmer(success ~ trial + (1|id),
                   data = shown_data, family = binomial,
                   control = glmerControl("bobyqa"))
summary(modeloTR4)

tab_model(modeloTR4)

sim.out <- simulateResiduals(fittedModel = modeloTR4, 
                             plot = FALSE)
plot(sim.out)

#FILE

modeloF3 <- glmer(success ~ file + (1|id),
                  data = shown_data, family = binomial,
                  control = glmerControl("bobyqa"))
summary(modeloF3)
Anova(modeloF3)

sim.out <- simulateResiduals(fittedModel = modeloF3, 
                             plot = FALSE)
plot(sim.out)

##REACTION TIME

time_data <- simp_data %>%
  filter(platform == "Matlab") %>% 
  mutate(success = as.character(success))

time_data$reactionTime <- time_data$reactionTime * 1000

bestNormalize(time_data$reactionTime)
set.seed(101)
yeo <- boxcox(time_data$reactionTime)

time_data$transTime <- predict(yeo)

plot(density(time_data$transTime))


mmrt_11 <- lmer(transTime ~ success + trial + (1|id), 
                data = time_data) 
summary(mmrt_11)

tab_model(mmrt_11)  

mmrt_11b <- lmer(reactionTime ~ success + trial + (1|id), 
                 data = time_data) 
summary(mmrt_11b)
tab_model(mmrt_11b)

sim.out <- simulateResiduals(fittedModel = mmrt_11, 
                             plot = FALSE)
plot(sim.out)

#### 3D - COMPLEX TEST ####

compl_data <- read.csv("recmem_3D_test.csv")

##data used for player description:

descriptive_data <- compl_data %>%
  distinct(id, .keep_all = TRUE)

#Gender

descriptive_data %>% 
  count(gender) %>% 
  mutate(f = n/sum(n))

#Age

descriptive_data <- descriptive_data %>% 
  mutate(age = ifelse(age == 17, 18, age))
descriptive_data %>% 
  group_by(gender) %>% 
  summarise(mean = mean(age), median = median(age), 
            infCI = quantile(age, 0.025), 
            supCI = quantile(age, 0.975)) %>% 
  ungroup()

descriptive_data %>% slice_max(age) %>% select(age)
descriptive_data %>% slice_min(age) %>% select(age)
mean(descriptive_data$age)

### ANALYSIS OF SUCCESS 

compl_data <- compl_data %>% 
  mutate(type = relevel(as_factor(type),ref = "No collision")) #ref category

mm0_5 <- glm(success ~ showBefore*type,
             data = compl_data, family = binomial)
summary(mm0_5)
tab_model(mm0_5)

sim.out <- simulateResiduals(fittedModel = mm0_5, 
                             plot = FALSE)
plot(sim.out)

mm0_7 <- glm(success ~ showBefore,
             data = compl_data, family = binomial)
tab_model(mm0_7)
summary(mm0_7)

sim.out <- simulateResiduals(fittedModel = mm0_7, 
                             plot = FALSE)
plot(sim.out)

##shown before
shown_before_data <- compl_data %>% 
  filter(showBefore == "Old")

modeloTR4 <- glm(success ~ type*trial + gender,
                 data = shown_before_data, family = binomial)

summary(modeloTR4)
tab_model(modeloTR4)

sim.out <- simulateResiduals(fittedModel = modeloTR4, 
                             plot = FALSE)
plot(sim.out)

modeloTR3 <- glm(success ~ type*trial,
                 data = shown_before_data, family = binomial)

summary(modeloTR3)
tab_model(modeloTR3)

sim.out <- simulateResiduals(fittedModel = modeloTR3, 
                             plot = FALSE)
plot(sim.out)

#subsetting by type

collision <- shown_before_data %>% filter(type == "Collision")
no_collision <- shown_before_data %>% filter(type == "No collision")

modeloTR_col <- glm(success ~ trial,
                    data = collision, family = binomial)
summary(modeloTR_col)
tab_model(modeloTR_col)

sim.out <- simulateResiduals(fittedModel = modeloTR_col, 
                             plot = FALSE)
plot(sim.out)

modeloTR_noc <- glm(success ~ trial,
                    data = no_collision, family = binomial)
summary(modeloTR_noc)
tab_model(modeloTR_noc)

sim.out <- simulateResiduals(fittedModel = modeloTR_noc, 
                             plot = FALSE)
plot(sim.out)

#graphic results:

efecto_t0 <- emmeans(modeloTR3, pairwise ~ type, at = list(trial = 0),
                     type = "response")

summary(efecto_t0)


ggeffect(modeloTR3, terms = c("trial [all]", "type"), ci.lvl = 0.68)

#FILE

modelof_4 <- glm(success ~ file,
                 data = shown_before_data, family = binomial)
summary(modelof_4)

Anova(modelof_4)

sim.out <- simulateResiduals(fittedModel = modelof_4, 
                             plot = FALSE)
plot(sim.out)

#### LOOMING TEST ####

loom_data <- read.csv("recmem_looming_test.csv")

##data used for player description:

descriptive_data <- loom_data %>%
  distinct(id, .keep_all = TRUE)

#Gender

descriptive_data %>% 
  count(gender) %>% 
  mutate(f = n/sum(n))

#Age

descriptive_data <- descriptive_data %>% 
  mutate(age = ifelse(age == 17, 18, age))
descriptive_data %>% 
  group_by(gender) %>% 
  summarise(mean = mean(age), median = median(age), 
            infCI = quantile(age, 0.025), 
            supCI = quantile(age, 0.975)) %>% 
  ungroup()

min(descriptive_data$age)
max(descriptive_data$age)
mean(descriptive_data$age)


## SUCCESS

mm0_6 <- glm(success ~ showBefore, 
             data = loom_data, family = binomial)
summary(mm0_6)
tab_model(mm0_6)

sim.out <- simulateResiduals(fittedModel = mm0_6, 
                             plot = FALSE)
plot(sim.out)

#SHOWN BEFORE

shown_before_data <- loom_data %>% filter(showBefore == "Old")

mm1_2 <- glm(success ~ gender + type,
             data = shown_before_data, family = binomial)
summary(mm1_2)
tab_model(mm1_2)

sim.out <- simulateResiduals(fittedModel = mm1_2, 
                             plot = FALSE)
plot(sim.out)

# graphic results

mm1_8 <- glm(success ~ type,
             data = shown_before_data, family = binomial)

ggeffect(mm1_8, terms = "type", ci.lvl = 0.68)

#FILE

modelof_4 <- glm(success ~ file,
                 data = shown_before_data, family = binomial)
summary(modelof_4)

Anova(modelof_4)

sim.out <- simulateResiduals(fittedModel = modelof_4, 
                             plot = FALSE)
plot(sim.out)

#### LOOMING AND COMPLEX TESTS ####

compl_data <- compl_data %>% mutate(experiment = "Complex")
loom_data <- loom_data %>% mutate(experiment = "Looming")

together <- compl_data %>% bind_rows(loom_data)

together_f <- together %>% filter(trial == 0 & 
                                    (type == "Collision" |
                                       type == "Receding") &
                                    showBefore == "Old")

modelo <- glm(success ~ experiment, data = together_f, family = binomial)
summary(modelo)

tab_model(modelo)

sim.out <- simulateResiduals(fittedModel = modelo, 
                             plot = FALSE)
plot(sim.out)

#graphic results

ggeffect(modelo, terms = c("experiment"), ci.lvl = 0.68)
