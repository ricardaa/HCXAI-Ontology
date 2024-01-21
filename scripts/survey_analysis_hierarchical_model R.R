# Dateipfad
dateipfad <- "C:/Users/ricar/Desktop/Data/survey_data.csv"

# Loa dataset
daten <- read.table(dateipfad, header = TRUE, sep = ",")

# Install package
install.packages("texreg")
install.packages("stargazer")
install.packages("broom.mixed")
install.packages("lme4")
install.packages("psych")
install.packages("irr")
install.packages("sjstats")
install.packages("lmerTest")
library(stargazer)
library(lmerTest)
library(lme4)
library(psych)
library(irr)
library(sjstats)
library(lmtest)
library(broom.mixed)
library(texreg)


# Intercept-Only Model/ Empty Model
m0 <- lmer(rating  ~ 1+(1 | ID), data = daten, REML=F )
# Intercept Only Model Summary
summary(m0)
# ICC of the intercept only model
performance::icc(m0)

# Random Intercept/ Fixed Slopes Model with L1 predictors
m1 <- lmer(rating ~ 1 +  xai_scope + expl_type + (1 | ID), data = daten, REML=F)
# Random Intercept Model Summary
summary(m1)
#Random Intercept ICC
performance::icc(m1)


# Random Intercept/ Fixed Slopes Model with L1 and L2 predictors
m2 <- lmer(rating ~ 1 +  xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1 | ID), data = daten, REML=F)
summary(m2)

# LR-Test m1 vs m2
lr_test_1 <- lrtest(m1, m2)
lr_test_1

# Random Slopes Model
m5 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1+xai_scope+expl_type| ID), data = daten, REML=F)
summary(m5)
#LR Test m2 vs m5
lr_test_2 <- lrtest(m2, m5)
lr_test_2

# Random Slope -> expl_type only as random effect
m6 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1+expl_type| ID), data = daten, REML=F)
summary(m6)

# Random Slopes Model -> xai_scope only as random effect
m7 <- lmer(rating ~ 1 +  xai_scope + expl_type +  age + gender + education + exp_index + att_index + tech_index + xai_scope:expl_type + (1+xai_scope | ID), data = daten, REML=F)
summary(m7)

# Cross Level Interaction
# xai_scope*XXX
m8 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + xai_scope*att_index + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m8)

lr_test_3 <- lrtest(m5, m8)
lr_test_3

# expl_type * XXX
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + expl_type*tech_index + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m9)
# LR Test m5 vs m9
lr_test_4 <- lrtest(m5, m9)
lr_test_4

# All significant interaction effects
m10 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + xai_scope*education + xai_scope*att_index + expl_type*exp_index  + expl_type*age + expl_type*att_index + expl_type*tech_index + (1+xai_scope+expl_type| ID), data = daten, REML=F)
summary(m10)
#LR Test m5 vs m10
lr_test_5 <- lrtest(m5, m10)
lr_test_5
