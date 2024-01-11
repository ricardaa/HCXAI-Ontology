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

#table
stargazer(daten)

# Nullmodel/ Intercept-Only Model/ Empty Model
m0 <- lmer(rating  ~ 1+(1 | ID), data = daten, REML=F )

summary(m0)

# icc mullmodel
performance::icc(m0)

# table for latex
# Extract fixed effects
fixed_effects <- fixef(m0)

# Convert fixed effects to a data frame
fixed_effects_df <- as.data.frame(fixed_effects)
stargazer(fixed_effects_df, title = "Intercept-Only Model", align = TRUE, type = "latex")
tidy_m0 <- tidy(m0)
stargazer(summary(m0))

# Random Intercept/ Fixed Slopes Model
m1 <- lmer(rating ~ 1 +  xai_scope + expl_type + (1 | ID), data = daten, REML=F)

summary(m1)
performance::icc(m1)

# plot residuals
plot(m2)


# Random Intercept/ Fixed Slopes Model
m2 <- lmer(rating ~ 1 +  xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1 | ID), data = daten, REML=F)
summary(m2)

print(m2, correlation=TRUE)

# LR-Test durch m1:m2
lr_test_1 <- lrtest(m1, m2)
lr_test_1

# Random Intercept: xai_scope only
m3 <- lmer(rating ~ 1 +  xai_scope + age + gender + education + exp_index + att_index + tech_index + (1 | ID), data = daten, REML=F)
summary(m3)

# Random Intercept: expl_type only
m4 <- lmer(rating ~ 1 +  expl_type + age + gender + education + exp_index + att_index + tech_index + (1 | ID), data = daten, REML=F)
summary(m4)

# Random Slopes Model -> xai_scope only as random effect
m5 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1+xai_scope+expl_type| ID), data = daten, REML=F)
summary(m5)
# isSingular error: according to arr et al. 2013, I "keep it maximal" -> fit the most complex model consistent with the experimental design, removing unimportant variables did not solve the error
#print(m5, correlation=TRUE)
#vcov(m5) 
texreg(list(m5,m10))
lr_test_2 <- lrtest(m2, m5)
lr_test_2

# Random Slope -> expl_type only as random effect
m6 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + (1+expl_type| ID), data = daten, REML=F)
summary(m6)

# test424
m6 <- lmer(rating ~ 1 +  xai_scope + expl_type +  age + gender + education + exp_index + att_index + tech_index + xai_scope:expl_type + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m6)

# model with all variables + random slope, but without interactions
m7 <- lmer(rating ~ 1 +  xai_scope + expl_type +  age + gender + education + expertise + att + techaff + job_groups + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m7)

# model only job_groups
m8 <- lmer(rating ~ 1 +  xai_scope + expl_type + job_groups + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m8)

# cross level interaction
# xai_scope*expertise
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + xai_scope*att_index + (1+xai_scope | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

# expl_type * XXX + only xai_scope as random effect
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + expl_type*tech_index + (1+xai_scope | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

#expl_type*job_groups
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + exp_index + age + gender + education + att_index + tech_index + job_groups + expl_type*tech_index + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

#expl_type*expertise
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + expertise + age + gender + education + att + techaff + job_groups + expl_type*expertise + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

#expl_type*tech_aff
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + expertise + age + gender + education + att + techaff + job_groups + expl_type*ethics_ai + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

#xai_scope*xxx
m9 <- lmer(rating ~ 1 + xai_scope + expl_type + expertise + age + gender + education + att + techaff + job_groups + xai_scope*use_apply + (1+xai_scope+expl_type | ID), data = daten, REML=F)
summary(m9)

lr_test_3 <- lrtest(m5, m9)
lr_test_3

# all signifikant interaction effects + no gender
m10 <- lmer(rating ~ 1 + xai_scope + expl_type + age + gender + education + exp_index + att_index + tech_index + job_groups + xai_scope*education + xai_scope*att_index + expl_type*exp_index  + expl_type*age + expl_type*att_index + expl_type*tech_index + (1+xai_scope+expl_type| ID), data = daten, REML=F)
summary(m10)

# xai_scope*education + xai_scope*att_index + expl_type*exp_index  + expl_type*att_index + expl_type*tech_index

lr_test_4 <- lrtest(m5, m10)
lr_test_4

cor(daten)

library(car)
vif(m10)

1 / vif(m10)

eigen(cor(daten))$values

