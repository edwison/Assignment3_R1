library(readr)
combined_data <- read.csv('combined.csv')
plots_data <- read.csv('plots.csv')
species_data <-read.csv('species.csv')
surveys_data <-read.csv('surveys.csv')
rodents_data <- read.csv('Portal_rodents_19772002_scinameUUIDs.csv')

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("ggplot2")

library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)

#1 hindfoot length of male vs female for one give species (species: DM)
#2 hindfoot length vs weight of male (species DM)
#3 hindfoot length vs weight of female (species DM)

#1 hindfoot length of male vs female for one give species (species: DM)
#make new table contains DM
DM_data <- filter(surveys_data, species_id == 'DM',sex != '',!is.na(weight),!is.na(hindfoot_length)) %>% select(sex,hindfoot_length,weight)
DM_male <- filter(DM_data, sex == 'M')
DM_female <- filter(DM_data, sex == 'F')
summary(DM_male)
summary(DM_female)

ggplot(data=DM_data, aes (x=sex, y=hindfoot_length)) + geom_boxplot()

#2 hindfoot length vs weight of male (species DM)
ggscatter(DM_male, x = "hindfoot_length", y = "weight",title = "Relationship between weight and hindfoot_length of Male DM", xlab = "Hindfoot_length(mm)", ylab = "weight(gm)",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "red", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE ) + stat_cor(method = "pearson", label.x = 20, label.y = 100)

#scatter plot
scatter.smooth(x=DM_male$hindfoot_length, y=DM_male$weight, main="Hindfoot Length ~ Weight of Male DM")

# calculate correlation between hindfoot_length and weight of male DM.
cor(DM_male$hindfoot_length, DM_male$weight)

# build linear regression model on Male DM
linearModel_DM_Male <- lm(hindfoot_length ~ weight, data=DM_male)
print(linearModel_DM_Male)
  #linear model for Male DM
  #hindfoot = intercept+(B*weight)
  #hindfoot = 32.658+(0.08*weight)

#R Square - preparation
residuals(linearModel_DM_Male)
coefficients(linearModel_DM_Male)
modelSum_DM_Male <- summary(linearModel_DM_Male)

modelCoeff_DM_Male <- modelSum_DM_Male$coefficients

beta.estimate_DM_Male <- modelCoeff_DM_Male["weight", "Estimate"]

std.error_DM_Male <- modelCoeff_DM_Male["weight", "Std. Error"]

t_value_DM_Male <- beta.estimate_DM_Male/std.error_DM_Male

#p value
p_value_DM_male <- 2*pt(-abs(t_value), df=nrow(DM_male)-ncol(DM_male))

#fstatistic
f_statistic_DM_male <- linearModel_DM_Male$fstatistic[1]

#parameters for model p-value calc
f_DM_male <- summary(linearModel_DM_Male)$fstatistic

model_p_DM_Male <- pf(f[1], f[2], f[3], lower=FALSE)

#R Square


summary_DM_Male <- summary(linearModel_DM_Male)

#3 hindfoot length vs weight of female (species DM)
ggscatter(DM_female, x = "hindfoot_length", y = "weight",title = "Relationship between weight and hindfoot_length of Female DM", xlab = "Hindfoot_length(mm)", ylab = "weight(gm)",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE ) + stat_cor(method = "pearson", label.x = 20, label.y = 100)

#scatter plot
scatter.smooth(x=DM_female$hindfoot_length, y=DM_female$weight, main="Hindfoot Length ~ Weight of Female DM")

# calculate correlation between hindfoot_length and weight of male DM.
cor(DM_female$hindfoot_length, DM_female$weight)

# build linear regression model on Female DM
linearModel_DM_Female <- lm(hindfoot_length ~ weight, data=DM_female)
print(linearModel_DM_Female)
  #linear model for Male DM
  #hindfoot = intercept+(B*weight)
  #hindfoot = 33.111+(0.06*weight)

residuals(linearModel_DM_Female)
coefficients(linearModel_DM_Female)
modelSum_DM_Female <- summary(linearModel_DM_Female)
