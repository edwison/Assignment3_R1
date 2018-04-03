library(readr)
combined_data <- read.csv('combined.csv')
plots_data <- read.csv('plots.csv')
species_data <-read.csv('species.csv')
surveys_data <-read.csv('surveys.csv')
rodents_data <- read.csv('Portal_rodents_19772002_scinameUUIDs.csv')

install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)

#1 hindfoot length of male vs female for one give species (species: DM)
#2 hindfoot length of one species reptiles vs one species rodents
#3 weights for males vs female for one gives species

#1 hindfoot length of male vs female for one give species (species: DM)
#make new table contains DM
DM_data <- filter(surveys_data, species_id == 'DM')
select(DM_data, species_id:sex:hindfoot_length)
DM_male <- filter(DM_data3, sex == 'M')
DM_female <- filter(DM_data3, sex == 'F')
summary(DM_male)
summary(DM_female)
mean(DM_male$hindfoot_length, na.rm = TRUE)
mean(DM_female$hindfoot_length, na.rm = TRUE)
rm(DM_data3)

install.packages("ggplot2")
library(ggplot2)
ggplot(data=subset(DM_3, !is.na(sex)), aes (x=sex, y=hindfoot_length)) + geom_boxplot()
ggplot(data=subset(DM_3), aes (x=sex, y=hindfoot_length)) + geom_boxplot()

#2 hindfoot length of one species reptiles vs one species rodents
