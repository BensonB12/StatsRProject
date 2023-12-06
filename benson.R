# Benson's File (So there are no merge conflicts)

setwd("c:/StatsRStudio/StatsRProject")

heart <- read.csv(file="heart.csv", sep=",", header=T)
df <- data.frame(heart$Sex, heart$ChestPainType, heart$Age, heart$RestingBP, heart$MaxHR)

# 2 Catigorical
# Sex: sex of the patient [M: Male, F: Female]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

library(ggplot2)

head(df)

ggplot(df, aes(x = heart.ChestPainType, y = heart.RestingBP)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.ChestPainType, y = heart.Age)) + 
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.ChestPainType, y = heart.MaxHR)) + 
  geom_bar(stat="identity")
