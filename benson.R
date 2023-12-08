# Benson's File (So there are no merge conflicts)

setwd("c:/StatsRStudio/StatsRProject")

heart <- read.csv(file="heart.csv", sep=",", header=T)
df <- data.frame(heart$Sex, heart$ChestPainType, heart$Age, heart$RestingBP, heart$MaxHR, heart$HeartDisease)

# (3) Catigorical
# Sex: sex of the patient [M: Male, F: Female]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
# HeartDisease: output class [1: heart disease, 0: Normal]

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

library(ggplot2)
install.packages("crayon")

head(df)

ggplot(df, aes(x = heart.ChestPainType, y = heart.RestingBP)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.ChestPainType, y = heart.Age)) + 
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.ChestPainType, y = heart.MaxHR)) + 
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.HeartDisease, y = heart.RestingBP)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.HeartDisease, y = heart.Age)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.HeartDisease, y = heart.MaxHR)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.Sex, y = heart.RestingBP)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.Sex, y = heart.Age)) +
  geom_bar(stat="identity")

ggplot(df, aes(x = heart.Sex, y = heart.MaxHR)) +
  geom_bar(stat="identity")

summary(heart$ChestPainType)

# side by side bar plot
condition <- rep(c("has Heart Disease", "Does not have HeartDisease"), 459)
ggplot(df, aes(fill=condition, y=heart$Age, x=heart$ChestPainType)) +
  geom_bar(position="dodge", stat="identity")

ggplot(df, aes(fill=condition, y=heart$Age, x=heart$ChestPainType)) +
  geom_bar(position="stack", stat="identity")
