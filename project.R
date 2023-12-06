setwd("C:/Users/verti/Downloads/r-final/StatsRProject")
setwd("c:/StatsRStudio/StatsRProject")

# 2 Catigorical
# FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

library(ggplot2)

heart <- read.csv(file="heart.csv", sep=",", header=T)
df <- data.frame(heart$Sex, heart$ChestPainType, heart$Age, heart$RestingBP, heart$MaxHR)

head(df)

ggplot(df, aes(x = heart.ChestPainType, y = heart.RestingBP)) +
geom_bar(stat="identity")
