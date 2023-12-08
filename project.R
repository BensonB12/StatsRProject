library(ggplot2)

setwd("C:/Users/verti/Downloads/r-final/StatsRProject")
setwd("c:/StatsRStudio/StatsRProject")

# 2 Catigorical
# FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

heart <- read.csv(file="heart.csv", sep=",", header=T)
df <- data.frame(heart$Sex, heart$ChestPainType, heart$Age, heart$RestingBP, heart$MaxHR, heart$HeartDisease)

head(df)

# Barplot
ggplot(heart, aes(x = ChestPainType, y = RestingBP)) +
  geom_bar(stat="identity", fill="steelblue")

# Scatterplot with fit line
plot(heart$Age, heart$RestingBP)
abline(lm(heart$RestingBP ~ heart$Age, data = heart), col = "blue")

#Sbs Box + Whisker Plot
boxplot(heart$MaxHR ~ heart$Sex)

t.test(heart$MaxHR ~ heart$Sex)