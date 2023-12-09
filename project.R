install.packages("languageserver")
install.packages("ggplot2", dep=T)
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

# Mosaic Plot``
heartdata <- xtabs(~heart$Sex + heart$ChestPainType, data = heart)
mosaicplot(heartdata, gp = shading_max,
           xlab = "Sex",
           ylab = "Type of Chest Pain",
       split_vertical = TRUE, 
       main="Differences of Chest Pain by Gender")

# Stacked Bar Plot
ggplot(df, aes(fill=condition, y=heart$Age, x=heart$ChestPainType)) +
  geom_bar(position="stack", stat="identity")

# side by side bar plot
condition <- rep(c("has Heart Disease", "Does not have HeartDisease"), 459)
ggplot(df, aes(fill=condition, y=heart$Age, x=heart$ChestPainType)) +
  geom_bar(position="dodge", stat="identity")

# Side by Side Box + Whisker Plot
boxplot(heart$MaxHR ~ heart$Sex)

# Histogram
hist(heart$Age)
hist(heart$RestingBP)
hist(heart$MaxHR)

# Scatterplot with fit line
plot(heart$Age, heart$RestingBP)
abline(lm(heart$RestingBP ~ heart$Age, data = heart), col = "blue")



t.test(heart$MaxHR ~ heart$Sex)

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




