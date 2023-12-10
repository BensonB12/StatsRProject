setwd("c:/StatsRStudio/StatsRProject")
install.packages("languageserver")
install.packages("ggplot2", dep=T)
#install.packages("ggmosaic")
#install.packages("devtools")
#devtools::install_github("haleyjeppson/ggmosaic")
library(ggplot2)

# 2 Catigorical
# FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]
# HeartDisease: true of false (1, 0)

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

heart <- read.csv(file="heart.csv", sep=",", header=T)

# Barplot
ggplot(heart, aes(x = ChestPainType, y = RestingBP)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(y= "Resting Blood Pressure (mm Hg)", x = "Chest Pain Type") +
  ggtitle("Residents in: Clevelend, Hungarian, \nSwitzerland, and California") +
  scale_x_discrete(labels = c('Asymptomatic','Atypical Angina','Non-Anginal', 'Typical Angina'))

# Mosaic Plot 
mosaicplot(~heart$Sex + heart$ChestPainType,
           xlab = "Sex",
           ylab = "Type of Chest Pain",
           main="Residents in: Clevelend, Hungarian, \nSwitzerland, and California")

# Stacked Bar Plot
condition <- rep(c("Has a Heart Disease", "Does not have Heart Disease"), 459)
ggplot(heart, aes(fill=condition, y=HeartDisease ,x=ChestPainType)) +
  geom_bar(position="stack", stat="identity") +
  labs(y= "Resting Blood Pressure (mm Hg)", x = "Chest Pain Type") +
  ggtitle("Residents in: Clevelend, Hungarian, \nSwitzerland, and California") +
  scale_x_discrete(labels = c('Asymptomatic','Atypical Angina','Non-Anginal', 'Typical Angina'))

# side by side bar plot
ggplot(heart, aes(fill=condition, y=Age, x=ChestPainType)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Residents in: Clevelend, Hungarian, \nSwitzerland, and California") +
  scale_x_discrete(labels = c('Asymptomatic','Atypical Angina','Non-Anginal', 'Typical Angina'))

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

ggplot(heart, aes(x = ChestPainType, y = RestingBP)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = ChestPainType, y = Age)) + 
  geom_bar(stat="identity")

ggplot(heart, aes(x = ChestPainType, y = MaxHR)) + 
  geom_bar(stat="identity")

ggplot(heart, aes(x = HeartDisease, y = RestingBP)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = HeartDisease, y = Age)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = HeartDisease, y = MaxHR)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = Sex, y = RestingBP)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = Sex, y = Age)) +
  geom_bar(stat="identity")

ggplot(heart, aes(x = Sex, y = MaxHR)) +
  geom_bar(stat="identity")