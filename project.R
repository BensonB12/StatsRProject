install.packages("languageserver")
install.packages("ggplot2", dep=T)
install.packages("fitdistrplus")
library(fitdistrplus)
library(ggplot2)
library(MASS)

setwd("c:/StatsRStudio/StatsRProject")

# 2 Catigorical
# FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
# ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic]

# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

heart <- read.csv(file="heart.csv", sep=",", header=T)

# Barplot
ggplot(heart, aes(x = ChestPainType, y = RestingBP)) +
  geom_bar(stat="identity", fill="steelblue")

# Mosaic Plot
mosaicplot(~heart$Sex + heart$ChestPainType, gp = shading_max,
    xlab = "Sex",
    ylab = "Type of Chest Pain",
    split_vertical = TRUE, 
    main="Differences of Chest Pain by Gender")

# Stacked Bar Plot
ggplot(heart, aes(fill=condition, y=Age, x=ChestPainType)) +
  geom_bar(position="stack", stat="identity")

# side by side bar plot
condition <- rep(c("has Heart Disease", "Does not have HeartDisease"), 459)
ggplot(heart, aes(fill=condition, y=Age, x=ChestPainType)) +
  geom_bar(position="dodge", stat="identity")

# Side by Side Box + Whisker Plot
boxplot(heart$MaxHR ~ heart$Sex)
boxplot(heart$Cholesterol ~ heart$HeartDisease)

# Histogram
hist(heart$Age)
hist(heart$RestingBP)
hist(heart$MaxHR)

# Scatterplot with fit line
plot(heart$Age, heart$RestingBP)
abline(lm(heart$RestingBP ~ heart$Age, data = heart), col = "blue")

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


###################################################
# 3 Numerical
# Age: age of the patient [years]
# RestingBP: resting blood pressure [mm Hg]
# MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]

# 1. A “fit” of a numeric variable using a continuous distribution we have learned.
fit.gamma <- fitdist(heart$MaxHR, distr = "gamma", method = "mle")
plot(fit.gamma)

# 2. Anova
anv <- aov(lm(RestingBP ~ HeartDisease, data=heart))
summary(anv)

# 3. At least 2 2-sample t-tests with appropriate correction.
    # Is there a significant difference in MaxHR between Sex categories?
t.test(heart$MaxHR ~ heart$Sex)
    # Is there a significant difference in presence of HeartDisease between Sex categories?
t.test(heart$HeartDisease ~ heart$Sex)

# 4. Chi-square test for independence.
    # Is there a significant association between chest pain type and the presence of heart disease?
chi <- chisq.test(heart$ChestPainType, heart$HeartDisease)
chi$expected
summary(heart$ChestPainType)

length(heart$ChestPainType)

# 5. Chi-square Goodness of fit test
chisq.test(heart$HeartDisease, chi$expected)


# 6. 2 proportion z-test
    # Is there a significant association between the presence of heart disease and sex?
prop.test(table(heart$Sex, heart$HeartDisease))

# 7. 1 proportion z-test
    # Test if the proportion of heart disease in the dataset is 0.5
prop.test(heart$HeartDisease[1], sum(heart$HeartDisease), p = 0.5)

# 8. Paired sample t-test.
    ##### Not possible. Explain why

# 9. 3 confidence intervals of your choice.



# 10. A linear regression model
    # Can we predice the presense of heart disease from age?
lm(HeartDisease ~ Age, data = heart)

# 11. A multilinear regression model
    # How do we predict the presence of heart disease from Age, Sex, MaxHR, FastingBS, and ChestPainType?
lm(HeartDisease ~ Age + Sex + MaxHR + FastingBS + ChestPainType, data = heart)