# CA1 for Justin Thomas. World Happiness Dataset

# read in dataset

happy <- read.csv("World-Happiness.csv")

str(happy)

# country a character variable, year an integer and the rest all numerical variables

summary(happy)

# summary statistics for all the numerical variables



# Research Questions
# 1	Does the GDP of a country affect the healthy life expectancy at birth?
# 2	Does the generosity of a county affect the social support of a country?
# 3	Does the freedom of life choices in a country affect the perception of corruption in a country?
# 4	Does the ‘Positive Affect’ affect the ‘Negative Affect’?
# 5	Does the Year affect the Life Ladder score?


# Data Preparation

# examine missing data
library(mice)
md.pattern(happy)
nrow(happy)

# 1708 rows not missing data out off 1949
# for many of our tests we will just be using two columns so if 
# another column in the row is missing data, it won't matter
# we will drop any rows if data in the columns we are analysing is missing.

library(psych)

pairs.panels(happy,
             smooth = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             pch = 21,
             lm = FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factor = 2,
             hist.col = 4,
             cex.labels = 1,
             ci = TRUE)


# Q1
# Check distribution of GDP and Life Expectancy data
# We could see from the pysch plot, in the histograms of the 2 columns that one could possibly make a case for GDP being normally distributed but
# life expectancy certainly is not. 

# attach dataset for ease
attach(happy)

# remember parameters for resetting
opar = par(no.readonly = TRUE)
par(mfrow = c(1,2))

# Histogram Plots
hist(Log.GDP.per.capita,col = "red", main = "Distribution of GDP data")
hist(Healthy.life.expectancy.at.birth, col = "red", main = "Distribution of Life Expectancy Data")


# QQ Plots
with (happy, 
      {qqnorm (Log.GDP.per.capita, 
               main = "normal qq plot of GDP data", 
               xlab = "Theoretical quantiles",
               ylab = "sample quantiles")
        qqline(Log.GDP.per.capita)
      })
with (happy, 
      {qqnorm (Healthy.life.expectancy.at.birth, 
               main = "Normal qq plot of Life Expectancy data", 
               xlab = "Theoretical quantiles",
               ylab = "sample quantiles")
        qqline(Healthy.life.expectancy.at.birth)
      })

# formal test of normality
# shapiro-wilks test
# p value tells us the chances that the sample
# comes from a normally dist
# If p > 0.05 = normally dist

normality_test1 <- shapiro.test(happy$Log.GDP.per.capita)
normality_test1$p.value
# p value is 2.342605e-20 so GDP not normal

normality_test2 <- shapiro.test(happy$Healthy.life.expectancy.at.birth)
normality_test2$p.value
# p value is 7.554077e-26 so life expectancy not normal

# This confirms that we can't use a parametric statistical test, we need to use a non-parametric test. 
# For 2 continuous variables, the one we chose is the Spearmans Correlation Co-Efficiient

res1 <-cor.test(happy$Log.GDP.per.capita, happy$Healthy.life.expectancy.at.birth,  method = "spearman", exact = FALSE)
res1


# Q2
# Check distribution of life expectancy and social support data
# We could see from the pysch plot, in the histograms of the 2 columns they don't look normailised, they look skewed but we will double check

# formal test of normality
# shapiro-wilks test
# p value tells us the chances that the sample
# comes from a normally dist
# If p > 0.05 = normally dist


normality_test4 <- shapiro.test(happy$Social.support)
normality_test4$p.value
# p value is 1.976689e-31 so social support confirmed as not normal

# This confirms that we can't use a parametric statistical test, we need to use a non-parametric test. 
# For 2 continuous variables, the one we chose is the Spearmans Correlation Co-Efficiient

res2 <-cor.test(happy$Social.support, happy$Healthy.life.expectancy.at.birth,  method = "spearman", exact = FALSE)
res2


# Q3
# Check distribution of freedom to make life choices and positive affect data
# We could see from the pysch plot, in the histograms of the 2 columns they don't look normailised, they look skewed but we will double check


# Histogram Plots
par(mfrow = c(1,2))
hist(Freedom.to.make.life.choices,col = "red", main = "Distribution of Freedom to make Life Choices data")
hist(Positive.affect, col = "red", main = "Distribution of Positive Affect Data")


# formal test of normality
# shapiro-wilks test
# p value tells us the chances that the sample
# comes from a normally dist
# If p > 0.05 = normally dist

normality_test5 <- shapiro.test(happy$Freedom.to.make.life.choices)
normality_test5$p.value
# p value is 6.076065e-22 so freedom to make life confirmed as not normal

normality_test6 <- shapiro.test(happy$Positive.affect)
normality_test6$p.value
# p value is  3.227913e-18 so Positive Affect confirmed as not normal

# This confirms that we can't use a parametric statistical test, we need to use a non-parametric test. 
# For 2 continuous variables, the one we chose is the Spearmans Correlation Co-Efficiient

res3 <-cor.test(happy$Freedom.to.make.life.choices, happy$Positive.affect,  method = "spearman", exact = FALSE)
res3



# Q4

# formal test of normality
# shapiro-wilks test
# p value tells us the chances that the sample
# comes from a normally dist
# If p > 0.05 = normally dist

normality_test7 <- shapiro.test(happy$Life.Ladder)
normality_test7$p.value
# p value is 2.610763e-12 so life ladder data is not normally distributed

hist(Life.Ladder)
with (happy, 
      {qqnorm (Life.Ladder, 
               main = "normal qq plot of life ladder data", 
               xlab = "Theoretical quantiles",
               ylab = "sample quantiles")
        qqline(Life.Ladder)
      })

res4 <-cor.test(happy$Log.GDP.per.capita, happy$Life.Ladder,  method = "spearman", exact = FALSE)
res4


# Q5
attach(happy)
par(mfrow = c(1,2))
hist(year,col = "red", main = "Distribution of Year data")
hist(Perceptions.of.corruption, col = "red", main = "Distribution of Perceptions of Corruption Data")

# formal test of normality
# shapiro-wilks test
# p value tells us the chances that the sample
# comes from a normally dist
# If p > 0.05 = normally dist

normality_test8 <- shapiro.test(happy$Perceptions.of.corruption)
normality_test8$p.value
# p value is 6.538547e-39 so corruption data is not normally distributed

wilcox.test(Perceptions.of.corruption ~ year)
# tried this initially but year has more than 2 levels so would not work


kruskal.test(Perceptions.of.corruption, year)
# not convinced this was correct

res5 <-cor.test(happy$year, happy$Perceptions.of.corruption,  method = "spearman", exact = FALSE)
res5

