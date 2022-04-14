library(data.table)
library(dplyr)
library(tidyverse)
library(gdata)
library(lme4)
library(ordinal)

result <- fread(file = '~/Desktop/result.csv')
view(result)

boxplot(CR ~ condition, CRset)
CRset <- result[which(result$subject != 5 & result$subject != 2 & result$subject != 1& result$subject != 6),]
CR.model <- lmer(CR ~ 1+condition+(1|subject)+(1|order), CRset, REML = FALSE)
CR.none <- lmer(CR ~ 1+(1|subject)+(1|order), CRset, REML = FALSE)
anova(CR.model,CR.none)
summary(CR.model)

plot(density(result$avgC))
qqnorm(result$avgC)
qqline(result$avgC)

plot(density(result$avgT))
qqnorm(result$avgT)
qqline(result$avgT)

avgT.model <- lmer(avgT ~ 1+condition+order+(1|subject), result, REML = FALSE)
avgT.none <- lmer(avgT ~ 1+condition+(1|subject), result, REML = FALSE)
anova(avgT.model,avgT.none)
summary(avgT.model)

avgC.model <- lmer(avgC ~ 1+condition+(1|subject)+(1|order), avgset, REML = FALSE)
avgC.none <- lmer(avgC ~ 1+(1|subject)+(1|order), avgset, REML = FALSE)
anova(avgC.model,avgC.none)
summary(avgC.model)
