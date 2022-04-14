library(data.table)
library(dplyr)
library(tidyverse)
library(gdata)
library(lme4)
library(ordinal)

result <- fread(file = '~/Desktop/result.csv')
view(result)

plot(density(result$avgC))
qqnorm(result$avgC)
qqline(result$avgC)

plot(density(result$avgT))
qqnorm(result$avgT)
qqline(result$avgT)

boxplot(CR ~ order,result)
avgT.model <- lmer(avgT ~ 1+condition+order+(1|subject), result, REML = FALSE)
avgT.none <- lmer(avgT ~ 1+condition+(1|subject), result, REML = FALSE)
anova(avgT.model,avgT.none)
summary(avgT.model)
avgC.model <- lmer(avgC ~ 1+condition+(1|subject)+(1|order), avgset, REML = FALSE)
avgC.none <- lmer(avgC ~ 1+(1|subject)+(1|order), avgset, REML = FALSE)
anova(avgC.model,avgC.none)
summary(avgC.model)
CRset <- result[which(result$subject != 5 & result$subject != 2 & result$subject != 1& result$subject != 6),]
CR.model <- lmer(CR ~ 1+condition+(1|subject)+(1|order), CRset, REML = FALSE)
CR.none <- lmer(CR ~ 1+(1|subject)+(1|order), CRset, REML = FALSE)
anova(CR.model,CR.none)
summary(CR.model)

boxplot(CR ~ condition, CRset)

m1 <- clmm(as.factor(Degree) ~ 1 + order + (1|subject), data=result)
m2 <- clmm(as.factor(Degree) ~ 1 + (1|subject), data=result)
anova(m1,m2)
print(summary(m2))