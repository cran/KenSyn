# kensyn Package. Knowledge synthesis in Agriculture : from experimental network to meta-analyisis.
# ch03. Network of experiments : complete blocks. Wheat varieties on a single year
# Francois Piraux (Arvalis) 2017-11-01
# R version 3.1.3
# loading packages
library(lme4) # version 1.1-10
#library(lsmeans) # version 2.20-23 deprecated => emmeans TODO
library(emmeans) # version 2.20-23
library(car) # version 2.0-25
library(outliers) # Version: 0.14
library(reshape2)
# required : pbkrtest, lmerTest, multcompView

#################################################################################
#### data on evaluation of wheat varieties
DF <-wheat_var

DF$experimentation <- as.factor(DF$experimentation)
DF$bloc <- as.factor(DF$bloc)
DF$variete <- factor(DF$variete, levels=unique(DF$variete))

head(DF,13)
str(DF)
summary(DF)
# table of means for each variety on each experiment
dcast(DF,  experimentation ~ variete, fun.aggregate=function(x) {round(mean(x),1)}, value.var="rdt")
#################################################################################
#### model with experiment as random effect - individual data
## fitting the model
res.lmer2 <- lmer(rdt ~ variete + (1|experimentation) +(1|experimentation:variete)  +(1|experimentation:bloc), data=DF, na.action=na.exclude)
res.lmer2

## component of the variance
print(VarCorr(res.lmer2), comp="Variance")

## means per variety and comparison between means
# moy_var2 <- lsmeans(res.lmer2, ~variete)  # deprecated => emmeans # TODO
moy_var2 <- emmeans(res.lmer2, ~variete)
moy_var2

confint(contrast(moy_var2, method="tukey", adjust="none"))

#### model with experiment as fixed effect - individual data
## fitting the model
res.lmer1 <- lmer(rdt ~ variete + experimentation + experimentation:variete + (1|experimentation:bloc), data=DF, na.action=na.exclude)
res.lmer1

## component of the variance
print(VarCorr(res.lmer1), comp="Variance")

## means per variety and comparison between means
# moy_var1 <- lsmeans(res.lmer1, ~variete)  # deprecated => emmeans # TODO
moy_var1 <- emmeans(res.lmer1, ~variete)
moy_var1

confint(contrast(moy_var1, method="tukey", adjust="none"))
#################################################################################
#### calculation with mean data per experiment
DFmoy <- aggregate(DF$rdt, list(variete=DF$variete,experimentation=DF$experimentation), mean)

#### model with experiment as random effect - mean data
## fitting the model
res.lmer3 <- lmer(x ~ variete + (1|experimentation), data=DFmoy, na.action=na.exclude)
res.lmer3

## component of the variance
print(VarCorr(res.lmer3), comp="Variance")

## means per variety and comparison between means
#moy_var3 <- lsmeans(res.lmer3, ~variete)   # deprecated => emmeans # TODO
moy_var3 <- emmeans(res.lmer3, ~variete)
moy_var3

confint(contrast(moy_var3, method="tukey", adjust="none"))


#### model with experiment as fixed effect - mean data
## fitting the model
res.lm4 <- lm(x ~ variete + experimentation, data=DFmoy, na.action=na.exclude)
res.lm4

## component of the variance
summary(res.lm4)$sigma^2

## means per variety and comparison between means
#moy_var4 <- lsmeans(res.lm4, ~variete)   # deprecated => emmeans # TODO
moy_var4 <- emmeans(res.lm4, ~variete)
moy_var4

confint(contrast(moy_var4, method="tukey", adjust="none"))


#################################################################################
#### validation of the model res.lmer3 (means data)
plot(fitted(res.lmer3),residuals(res.lmer3), abline(h=0))
hist(residuals(res.lmer3))

DFmoy$residus <- residuals(res.lmer3)
grubbs.test(DFmoy$residus)
DFmoy[which.max(DFmoy$residus),]

# hist(ranef(res.lmer3)$experimentation[,])


#### comparisons of means : model res.lmer3
# TODO : pbl chiffre differents du chapitre : to check
# anova table
options(contrasts=c("contr.sum","contr.poly"))
res.lmer3 <- lmer(x ~ variete + (1|experimentation), data=DFmoy, na.action=na.exclude)
Anova(res.lmer3, type="III", test.statistic="F")

# ajusted mean
#moy_var3 <- lsmeans(res.lmer3, ~variete)    # deprecated => emmeans # TODO
moy_var3 <- emmeans(res.lmer3, ~variete)

# comparisons to a control
# ? Dunnett Test  ?TODO : check
# to a single control : v2
contrast(moy_var3, method="trt.vs.ctrl", ref=2)
# to a average control : v2 and v9
contrast(moy_var3, method="trt.vs.ctrl", ref=c(2,9))

# confidence interval of the difference
# to a single control : v2
confint(contrast(moy_var3, method="trt.vs.ctrl", ref=2), adjust="none")

# comparisons 2 by 2
pairs(moy_var3, adjust="tukey")
cld(moy_var3, Letters=c(LETTERS))
confint(contrast(moy_var3, method="tukey", adjust="none"))

# to the general mean
confint(contrast(moy_var3, adjust = "tukey"))


# end of script
