# STAT486 PROJECT 4
# HEBA SYED

setwd("C:/Users/heb/Documents/STAT")

crops<-read.csv("CropData.csv")
attach(crops)

str(crops)

tapply(yield, density, sum)
#        1        2 
# 8485.656 8507.830 
tapply(yield, fertilizer, sum)
# 1        2        3 
# 5656.225 5661.863 5675.397 
tapply(yield, block, sum)
# 1        2        3        4 
# 4244.553 4255.605 4241.103 4252.225 

summary(yield)

par(mfrow=c(2,2))
barplot(tapply(yield, density, sum), main = "sum of yield per density", density = c(30,7), angle = c(11,36), col = "pink")
barplot(tapply(yield, fertilizer, sum), main = "sum of yield per fertilizer type", density = c(10,20, 10), angle = c(45,90,0), col = "purple")
barplot(tapply(yield, block, sum), main = "sum of yield per block", density=c(5,10,20,30,7) , angle=c(0,45,90,11,36), col = "lightblue")
par(mfrow=c(1,1))

crops$density<-ordered(as.factor(density))
crops$block<-as.factor(block)
crops$fertilizer<-as.factor(fertilizer)

modelA<-lm(yield~fertilizer+density+fertilizer*density, data = crops)
summary(modelA)
anova(modelA) # mse1: 0.3371 
# interaction effect is not significant (0.5325)

modelB<-lm(yield~fertilizer+density, data = crops)
summary(modelB)


modelC<-lm(yield~fertilizer+density+fertilizer*density+block, data = crops)

anova(modelA, modelC, test="F")
# Model 1: yield ~ fertilizer + density + fertilizer * density
# Model 2: yield ~ fertilizer + density + fertilizer * density + block
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     90 30.337                           
# 2     88 29.851  2   0.48614 0.7166 0.4913
# blocking effect not significant

AIC(modelA) # 175.8451
AIC(modelB) # 173.1895 - lowest AIC value
AIC(modelC) # 178.2943


summary(modelB)
anova(modelB)
# both are significant, 

# assumption check
qqnorm(modelB$residuals)
qqline(modelB$residuals)

par(mfrow=c(2,2))
plot(modelB)

library(car)
leveneTest(yield~fertilizer, data = crops)
leveneTest(yield~density, data = crops)
leveneTest(yield~block, data = crops)
# equal var

tk<-TukeyHSD(aov(modelB), ordered = TRUE, conf.level = 0.95)
tk
par(mfrow=c(1,2))
plot(tk, las=1, col= "green")
TukeyHSD(aov(modelA), ordered = TRUE)
