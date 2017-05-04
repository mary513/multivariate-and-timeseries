library(car)
library(lmtest)
library(sandwich)

rm(list=ls())
tmpenv <- new.env()
load("C:/weight.RData", envir=tmpenv)
prelimdata<- tmpenv$data

#Count baseline observations
nrow(prelimdata)


#Check data for non-viable values
sum(is.na(prelimdata$fatheduc))
sum(is.na(prelimdata$motheduc))
sum(prelimdata$faminc<0)
sum(prelimdata$parity==0)
sum(prelimdata$bwght==0)
sum(is.na(prelimdata$male))
#Remove non-viable data for multiple regression
alldata<-subset(prelimdata, prelimdata$bwght != 0 & !is.na(fatheduc) & !is.na(motheduc))
nrow(alldata)


model1<- lm(data = alldata, alldata$bwght ~ alldata$cigs + alldata$parity + alldata$faminc + alldata$motheduc + alldata$fatheduc + alldata$male)
summary(model1)
#plot(model1)
summary(alldata$cigs)
hist(alldata$cigs, breaks=10)
summary(alldata$parity)
hist(alldata$parity, breaks=10)
summary(alldata$faminc)
hist(alldata$faminc, breaks=10)
summary(alldata$motheduc)
hist(alldata$motheduc, breaks=10)
summary(alldata$fatheduc)
hist(alldata$fatheduc, breaks=10)

# Explain the coefficients of cigs, faminc, motheduc, and male
# After scrubbing the data and removing non-viable values as noted above:

# cigs: holding all other variables constant, for each additional cigarette, the
# average change in a baby's birthweight is -0.585 ounces. 
# This is statistically significant at more than 1/10 of 1%,
# which means that the probability of selecting, at random, a birthweight 
# this extreme is less than 1/10 of 1%, very rare.
# 
# faminc: holding all other variables constant, each unit increment in faminc
# (gauging from the EDA, a 'unit' is likely $1000), average change in the birthweight
# is +0.063 ounces. Faminc is statistically significant at 10% 

# motheeduc: holding all other variables constant, 
# for each additional year of education that the mother
# has achieved, the average change in the baby's birthweight is -0.389 ounces. 
# Mothereduc is not statistically significant

# male: holding all other variables constant, if the baby is a male, 
# on average it weighs +3.761 ounces more
# than a female baby. Male variable is statistically significant at 1%.  


#Test the hypothesis that the average daily number of cigarettes the mother smoked 
# during pregancy has no effect on birth weight. Interpret the results. 
# H0: B(cigs) = 0
# H1: B(cigs) != 0
# T value (coef / std error) provided in the lm Summary, is -5.304
# For n-k-1 degrees of freedom (>120) we use the z distribution; -5.304 is statistically significant
# at more than 1% probability density. In other words, based on the model provided,
# the probability density of selecting a birthweight at random, that is as least as great as
# the mean birthweight for mothers who smoked,
# holding all other factors constant, is less than 1%. 
# We can reject the null hypothesis that cigarettes have no effect on birthweight

# 4)
# Test the hypothesis that parents (i.e. both mother and father) education has no effect on birth weight.

#create variable as combined education of mother + father
alldata["mothfathedu"]<-alldata$motheduc + alldata$fatheduc
#replace prior variables for motheduc, fatheduc with the new, combined variable
model2<- lm(alldata$bwght ~ alldata$cigs + alldata$parity + alldata$faminc + alldata$mothfathedu + alldata$male)
summary(model2)
# Results show the t-statistic of the slope coefficient of 0.07417 is not statistically signficant.
# So we can NOT reject the null hypothesis that parents' (both mother and father)
# education has no effect on birth weight
# Note: measured separately the education of each parent is more influential.
# Since they are in opposite directions, (motheduc cause decrease and fatheduc cause increase)
# when combined the effect is even less significant as they mostly cancel each other out. 

# Current suspicion is that the model can be restricted, at least remove mother/father educ, 
# possibly faminc and conduct joint hypothesis testing. 

# 5)
# Test the hypothesis that an increase in family income by $10,000 has the
# same effect as an increase in father's education by 1 year.
# First create 2 new variables for comparison: faminc  + 10 (since income is in $k) and fatheduc + 1

alldata["faminc_10"]<-alldata$faminc + 10
alldata["fatheduc_1"]<-alldata$fatheduc + 1
faminc_10<-alldata$faminc_10
fatheduc_1<-alldata$fatheduc_1

model4<- lm(alldata$bwght ~ alldata$cigs + alldata$parity + faminc_10 + fatheduc_1 + alldata$motheduc + alldata$male)
summary(model4)
linearHypothesis(model4, "faminc_10 = fatheduc_1", vcov = vcovHC)

#the p-value is 0.1424 (14.2% probability density) which is not statistically significant at even 10%

# Test another way:
# create combined variable and use combined variable and original value for faminc_10. 
# Faminc_10 variable will measure the difference between the effects of faminc+10k and fathereduc+1 year.
alldata$faminc_10_fatheduc_1<-faminc_10 + fatheduc_1
faminc_10_fatheduc_1<-alldata$faminc_10_fatheduc_1

model4a<- lm(alldata$bwght ~ alldata$cigs + alldata$parity + faminc_10 + faminc_10_fatheduc_1 + alldata$motheduc + alldata$male)
coeftest(model4a, vcov = vcovHC)
#We see the p-value is exactly the same as using the linearHypothesis function.  


# Test the overall significance of this regression.
# Use F statistic
# F  = [(SSRr 2 SSRur )/q  ]  /  [SSRur / (n - k - 1) ]
# From regression, F-statistic: 11.19 on 5 and 1175 DF,
# p-value,1.474e-10, is highly significant (much less than 1%)
# H0: B(cigs)=0, B(parity)=0, B(faminc)=0, B(motheduc)=0, B(fatheduc)=0, B(male)=0,
# We can reject H0
# Suspicion that some of the coefficients can be removed without statistically impacting the model

# The regression as initially stated is statistically significant, but that does not mean it 
# might still be significant with fewer predictor variables. 
# Joint hypothesis test
# Remove motheduc, fatheduc, faminc

model_rest = lm(data = alldata, alldata$bwght ~ alldata$cigs + alldata$parity + alldata$male)
waldtest(model1, model_rest, vcov = vcovHC)
# motheduc, fatheduc, faminc are jointly significant

# now try by removing motheduc, fatheduc
model_rest2 = lm(data = alldata, alldata$bwght ~ alldata$cigs + alldata$parity + alldata$male + alldata$faminc)
summary(model_rest2)
waldtest(model1, model_rest2, vcov = vcovHC)

#Not significant, so motheduc, fatheduc can be removed from the model

# Conclusion: "model_rest2" is the best fit OLS model. 

