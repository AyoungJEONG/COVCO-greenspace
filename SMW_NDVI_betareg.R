#######
# This is a script used to produce the main findings of Jeong et al SMW 2022
# Usage requires R knowledge and this script needs to be adapted to the local file paths and variable names


setwd('path_to_your_local_directory')


library(betareg)
library(lmtest)
library(effects)
library(gridExtra)



covco <- readRDS('path_to_your_data_file')

# transform life satisfaction score into a proportion
covco$satisf_now_prop <- (covco$satisf_now+0.01)/10.1
covco$satisf_past_prop <- (covco$satisf_past+0.01)/10.1



#######
# NDVI 
# note: plot the significant effects of NDVI, using predictorEffect {effects}

# base model
model1 <- satisf_now_prop ~ ndvi_buff300 +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit1 <- betareg(model1, data = covco)
summary(fit1)

write.csv(summary(fit1)$coef$mean[,-3], 'summary_ndvi.csv', quote = F)

# quadratic term
model2 <- satisf_now_prop ~ ndvi_buff300 + I(ndvi_buff300^2) +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit2 <- betareg(model2, data = covco)
summary(fit2)
# -> no evidence of quadratic relationship for NDVI

# interaction with dichotomized month of entry
model3 <- satisf_now_prop ~ ndvi_buff300 * mon_entry_2cat +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity
fit3 <- betareg(model3, data = covco)
summary(fit3)
lrtest(fit1, fit3)

# interaction with household income
model4 <- satisf_now_prop ~ ndvi_buff300 * hh_income +
  satisf_past_prop + agecat + sex + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit4 <- betareg(model4, data = covco)
summary(fit4)
lrtest(fit1, fit4) # p=0.00487

jpeg('ndvi_hh_income.jpg', res = 300, height = 2000, width = 2000)
plot(predictorEffect('ndvi_buff300', fit4),
     lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
     axes=list(x=list(ndvi_buff300=list(lab='NDVI 300m buffer')),
               y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
     lattice=list(key.args=list(title='Household income [CHF]')),
     main = '', rug=FALSE, ylim=c(6.7,8.2))
dev.off()

write.csv(summary(fit4)$coef$mean[,-3], 'summary_ndvi_hh_income.csv', quote = F)

# interaction with financial worries
model5 <- satisf_now_prop ~ ndvi_buff300 * finwor_3cat +
  satisf_past_prop + agecat + sex + hh_income + kanton + urbanity + mon_entry_2cat
fit5 <- betareg(model5, data = covco)
summary(fit5)
lrtest(fit1, fit5) # p=0.0137

jpeg('ndvi_finwor.jpg', res = 300, height = 2000, width = 2000)
plot(predictorEffect('ndvi_buff300', fit5),
     lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
     axes=list(x=list(ndvi_buff300=list(lab='NDVI 300m buffer')),
               y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
     lattice=list(key.args=list(title='Financial worries')),
     main = '', rug=FALSE, ylim=c(6.7,8.2))
dev.off()

write.csv(summary(fit5)$coef$mean[,-3], 'summary_ndvi_finwor.csv', quote = F)

# interaction with canton
model6 <- satisf_now_prop ~ ndvi_buff300 * kanton +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + urbanity + mon_entry_2cat
fit6 <- betareg(model6, data = covco)
summary(fit6)
lrtest(fit1, fit6)

# interaction with urbanity
model7 <- satisf_now_prop ~ ndvi_buff300 * urbanity +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + mon_entry_2cat
fit7 <- betareg(model7, data = covco)
summary(fit7)
lrtest(fit1, fit7)

# interaction with agecat
model8 <- satisf_now_prop ~ ndvi_buff300 * agecat +
  satisf_past_prop + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit8 <- betareg(model8, data = covco)
summary(fit8)
lrtest(fit1, fit8) # p=0.01582

jpeg('ndvi_agecat.jpg', res = 300, height = 2000, width = 2000)
plot(predictorEffect('ndvi_buff300', fit8),
     lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
     axes=list(x=list(ndvi_buff300=list(lab='NDVI 300m buffer')),
               y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
     lattice=list(key.args=list(title='Age categories')),
     main = '', rug=FALSE, ylim=c(6.7,8.2))
dev.off()

write.csv(summary(fit8)$coef$mean[,-3], 'summary_ndvi_agecat.csv', quote = F)

# interaction with sex
model9 <- satisf_now_prop ~ ndvi_buff300 * sex +
  satisf_past_prop + agecat + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit9 <- betareg(model9, data = covco)
summary(fit9)
lrtest(fit1, fit9)


