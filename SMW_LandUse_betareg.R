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
# green space area proportion variables
# note: examine the interaction for the variables that showed significant modification of NDVI effect
#       i.e. hh_income, finwor_3cat, and agecat, and plot them to make a comparison to NDVI

# base model
model1 <- satisf_now_prop ~ forest_buff300 + park_buff300 + agri_buff300 +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit1 <- betareg(model1, data = covco)
summary(fit1)

write.csv(summary(fit1)$coef$mean[,-3], 'summary_greenspace_areaprop.csv', quote = F)

# quadratic term
model2 <- satisf_now_prop ~ forest_buff300 + I(forest_buff300^2) + park_buff300 + I(park_buff300^2) + agri_buff300 + + I(agri_buff300^2) +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit2 <- betareg(model2, data = covco)
summary(fit2)
# -> no evidence of quadratic relationship for any area proportion variables

# interaction with household income
model4 <- satisf_now_prop ~ (forest_buff300 + park_buff300 + agri_buff300) * hh_income +
  satisf_past_prop + agecat + sex + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit4 <- betareg(model4, data = covco)
summary(fit4)
lrtest(fit1, fit4) # p=0.432

p4.forest <- plot(predictorEffect('forest_buff300', fit4),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                  axes=list(x=list(forest_buff300=list(lab='Forest 300m buffer')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Household income [CHF]')),
                  main = '', rug=FALSE)
p4.park <- plot(predictorEffect('park_buff300', fit4),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(park_buff300=list(lab='Park 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Household income [CHF]')),
                main = '', rug=FALSE)
p4.agri <- plot(predictorEffect('agri_buff300', fit4),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(agri_buff300=list(lab='Agricultural area 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Household income [CHF]')),
                main = '', rug=FALSE)

jpeg('greenspace_hh_income.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p4.forest, p4.park, p4.agri, nrow=1)
dev.off()

write.csv(summary(fit4)$coef$mean[,-3], 'summary_greenspace_areaprop_hh_income.csv', quote = F)

# interaction with financial worries
model5 <- satisf_now_prop ~ (forest_buff300 + park_buff300 + agri_buff300) * finwor_3cat +
  satisf_past_prop + agecat + sex + hh_income + kanton + urbanity + mon_entry_2cat
fit5 <- betareg(model5, data = covco)
summary(fit5)
lrtest(fit1, fit5) # p=0.3828

p5.forest <- plot(predictorEffect('forest_buff300', fit5),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                  axes=list(x=list(forest_buff300=list(lab='Forest 300m buffer')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Financial worries')),
                  main = '', rug=FALSE)
p5.park <- plot(predictorEffect('park_buff300', fit5),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(park_buff300=list(lab='Park 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Financial worries')),
                main = '', rug=FALSE)
p5.agri <- plot(predictorEffect('agri_buff300', fit5),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(agri_buff300=list(lab='Agricultural area 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Financial worries')),
                main = '', rug=FALSE)

jpeg('greenspace_finwor.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p5.forest, p5.park, p5.agri, nrow=1)
dev.off()

write.csv(summary(fit5)$coef$mean[,-3], 'summary_greenspace_areaprop_finwor.csv', quote = F)

# interaction with agecat
model8 <- satisf_now_prop ~ (forest_buff300 + park_buff300 + agri_buff300) * agecat +
  satisf_past_prop + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit8 <- betareg(model8, data = covco)
summary(fit8)
lrtest(fit1, fit8) # p=0.6216

p8.forest <- plot(predictorEffect('forest_buff300', fit8),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                  axes=list(x=list(forest_buff300=list(lab='Forest 300m buffer')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Age categories')),
                  main = '', rug=FALSE)
p8.park <- plot(predictorEffect('park_buff300', fit8),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(park_buff300=list(lab='Park 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Age categories')),
                main = '', rug=FALSE)
p8.agri <- plot(predictorEffect('agri_buff300', fit8),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(agri_buff300=list(lab='Agricultural area 300m buffer')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Age categories')),
                main = '', rug=FALSE)

jpeg('greenspace_agecat.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p8.forest, p8.park, p8.agri, nrow=1)
dev.off()

write.csv(summary(fit8)$coef$mean[,-3], 'summary_greenspace_areaprop_agecat.csv', quote = F)

# correlation with NDVI
cor(d$ndvi_buff300, d$forest_buff300 + d$park_buff300 + d$agri_buff300)
with(d[d$kanton=='BS',], cor(ndvi_buff300, forest_buff300 + park_buff300 + agri_buff300))
with(d[d$kanton=='BL',], cor(ndvi_buff300, forest_buff300 + park_buff300 + agri_buff300))


