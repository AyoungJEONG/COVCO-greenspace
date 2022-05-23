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
# route to green space
# note: examine the interaction for the variables that showed significant modification of NDVI effect
#       i.e. hh_income, finwor_3cat, and agecat, and plot them to make a comparison to NDVI

# base model
model1 <- satisf_now_prop ~ forest_route + park_route + agri_route + ndvi_buff300 +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit1 <- betareg(model1, data = covco)
summary(fit1)

write.csv(summary(fit1)$coef$mean[,-3], 'summary_greenspace_route.csv', quote = F)

# quadratic term
model2 <- satisf_now_prop ~ forest_route + I(forest_route^2) + park_route + I(park_route^2) + agri_route + I(agri_route^2) + ndvi_buff300 +
  satisf_past_prop + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit2 <- betareg(model2, data = covco)
summary(fit2)
# -> no evidence of quadratic relationship for any route variables

# interaction with household income
model4 <- satisf_now_prop ~ (forest_route + park_route + agri_route) * hh_income + ndvi_buff300 +
  satisf_past_prop + agecat + sex + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit4 <- betareg(model4, data = covco)
summary(fit4)
lrtest(fit1, fit4) # p=0.002354

p4.forest <- plot(predictorEffect('forest_route', fit4),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                  axes=list(x=list(forest_route=list(lab='Route to forest [km]')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Household income [CHF]')),
                  main = '', rug=FALSE)
p4.park <- plot(predictorEffect('park_route', fit4),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(park_route=list(lab='Route to park [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Household income [CHF]')),
                main = '', rug=FALSE)
p4.agri <- plot(predictorEffect('agri_route', fit4),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.1),
                axes=list(x=list(agri_route=list(lab='Route to agricultural area [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Household income [CHF]')),
                main = '', rug=FALSE)

jpeg('route_hh_income.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p4.forest, p4.park, p4.agri, nrow=1)
dev.off()

write.csv(summary(fit4)$coef$mean[,-3], 'summary_greenspace_route_hh_income.csv', quote = F)

# interaction with financial worries
model5 <- satisf_now_prop ~ (forest_route + park_route + agri_route) * finwor_3cat + ndvi_buff300 +
  satisf_past_prop + agecat + sex + hh_income + kanton + urbanity + mon_entry_2cat
fit5 <- betareg(model5, data = covco)
summary(fit5)
lrtest(fit1, fit5) # p=0.295

p5.forest <- plot(predictorEffect('forest_route', fit5),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                  axes=list(x=list(forest_route=list(lab='Route to forest [km]')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Financial worries')),
                  main = '', rug=FALSE)
p5.park <- plot(predictorEffect('park_route', fit5),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(park_route=list(lab='Route to park [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Financial worries')),
                main = '', rug=FALSE)
p5.agri <- plot(predictorEffect('agri_route', fit5),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(agri_route=list(lab='Route to agricultural area [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Financial worries')),
                main = '', rug=FALSE)

jpeg('route_finwor.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p5.forest, p5.park, p5.agri, nrow=1)
dev.off()

write.csv(summary(fit5)$coef$mean[,-3], 'summary_greenspace_route_finwor.csv', quote = F)

# interaction with agecat
model8 <- satisf_now_prop ~ (forest_route + park_route + agri_route) * agecat + ndvi_buff300 +
  satisf_past_prop + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit8 <- betareg(model8, data = covco)
summary(fit8)
lrtest(fit1, fit8) # p=0.006195

p8.forest <- plot(predictorEffect('forest_route', fit8),
                  lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                  axes=list(x=list(forest_route=list(lab='Route to forest [km]')),
                            y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                  lattice=list(key.args=list(title='Age categories')),
                  main = '', rug=FALSE)
p8.park <- plot(predictorEffect('park_route', fit8),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(park_route=list(lab='Route to park [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Age categories')),
                main = '', rug=FALSE)
p8.agri <- plot(predictorEffect('agri_route', fit8),
                lines=list(multiline=TRUE), confint=list(style='auto', alpha=0.05),
                axes=list(x=list(agri_route=list(lab='Route to agricultural area [km]')),
                          y=list(transform=function(x) (exp(x)/(1+exp(x)))*10.1-0.01, lab='Life satisfaction')),
                lattice=list(key.args=list(title='Age categories')),
                main = '', rug=FALSE)

jpeg('route_agecat.jpg', res = 300, height = 1500, width = 4500)
grid.arrange(p8.forest, p8.park, p8.agri, nrow=1)
dev.off()

write.csv(summary(fit8)$coef$mean[,-3], 'summary_greenspace_route_agecat.csv', quote = F)


