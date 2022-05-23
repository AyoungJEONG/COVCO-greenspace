#######
# This is a script used to produce the main findings of Jeong et al SMW 2022
# Usage requires R knowledge and this script needs to be adapted to the local file paths and variable names


setwd('path_to_your_local_directory')


library(MASS)



covco <- readRDS('path_to_your_data_file')

# transform life satisfaction score into a factor
covco$satisf_now_fac <- as.factor(covco$satisf_now)
covco$satisf_past_fac <- as.factor(covco$satisf_past)



#######
# NDVI 

# base model
model1.polr <- satisf_now_fac ~ ndvi_buff300 +
  satisf_past_fac + agecat + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit1.polr <- polr(model1.polr, data = covco, method = 'probit', Hess = TRUE)
sum1.polr <- as.data.frame(summary(fit1.polr)$coefficients)
sum1.polr$p.value <- 2 * pnorm(-abs(sum1.polr[, 't value']))

write.csv(sum1.polr[,-3], 'summary_polr_ndvi.csv', quote = F)

# interaction with household income
model4.polr <- satisf_now_fac ~ ndvi_buff300 * hh_income +
  satisf_past_fac + agecat + sex + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit4.polr <- polr(model4.polr, data = covco, method = 'probit', Hess = TRUE)
sum4.polr <- as.data.frame(summary(fit4.polr)$coefficients)
sum4.polr$p.value <- 2 * pnorm(-abs(sum4.polr[, 't value']))

write.csv(sum4.polr[,-3], 'summary_polr_ndvi_hh_income.csv', quote = F)

# interaction with financial worries
model5.polr <- satisf_now_fac ~ ndvi_buff300 * finwor_3cat +
  satisf_past_fac + agecat + sex + hh_income + kanton + urbanity + mon_entry_2cat
fit5.polr <- polr(model5.polr, data = covco, method = 'probit', Hess = TRUE)
sum5.polr <- as.data.frame(summary(fit5.polr)$coefficients)
sum5.polr$p.value <- 2 * pnorm(-abs(sum5.polr[, 't value']))

write.csv(sum5.polr[,-3], 'summary_polr_ndvi_finwor.csv', quote = F)

# interaction with agecat
model8.polr <- satisf_now_fac ~ ndvi_buff300 * agecat +
  satisf_past_fac + sex + hh_income + finwor_3cat + kanton + urbanity + mon_entry_2cat
fit8.polr <- polr(model8.polr, data = covco, method = 'probit', Hess = TRUE)
sum8.polr <- as.data.frame(summary(fit8.polr)$coefficients)
sum8.polr$p.value <- 2 * pnorm(-abs(sum8.polr[, 't value']))

write.csv(sum8.polr[,-3], 'summary_polr_ndvi_agecat.csv', quote = F)


