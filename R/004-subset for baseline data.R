require(haven)
require(tidyverse)

nda18 = readRDS("E:/ABCD study/R/Outputs/nda18_step3.Rds")
# completedData <- readRDS("E:/ABCD study/R/Outputs/completedData_step4.Rds")
summary(nda18$eventname)

nda18_b1 <- nda18[which (nda18$eventname == "baseline_year_1_arm_1"), ]
# completedData_b1 <-completedData[which(completedData$eventname == "baseline_year_1_arm_1"),]
# 
# 
# 
# nda18_step5 = merge(nda18_b1, completedData_b1, by = c("src_subject_id"))
# 
# # using the "polyreg" method to impute missing household income, the resulsts could be different for household.income.3level.imputed and household.income.4level.imputed
# test<- nda18_step5 %>% select(contains('household.income'))
# head(test[is.na(test$household.income.3level),],20)

saveRDS(nda18_b1, "E:/ABCD study/R/Outputs/nda18_b1.Rds")




