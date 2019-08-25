################################################
######### Merging variables of interest ########
################################################
require(data.table)
require(tidyverse)
nda18_b1 <- readRDS("E:/ABCD study/R/Outputs/nda18_b1.Rds")
variables_of_interest <-
  read.csv(file = "E:/ABCD study/R/Variables of Interest_V2.1.csv")
voi_daniel <-
  as.character(variables_of_interest$Variables_of_Interest)

demographics <- as.character(
  c(
    "age",
    "sex",
    "female",
    "race.ethnicity.5level",
    "race.eth.8level",
    "married.or.livingtogether",
    "married",
    "high.educ",
    "household.income.3level",
    "household.income.4level",
    "race.ethnicity.5level.imputed",
    "race.eth.8level.imputed",
    "household.income.3level.imputed",
    "household.income.4level.imputed",
    "high.educ.imputed" ,
    "married.imputed"
  )
)


voi <- c(voi_daniel, demographics)



nda18_b1_voi <-
  nda18_b1[, which(names(nda18_b1) %in% voi)]

write.csv(nda18_b1_voi, "E:/ABCD study/R/Outputs/nda18_b1_voi.csv")
