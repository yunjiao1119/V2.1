################################################
######### Merging variables of interest ########
################################################
require(data.table)
require(tidyverse)
nda18_b1 <- readRDS("E:/ABCD study/R/Outputs/nda18_step6.Rds")
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
    "married.imputed",
    
    "src_subject_id"                      ,
    "nihtbx_flanker_uncorrected.imputed" ,
    "nihtbx_list_uncorrected.imputed"     ,
    "nihtbx_cardsort_uncorrected.imputed",
    "nihtbx_pattern_uncorrected.imputed"  ,
    "nihtbx_picture_uncorrected.imputed" ,
    "nihtbx_reading_uncorrected.imputed"  ,
    "nihtbx_picvocab_uncorrected.imputed",
    "pea_ravlt_sd_trial_i_tc.imputed"     ,
    "pea_ravlt_sd_trial_ii_tc.imputed"   ,
    "pea_ravlt_sd_trial_iii_tc.imputed"  ,
    "pea_ravlt_sd_trial_iv_tc.imputed"  ,
    "pea_ravlt_sd_trial_v_tc.imputed"    ,
    "pea_ravlt_sd_trial_vi_tc.imputed"  ,
    "pea_ravlt_ld_trial_vii_tc.imputed"  ,
    "pea_wiscv_tss.imputed"             ,
    "lmt_scr_perc_correct.imputed"
  )
)
  

voi <- c(voi_daniel, demographics)



nda18_b1_voi <-
  nda18_b1[, which(names(nda18_b1) %in% voi)]


saveRDS(nda18_b1_voi,"E:/ABCD study/R/Outputs/nda18_b1_voi.Rds")
write.csv(nda18_b1_voi, "E:/ABCD study/R/Outputs/nda18_b1_voi.csv")
