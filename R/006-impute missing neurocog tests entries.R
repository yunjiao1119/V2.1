require(tidyverse)
library(data.table)
library(mice)


nda18 = readRDS("E:/ABCD study/R/Outputs/nda18_step5.Rds")


# transformNda18NA<- function(x){
#   nda18$x[nda18$x == ""] = NA
# }


nda18$nihtbx_flanker_uncorrected[nda18$nihtbx_flanker_uncorrected == ""] = NA
nda18$nihtbx_list_uncorrected[nda18$nihtbx_list_uncorrected == ""] = NA
nda18$nihtbx_cardsort_uncorrected[nda18$nihtbx_cardsort_uncorrected == ""] = NA
nda18$nihtbx_pattern_uncorrected[nda18$nihtbx_pattern_uncorrected == ""] = NA
nda18$nihtbx_picture_uncorrected[nda18$nihtbx_picture_uncorrected == ""] = NA
nda18$nihtbx_reading_uncorrected[nda18$nihtbx_reading_uncorrected == ""] = NA
nda18$nihtbx_picvocab_uncorrected[nda18$nihtbx_picvocab_uncorrected == ""] = NA
nda18$pea_ravlt_sd_trial_i_tc[nda18$pea_ravlt_sd_trial_i_tc == ""] = NA
nda18$pea_ravlt_sd_trial_ii_tc[nda18$pea_ravlt_sd_trial_ii_tc == ""] = NA
nda18$pea_ravlt_sd_trial_iii_tc[nda18$pea_ravlt_sd_trial_iii_tc == ""] = NA
nda18$pea_ravlt_sd_trial_iv_tc[nda18$pea_ravlt_sd_trial_iv_tc == ""] = NA
nda18$pea_ravlt_sd_trial_v_tc[nda18$pea_ravlt_sd_trial_v_tc == ""] = NA
nda18$pea_ravlt_sd_trial_vi_tc[nda18$pea_ravlt_sd_trial_vi_tc == ""] = NA
nda18$pea_ravlt_ld_trial_vii_tc[nda18$pea_ravlt_ld_trial_vii_tc == ""] = NA
nda18$pea_wiscv_tss[nda18$pea_wiscv_tss == ""] = NA
nda18$lmt_scr_perc_correct[nda18$lmt_scr_perc_correct == ""] = NA





dat_nms <- c(
  "src_subject_id",
  "eventname",
  "abcd_site",
  "nihtbx_flanker_uncorrected",
  "nihtbx_list_uncorrected",
  "nihtbx_cardsort_uncorrected",
  "nihtbx_pattern_uncorrected",
  "nihtbx_picture_uncorrected",
  "nihtbx_reading_uncorrected",
  "nihtbx_picvocab_uncorrected",
  
  "pea_ravlt_sd_trial_i_tc",
  "pea_ravlt_sd_trial_ii_tc",
  "pea_ravlt_sd_trial_iii_tc",
  "pea_ravlt_sd_trial_iv_tc",
  "pea_ravlt_sd_trial_v_tc",
  "pea_ravlt_sd_trial_vi_tc",
  "pea_ravlt_ld_trial_vii_tc",
  
  "pea_wiscv_tss",
  "lmt_scr_perc_correct"
  
)



if (sum(as.numeric(dat_nms %in% names(nda18))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(nda18[, dat_nms])
for (m in dat_nms)
  print(paste("number missing for ", m, ": ", sum(is.na(nda18[[m]]) |
                                                    (nda18[m] == "")), sep = ""))


# Number of multiple imputed datasets & maximum number of iterations
n.imp = 10
n.iter = 5

var.ls <-
  c(
    "src_subject_id",
    "eventname",
    
    "nihtbx_flanker_uncorrected",
    "nihtbx_list_uncorrected",
    "nihtbx_cardsort_uncorrected",
    "nihtbx_pattern_uncorrected",
    "nihtbx_picture_uncorrected",
    "nihtbx_reading_uncorrected",
    "nihtbx_picvocab_uncorrected",
    
    "pea_ravlt_sd_trial_i_tc",
    "pea_ravlt_sd_trial_ii_tc",
    "pea_ravlt_sd_trial_iii_tc",
    "pea_ravlt_sd_trial_iv_tc",
    "pea_ravlt_sd_trial_v_tc",
    "pea_ravlt_sd_trial_vi_tc",
    "pea_ravlt_ld_trial_vii_tc",
    
    "pea_wiscv_tss",
    "lmt_scr_perc_correct"
  )

dat0 <- dat[, var.ls, with = FALSE]

dat0[, table(nihtbx_flanker_uncorrected, useNA = "if")]
dat0[, table(nihtbx_list_uncorrected, useNA = "if")]
dat0[, table(nihtbx_cardsort_uncorrected, useNA = "if")]
dat0[, table(nihtbx_pattern_uncorrected, useNA = "if")]
dat0[, table(nihtbx_picture_uncorrected, useNA = "if")]
dat0[, table(nihtbx_reading_uncorrected, useNA = "if")]
dat0[, table(nihtbx_picvocab_uncorrected, useNA = "if")]

dat0[, table(pea_ravlt_sd_trial_i_tc, useNA = "if")]
dat0[, table(pea_ravlt_sd_trial_ii_tc, useNA = "if")]
dat0[, table(pea_ravlt_sd_trial_iii_tc, useNA = "if")]
dat0[, table(pea_ravlt_sd_trial_iv_tc, useNA = "if")]
dat0[, table(pea_ravlt_sd_trial_v_tc, useNA = "if")]
dat0[, table(pea_ravlt_sd_trial_vi_tc, useNA = "if")]
dat0[, table(pea_ravlt_ld_trial_vii_tc, useNA = "if")]

dat0[, table(pea_wiscv_tss, useNA = "if")]
dat0[, table(lmt_scr_perc_correct, useNA = "if")]



ini <- mice(dat0, m = 1, maxit = 0)
meth = ini$meth


# 
# 
# meth["race.ethnicity.5level"]   <- "polyreg"
# meth["race.eth.8level"]   <- "polyreg"
# meth["household.income.3level"]      <- "polyreg"
# meth["household.income.4level"]      <- "polyreg"
# meth["high.educ"]  <- "polyreg"
# meth["married"] <- "logreg"

for (v in neuro_imp){
  meth[v]="norm.predict"
}


pred = ini$pred

# Excluding variables from the imputation models
pred[, c("src_subject_id",    "eventname")] <- 0
pred

# Specifying parameters for the imputation
post <- mice(
  dat0,
  meth = meth,
  pred = pred,
  seed = 111,
  m = 1,
  maxit = 0
)$post

dat.imp <- mice(
  dat0,
  meth = meth,
  pred = pred,
  post = post,
  seed = 1111,
  m = n.imp,
  maxit = n.iter
)
rm(dat0)

# get one imputed dataset out
completedData <- complete(dat.imp, 1)

names(completedData)
completedData<- completedData[,-c(2)]

# merge to the original data
vars = colnames(completedData)[-c(1)]
colnames(completedData)[-c(1)] = paste0(vars, ".imputed") #rename these variables to imputed variables
nda18_step6 = merge(nda18, completedData, by = c("src_subject_id"))

saveRDS(completedData, "E:/ABCD study/R/Outputs/completedData_step6.Rds")
saveRDS(nda18_step6 , "E:/ABCD study/R/Outputs/nda18_step6.Rds")


summary(nda18_step6[, vars])
summary(nda18_step6[, names(completedData)])
summary(completedData)


