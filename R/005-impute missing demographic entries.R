
if (!('data.table' %in% installed.packages()[, "Package"]))
  install.packages('data.table')
if (!('mice' %in% installed.packages()[, "Package"]))
  install.packages('mice')
library(data.table)
library(mice)


nda18 = readRDS("E:/ABCD study/R/Outputs/nda18_b1.Rds")
nda18$household.income.3level[nda18$household.income.3level == ""] = NA
nda18$household.income.4level[nda18$household.income.4level == ""] = NA
nda18$high.educ[nda18$high.educ == ""] = NA
nda18$married[nda18$married ==""]=NA
nda18$race.ethnicity.5level[nda18$race.ethnicity.5level==""]=NA
nda18$race.eth.8level[nda18$race.eth.8level==""]=NA


dat_nms = c(
  "src_subject_id",
  "eventname",
  "abcd_site",
  "age",
  "female",
  "race.ethnicity.5level",
  "race.eth.8level",
  "high.educ",
  "married",
  "household.income.3level",
  "household.income.4level"
)
if (sum(as.numeric(dat_nms %in% names(nda18))) != length(dat_nms))
  print("Error: missing core demographics. Add those first")
dat = data.table(nda18[, dat_nms])
for (m in dat_nms)
  print(paste("number missing for ", m, ": ", sum(is.na(nda18[[m]]) |
                                                    (nda18[m] == "")), sep = ""))


# Number of multiple imputed datasets & maximum number of iterations
n.imp = 5
n.iter = 5

var.ls <-
  c(
    "src_subject_id",
    "eventname",
    "age",
    "female",
    "race.ethnicity.5level",
    "race.eth.8level",
    "household.income.3level",
    "household.income.4level",
    "high.educ",
    "married"
  )
dat0 <- dat[, var.ls, with = FALSE]
dat0[, table(age, useNA = "if")]
dat0[, table(female, useNA = "if")]
dat0[, table(race.ethnicity.5level, useNA = "if")]
dat0[, table(race.eth.8level, useNA = "if")]
dat0[, table(household.income.3level, useNA = "if")]
dat0[, table(household.income.4level, useNA = "if")]
dat0[, table(high.educ, useNA = "if")]
dat0[, table(married, useNA = "if")]

ini <- mice(dat0, m = 1, maxit = 0)
meth = ini$meth



meth["race.ethnicity.5level"]   <- "polyreg"
meth["race.eth.8level"]   <- "polyreg"
meth["household.income.3level"]      <- "polyreg"
meth["household.income.4level"]      <- "polyreg"
meth["high.educ"]  <- "polyreg"
meth["married"] <- "logreg"


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
completedData <- complete(dat.imp,1)
names(completedData)

completedData<- completedData[,-c(2,3,4)]
# saveRDS(completedData, "completedData.Rds")
# completedData<- readRDS("completedData_step4.Rds")

# merge to the original data
vars = colnames(completedData)[-c(1)]
colnames(completedData)[-c(1)] = paste0(vars, ".imputed") #rename these variables to imputed variables

nda18_step5 = merge(nda18, completedData, by = c("src_subject_id"))

summary(nda18_step5[, names(completedData)])
summary(completedData)

saveRDS(nda18_step5, "E:/ABCD study/R/Outputs/nda18_step5.Rds")
