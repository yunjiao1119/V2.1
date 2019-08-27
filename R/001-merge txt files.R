
rm(list = ls())
script.dir <- "E:/ABCD study/data/ABCD_V2.1/ABCD data"
setwd(script.dir)
input_list = Sys.glob(paths = c(paste(script.dir, "/*.txt", sep = "")))

if (length(which(grepl("package_info", input_list))) > 0)
  input_list = input_list[-which(grepl("package_info", input_list))]
if (length(which(grepl("fmriresults01", input_list))) > 0)
  input_list = input_list[-which(grepl("fmriresults01", input_list))]
if (length(which(grepl("genomics_sample03", input_list))) > 0)
  input_list = input_list[-which(grepl("genomics_sample03", input_list))]
if (length(which(grepl("aurora01", input_list))) > 0)
  input_list = input_list[-which(grepl("aurora01", input_list))]
if (length(which(grepl("omics_experiments", input_list))) > 0)
  input_list = input_list[-which(grepl("omics_experiments", input_list))]
if (length(which(grepl("errors", input_list))) > 0)
  input_list = input_list[-which(grepl("errors", input_list))]

instrument.name = NULL
for (p in 1:length(input_list)) {
  instrument.name[p] = gsub('*.txt$|.txt', '', basename(input_list[p]))
}

alia = read.csv('NDA_DEAP_names_2.0.csv')
tables = list()
for (p in 1:length(input_list)) {
  print(p)
  input = input_list[p]
  print(paste("import: ", input, " [", p, "/", length(input_list), "]", sep =
                ""))
  
  # read data from the tab-separated files as characters, don't use the usual comment character (can be in second row of item description)
  dt <- tryCatch({
    a = read.csv(
      file = input,
      sep = '\t',
      header = TRUE,
      row.names = NULL,
      comment.char = "",
      quote = "",
      check.names = FALSE
    )
    a = as.data.frame(sapply(a, function(x)
      gsub("\"", "", x)))
    names(a) = as.list(sapply(names(a), function(x)
      gsub("\"", "", x)))
    a
  }, error = function(e) {
    print(e)
    read.table(file = input,
               sep = '\t',
               header = TRUE)
  })
  
  # replace variable names from nda with their alias names to make them more like ABCD
  instrument = instrument.name[p]
  ali  = alia[which(alia$instrument == instrument),]
  nn = names(dt)
  for (q in 1:length(nn)) {
    if (nn[q] %in% ali$nda) {
      colnames(dt)[q] <- as.character(ali$abcd[ali$nda == nn[q]])
    }
  }
  tables[[p]] = dt
}

len.tables = length(tables)
for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[-1,]
  dt = droplevels(dt)
  tables[[p]] = dt
}


for (p in 1:len.tables) {
  dt = tables[[p]]
  if ("visit" %in% names(dt)) {
    print(p)
    print(instrument.name[p])
    dt$eventname = dt$visit
  }
  tables[[p]] = dt
}


for (p in 1:len.tables) {
  dt = tables[[p]]
  dt = dt[,!(
    names(dt) %in% c(
      "collection_id",
      "collection_title",
      "promoted_subjectkey",
      "subjectkey",
      "study_cohort_name"
    )
  )]
  tables[[p]] = dt
}
lt01.indx = which(instrument.name == "abcd_lt01")
#longitudinal tracking


rm.vars = c("visit", "interview_age", "interview_date", "gender")
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (instrument.name[p] == "abcd_midabwdp201") {
    #both "abcd_midabwdp201" and "abcd_midabwdp01" have the same variables (same values), delete one;
    dt = dt[,!(
      names(dt) %in% c(
        "tfmri_mid_all_antic.large.vs.small.reward_beta_cort.destrieux_g.front.inf.orbital.rh",
        rm.vars
      )
    )]
    
  } else if (instrument.name[p] == "abcd_dmdtifp201") {
    #both abcd_dmdtifp101 and abcd_dmdtifp201 have the same variable, delete one;
    dt = dt[,!(names(dt) %in% c("dmri_dtifull_visitid", rm.vars))]
  } else if (p != lt01.indx) {
    dt = dt[,!(names(dt) %in% rm.vars)]
  }
  
  tables[[p]] = dt
}

lt.bl = tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname == "baseline_year_1_arm_1"),]
dim(lt.bl)

lt.1yr = tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname == "1_year_follow_up_y_arm_1"),]
dim(lt.1yr)

lt.18m = tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname == "18_month_follow_up_arm_1"),]
dim(lt.18m)

lt.6m = tables[[lt01.indx]][which(tables[[lt01.indx]]$eventname == "6_month_follow_up_arm_1"),]
dim(lt.6m)

# total in 4 events
dim(lt.bl)[1] +  dim(lt.1yr)[1] +  dim(lt.18m)[1] +  dim(lt.6m)[1]

event.tot = c(dim(lt.bl)[1] ,  dim(lt.1yr)[1]  ,  dim(lt.18m)[1] ,  dim(lt.6m)[1])
diff.event = c(
  "baseline_year_1_arm_1",
  "1_year_follow_up_y_arm_1",
  "18_month_follow_up_arm_1",
  "6_month_follow_up_arm_1"
)
# check: if any table without eventname
for (p in 1:len.tables) {
  dt = tables[[p]]
  if (!("eventname" %in% names(dt))) {
    print(p)
    print(instrument.name[p])
  }
}


for (p in 1:length(tables)) {
  dt = tables[[p]]
  dt = droplevels(dt)
  tables[[p]] = dt
}


t2 = tables
rm(tables)
while (length(t2) > 1) {
  print("iteration")
  access = seq(1, length(t2) - 1, 2)
  for (i in access) {
    bm = dim(t2[[i]])
    
    by.vars = c("src_subject_id", "eventname")
    t2[[i]] = merge(t2[[i]], t2[[i + 1]], by = by.vars, all = TRUE)
    
    print(
      paste(
        "rows before: ",
        bm[1],
        dim(t2[[i + 1]])[1],
        " rows after: ",
        dim(t2[[i]])[1],
        "indices: ",
        i,
        i + 1,
        " columns: ",
        bm[2],
        "+",
        dim(t2[[i + 1]])[2],
        " = ",
        dim(t2[[i]])[2]
      )
    )
  }
  # for odd number of instruments add the last spreadsheet back to the list
  if (length(t2) %% 2 != 0)
    access = append(access, length(t2))
  # reduce the list
  t2 = t2[access]
}
nda18 = t2[[1]]
nda18 = nda18[,-which(grepl("dataset_id", colnames(nda18)))]


nda18$eventname = factor(nda18$eventname, levels(nda18$eventname)[c(2, 4, 1, 3)])


saveRDS(nda18, "E:/ABCD study/R/Outputs/nda18_step1.Rds")
names.nda18 = colnames(nda18)
save(file = "E:/ABCD study/R/Outputs/names.nda18.RData", names.nda18)
