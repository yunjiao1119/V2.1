
nda18 = readRDS("E:/ABCD study/R/Outputs/nda18_step2.Rds")

categories = read.csv('E:/ABCD study/R/choices_coding_nda18.csv')

for (kitty in categories$name) {
  if (!(kitty %in% names(nda18)))
    next
  choices = strsplit(as.character(categories[categories$name == kitty, ]$choices), "|", fixed =
                       TRUE)
  lev = levels(nda18[[kitty]])
  orig_levels = lev
  for (c in 1:length(choices[[1]])) {
    choice = choices[[1]][c]
    number = trimws(strsplit(choice, ",")[[1]][1])
    labels = strsplit(choice, ",")[[1]][-1]
    # I am not able to simply paste the result from strsplit, use a loop instead
    label = labels[[1]]
    if (length(labels) > 1)
      for (i in 2:length(labels))
        label = paste(label, labels[[i]], sep = ",")
    label = trimws(label)
    lev[which(lev == number)] = label
  }
  nda18[[kitty]] = factor(nda18[[kitty]], levels = orig_levels, labels =
                            lev)
  nda18[[kitty]][nda18[[kitty]] == ""] = NA
}


ncols = ncol(nda18)
colnames = names(nda18)
data_clean = nda18
data_clean = droplevels(data_clean)
typevec = NA
nlevvec = rep(NA, length(typevec))
is.wholenumber <-
  function(x, tol = .Machine$double.eps ^ 0.5)
    abs(x - round(x)) < tol | is.na(x)
for (coli in 3:ncols) {
  levvec = levels(as.factor(as.character(nda18[, coli])))
  nlev = length(levvec)
  levvec.numeric = suppressWarnings(as.numeric(levvec))
  nnum = sum((!is.na(levvec.numeric)) | (levvec == "") |
               (levvec == "NA"))
  nempty = sum(levvec == "" | (levvec == "NA"))
  nlevvec[coli] = nlev
  if (names(nda18)[coli] %in% categories$name) {
    typevec[coli] = 'Categorical'
  } else if (nnum == nlev) {
    # All numeric
    data_clean[, coli] = as.numeric(as.character(nda18[, coli]))
    nint = sum(is.wholenumber(levvec.numeric))
    if (nint == nlev) {
      typevec[coli] = 'Integer'
    } else {
      typevec[coli] = 'Real'
    }
  } else if ((nnum - nempty) == 0) {
    # No numeric, other than empty string
    if (nlev == 2) {
      typevec[coli] = 'Binary'
    } else {
      typevec[coli] = 'Categorical'
    }
  } else {
    typevec[coli] = 'Ambiguous' # Inspect more closely
  }
  cat(sprintf(
    '%5d: type=%s nlev=%d (%s)\n',
    coli,
    typevec[coli],
    nlevvec[coli],
    colnames[coli]
  ))
}
nda18 = data_clean
# Ambiguius columns
#colnames[typevec=='Ambiguous']

# Empty columns
#colnames[(typevec=='Integer')&(nlevvec==0)]

# All-zero columns
#colnames[(typevec=='Integer')&(nlevvec==1)]
#which((typevec=='Integer')&(nlevvec==1))


saveRDS(nda18, "E:/ABCD study/R/Outputs/nda18_step3.Rds")