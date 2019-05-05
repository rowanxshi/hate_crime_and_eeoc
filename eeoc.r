library(data.table)
library(magrittr)
library(stringr)

# prep
## load
y2010 = fread("2010.txt", showProgress = T)
y2010_vars = c("charge_num", "state", "num_employees_code", "num_employees", "naics_code", "naics", "type_code", "type", "birth_date", "sex", "date_received", "date_fepa_sent_to_eeoc", "date_closed", "closure_code", "closure", "monetary_benefits", "statute_code", "statute", "basis_code", "basis", "issue_code", "issue", "date_court_filing", "civil_action_num", "court", "date_resolution", "litigation_monetary_benefits", "case_type")
names(y2010) = y2010_vars
rest = fread("2011_2017.txt")
vars = c("year", "charge_num", "state", "num_employees_code", "num_employees", "naics_code", "naics", "type_code", "type", "birth_date", "sex", "date_received", "date_closed", "closure_code", "closure", "monetary_benefits", "statute_code", "statute", "basis_code", "basis", "issue_code", "issue", "date_court_filing", "civil_action_num", "court", "date_resolution", "case_type", "litigation_monetary_benefits")
names(rest) = vars
## append
y2010_vars[!(y2010_vars %in% vars)]
vars[!(vars %in% y2010_vars)]
y2010[, c("date_fepa_sent_to_eeoc", "year") := .(NULL, "FY2010")]
(all(names(y2010) %in% names(rest)) & all(names(rest) %in% names(y2010))) %>% assertthat::assert_that()
full = rbind(rest, y2010[, ..vars])
## format
# full[, lapply(.SD, factor)] %>% summary
### factors with codes
gen_codebook = function(code_var, label_var) {
  var_pair = c(code_var, label_var)
  codebook = full[, ..var_pair] %>% unique
  codebook = codebook[get(code_var) != "null" & get(code_var) != ""]
  codebook[get(label_var) == "", (label_var) := "Other"]
}
gen_factor = function(i) {
  full[, c(code_vars[[i]], label_vars[[i]]) := .(factor(get(code_vars[[i]]), levels = codebooks[[i]][[1]], labels = codebooks[[i]][[2]]), NULL)]
}
code_vars = grep("code", vars, value = T)
label_vars = str_replace(code_vars, "_code", "")
codebooks = mapply(gen_codebook, code_vars, label_vars, SIMPLIFY = F)
lapply(which(!grepl("naics", code_vars)), gen_factor) # leave naics as is
### other factors
replace_empty_with_na = function(var_name) {
  full[get(var_name) == "", (var_name) := NA]
  full[get(var_name) == "null", (var_name) := NA]
}
other_factor_vars = c("state", "sex", "court", "case_type")
lapply(other_factor_vars, replace_empty_with_na)
full[, c(other_factor_vars) := lapply(.SD, factor), .SDcols = other_factor_vars]
### dates
conv_dates = function(date_var) {
  full[grepl("/[0-9]{4}$", get(date_var)), paste0(date_var, "_processed") := as.Date(get(date_var), format = "%m/%d/%Y")]
  full[grepl("/[0-9]{2}$", get(date_var)), paste0(date_var, "_processed") := as.Date(get(date_var), format = "%m/%d/%y")]
  full[, paste0(date_var, c("", "_processed")) := .(get(paste0(date_var, "_processed")), NULL)]
}
full = full[!grepl("/0[0-9]$", birth_date) & !grepl("/1[0-7]$", birth_date)] # drop ambiguous '00 - '17 birthdays
full[grepl("/[0-9]{2}$", birth_date), birth_date := sapply(birth_date, str_replace, "/([0-9]{2}$)", "/19\\1", USE.NAMES = F)] # add in 1900 to 2-digit years
date_vars = grep("date", vars, value = T)
lapply(date_vars, conv_dates)
full = full[year(birth_date) > 1917 & year(birth_date) < 2010]
### numbers
full[, c("year") := .(as.integer(str_extract(year, "[0-9]+")))]
number_vars = c("charge_num", "monetary_benefits", "litigation_monetary_benefits")
lapply(number_vars, replace_empty_with_na)
full[, (number_vars) := lapply(.SD, function(x) as.numeric(str_replace_all(x, ",", ""))), .SDcol = number_vars]
### drop entries that aren't cases
full = full[court != "COURT" | is.na(court)]
## save
full[, naics := NULL]
lapply(c("naics_code", "civil_action_num"), replace_empty_with_na)
save(full, codebooks, file = "eeoc.RData")






















#
