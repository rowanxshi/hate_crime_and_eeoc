library(ggplot2)

##############
####  DC  ####
##############

dc_hate = fread("dc_hate.csv")
names(dc_hate) = c("offense_date", "offense_time", "report_date", "report_year", "report_month", "ccn", "district", "block", "bias", "targeted_group", "offense")

# prep
## dates
dc_hate[, offense_date := as.Date(offense_date, format = "%m/%d/%Y")]
dc_hate[, report_date := as.Date(report_date, format = "%m/%d/%y")]
dc_hate[, c("report_year", "report_month") := NULL]
## string/factors
replace_empty_with_na = function(var_name) {
  dc_hate[get(var_name) == "", (var_name) := NA]
}
factor_vars = c("bias", "targeted_group", "offense")
lapply(factor_vars, replace_empty_with_na)
dc_hate[, (factor_vars) := lapply(.SD, function(x) factor(tolower(x))), .SDcols = factor_vars]
save(dc_hate, file = "dc_hate.RData")

# exploration
## types over the years
dc_hate[, general_offense := offense]
dc_hate[grepl("property", general_offense), general_offense := "damage or destruction to property"]
dc_hate[grepl("threat", general_offense), general_offense := "threats"]
dc_hate[grepl("assault", general_offense), general_offense := "assault"]
dc_hate[grepl("adw", general_offense), general_offense := "assault"]
library(RColorBrewer)
ggplot(dc_hate[year(offense_date) < 2019]) + geom_bar(aes(x = year(offense_date), fill = general_offense), stat = "count") +
    scale_fill_brewer(palette="Set1")
## counts
dc_hate_counts = dc_hate[, .(year = year(offense_date), month = month(offense_date))][, .(hate_count = .N), by = .(month, year)][year <= 2017]









#
