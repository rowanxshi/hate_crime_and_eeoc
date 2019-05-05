load("eeoc.RData")

# state and court
state_names = fread("state_name, state\nAlabama, AL\nAlaska, AK\nArizona, AZ\nArkansas, AR\nCalifornia, CA\nColorado, CO\nConnecticut, CT\nDelaware, DE\nFlorida, FL\nGeorgia, GA\nHawaii, HI\nIdaho, ID\nIllinois, IL\nIndiana, IN\nIowa, IA\nKansas, KS\nKentucky, KY\nLouisiana, LA\nMaine, ME\nMaryland, MD\nMassachusetts, MA\nMichigan, MI\nMinnesota, MN\nMississippi, MS\nMissouri, MO\nMontana, MT\nNebraska, NE\nNevada, NV\nNew Hampshire, NH\nNew Jersey, NJ\nNew Mexico, NM\nNew York, NY\nNorth Carolina, NC\nNorth Dakota, ND\nOhio, OH\nOklahoma, OK\nOregon, OR\nPennsylvania, PA\nRhode Island, RI\nSouth Carolina, SC\nSouth Dakota, SD\nTennessee, TN\nTexas, TX\nUtah, UT\nVermont, VT\nVirginia, VA\nWashington, WA\nWest Virginia, WV\nWisconsin, WI\nWyoming, WY\nAmerican Samoa, AS\nDistrict of Columbia, DC\nFederated States of Micronesia, FM\nGuam, GU\nMarshall Islands, MH\nNorthern Mariana Islands, MP\nPalau, PW\nPuerto Rico, PR\nVirgin Islands, VI\nArmed Forces Pacific, AP")
state_names = state_names[, .(state, state_name)]
full = state_names[full, on = .(state)]
full[, state := factor(state)]

# length and age
full[, c("case_length", "age") := .(difftime(date_closed, date_received, units = "days"), as.integer(floor(difftime(date_received, birth_date, units = "weeks")/52)))]
full = full[((age >= 18 & age <= 85) | is.na(age)) & ((case_length >= 0) | is.na(case_length))]

# time series -- probably want to do per capita or something rates to compare states
eeoc_counts = full[, .(year = year(date_received), month = month(date_received), state, naics2 = factor(substr(naics_code, 1, 2)))][, .(eeoc_count = .N), by = .(year, month, state, naics2)]
setkeyv(eeoc_counts, c("year", "month", "naics2", "state"))
fill_combinations = function(var_list, data) {
  build_scaffold = function(var_list) {
    get_values = function(variable) {
      data[, get(variable)] %>% unique %>% factor
    }
    values = lapply(var_list, get_values)
    scaffold = do.call(expand.grid, values) %>% setDT
    names(scaffold) = var_list
    setkeyv(scaffold, var_list)
    scaffold
  }
  merge(build_scaffold(var_list), copy(data)[, (var_list) := lapply(.SD, factor), .SDcols = var_list], all = T)
}
eeoc_counts = fill_combinations(c("year", "month", "state", "naics2"), eeoc_counts)
eeoc_counts[is.na(eeoc_count), eeoc_count := 0]
eeoc_counts = eeoc_counts[!(year == 2017 & month %in% 10:12)]

## DC
load("dc_hate_counts.RData")
dc_hate_counts = fill_combinations(c("year","month"), dc_hate_counts)
dc_hate_counts[is.na(hate_count), hate_count := 0]
save(dc_hate_count, file ="dc_hate_counts.RData")
### merge
dc_hate_counts[, paste0("lag", 1:2, "_hate_count") := lapply(1:2, function(x) shift(hate_count, n=x))]
dc_merged = merge(eeoc_counts[state == "DC"], dc_hate_counts, by = c("year", "month"))
dc_model = lm(eeoc_count ~ factor(year) + factor(month) + factor(naics2) + hate_count + lag1_hate_count + lag2_hate_count, data = dc_merged)
dc_merged[!is.na(lag2_hate_count) & !is.na(naics2), eeoc_count_hat := predict.lm(dc_model)]
dc_merged[, eeoc_count_demeaned := dc_model$residuals + dc_model$coefficients[["lag1_hate_count"]]*lag1_hate_count + dc_model$coefficients[["lag2_hate_count"]]*lag2_hate_count + dc_model$coefficients[["hate_count"]]*hate_count]
ggplot(dc_merged, aes(x = lag1_hate_count, y = eeoc_count_demeaned)) + geom_jitter(alpha = 0.25, color = "red") + labs(x = "previous month hate crimes", y = "number of EEOC complaints")

























#