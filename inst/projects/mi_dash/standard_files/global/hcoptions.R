### set options for highcharter package
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- c("k", "M", "B", "T", "P", "E")
options(highcharter.lang = hcoptslang)
