# Copyright (c) 2023 Honorata Bogusz

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

options(timeout = 600)

requiredPackages = c("splitstackshape", "tidyverse", "dplyr", "reshape2", "haven", 
                     "stringr", "viridisLite", "gridExtra", "lmreg", "mice","Hmisc",
                     "ggrepel", "viridis", "foreign", "bnstruct", "outliers","forecast",
                     "TSstudio", "zoo", "factoextra", "RStata", "plyr", "zoo", "imputeTS",
                     "readxl", "plm")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
gc()

# Load exposure to robots
load("../generated_data/etr_nonpioneer_2007_2017.RData")

regions_df_2 <- exposure_to_robots[,c("region", "year")]
regions_df_2$year <- NULL
regions_df_2 <- unique(regions_df_2)
regions_df_2$list <- 1

# Load exposure to robots
load("../generated_data//etr_pioneer_1997_2017.RData")

regions_df_1 <- exposure_to_robots[,c("region", "year")]
regions_df_1$year <- NULL
regions_df_1 <- unique(regions_df_1)
regions_df_1$list <- 1

rm(exposure_to_robots)

unique_countries <- c("IE", "UK", 
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL", 
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")

# 1. FERTILITY RATES

fertility <- read.csv("../original_data/fertility_rates.csv", header=T)

fertility <- fertility[,c("geo", "TIME_PERIOD", "age", "OBS_VALUE")]
colnames(fertility) <- c("region", "year", "age", "fr")
fertility <- subset(fertility, year>1996 & year<2018)
unique(fertility$year)

for(i in names(fertility)){
  fertility[[i]] <- str_replace_all(fertility[[i]], "b", "")
  fertility[[i]] <- str_replace_all(fertility[[i]], "u", "")
  fertility[[i]] <- str_replace_all(fertility[[i]], "e", "")
  fertility[[i]] <- str_replace_all(fertility[[i]], " ", "")
  fertility[[i]] <- str_replace_all(fertility[[i]], ":", "")
}

fertility$fr <- as.numeric(fertility$fr)

na <- is.na(fertility)
sum(na)/sum(1-na)

# reshape from long to wide
fertility <- reshape(fertility, idvar = c("region", "year"), timevar = "age", direction = "wide")

# check the number of NAs and do linear interpolations in subgroups
fertility <- fertility %>% mutate_all(~ifelse(is.nan(.), NA, .))
temp <- fertility[rowSums(is.na(fertility)) > 0,]

df <- data.frame()

for (region_ in unique(fertility$region)){
  temp <- subset(fertility, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

fertility <- df
temp <- fertility[rowSums(is.na(fertility)) > 0,]

# replace NA with 0
fertility[is.na(fertility)] <- 0

# calculate age-specific fertility rates in intervals

fertility$fr_2024 <- fertility$fr.Y20 +
  fertility$fr.Y21 +
  fertility$fr.Y22 +
  fertility$fr.Y23 +
  fertility$fr.Y24

fertility$fr_2529 <- fertility$fr.Y25 +
  fertility$fr.Y26 +
  fertility$fr.Y27 +
  fertility$fr.Y28 +
  fertility$fr.Y29

fertility$fr_3034 <- fertility$fr.Y30 +
  fertility$fr.Y31 +
  fertility$fr.Y32 +
  fertility$fr.Y33 +
  fertility$fr.Y34

fertility$fr_3539 <- fertility$fr.Y35 +
  fertility$fr.Y36 +
  fertility$fr.Y37 +
  fertility$fr.Y38 +
  fertility$fr.Y39

fertility$fr_4044 <- fertility$fr.Y40 +
  fertility$fr.Y41 +
  fertility$fr.Y42 +
  fertility$fr.Y43 +
  fertility$fr.Y44

fertility$fr_45plus <- fertility$fr.Y45 +
  fertility$fr.Y46 +
  fertility$fr.Y47 +
  fertility$fr.Y48 +
  fertility$fr.Y49 +
  fertility$fr.Y_GE50

colnames(fertility)[3] <- "tfr"

fertility <- fertility[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539", "fr_4044", "fr_45plus")]

rm(list=setdiff(ls(), c("fertility", "regions", "regions_df_1", "regions_df_2", "unique_countries")))
temp <- fertility[rowSums(is.na(fertility)) > 0,]

# discard countries and regions from outside our sample

fertility$country <- sub('..$','',fertility$region)
length(unique_countries)
length(unique(fertility$country))
fertility <- fertility[fertility$country %in% unique_countries, ]
length(unique(fertility$country))
fertility$country <- NULL
length(unique(fertility$region))
# 26 countries from the list still in dataframe -- GOOD

# length(regions)
# length(unique(fertility$region))
# fertility <- fertility[fertility$region %in% regions, ]
# length(unique(fertility$region))
# # 8 regions lost

fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

length(unique(fertility$region))

# exclude regions which are overseas
discarded_regions <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(unique(fertility$region))
`%ni%` <- Negate(`%in%`)
fertility <- fertility[fertility$region %ni% discarded_regions, ]
length(unique(fertility$region))
# 10 regions discarded

# compare regions between the dataframe and our list
regions_fert <- fertility[,c("region", "year")]
regions_fert$year <- NULL
regions_fert <- unique(regions_fert)
regions_fert$df <- 1

temp_1 <- merge(regions_df_1, regions_fert, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_fert, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

rm(list=setdiff(ls(), c("fertility", "regions", "regions_df_1", "regions_df_2", "temp_1", "temp_2")))

# need to sum up split regions: first for both 1997-2007 and 2007-2017

# HU11 + HU12 = HU10
case_temp <- subset(fertility, region=="HU11" | region=="HU12")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# IE04 = IE01
fertility$region <- str_replace_all(fertility$region, "IE04", "IE01")

# IE05 + IE06 = IE02
case_temp <- subset(fertility, region=="IE05" | region=="IE06")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(fertility, region=="LT01" | region=="LT02")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(fertility, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(fertility, region=="UKM8" | region=="UKM9")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(fertility, region=="UKI3" | region=="UKI4")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(fertility, region=="UKI5" | region=="UKI6" | region=="UKI7")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# compare regions between the dataframe and our list
regions_fert <- fertility[,c("region", "year")]
regions_fert$year <- NULL
regions_fert <- unique(regions_fert)
regions_fert$df <- 1

temp <- merge(regions_df_2, regions_fert, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_fert, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

rm(list=setdiff(ls(), c("fertility", "regions_df_1", "regions_df_2")))

# Load exposure to robots and merge with fertility
load("../generated_data/etr_nonpioneer_2007_2017.RData")

df_2007_2017 <- merge(exposure_to_robots, fertility, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(fertility, region=="DEB1" | region=="DEB2" | region=="DEB3")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(fertility, region=="DED2" | region=="DED4" | region=="DED5")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(fertility, region=="FI1B" | region=="FI1C")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(fertility, region=="ITH1" | region=="ITH2")
case_temp_neq <- setdiff(fertility, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$tfr, case_temp$fr_2024, case_temp$fr_2529, case_temp$fr_3034, case_temp$fr_3539, case_temp$fr_4044, case_temp$fr_45plus), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp[,c("region", "year", "tfr", "fr_2024", "fr_2529", "fr_3034", "fr_3539","fr_4044", "fr_45plus")]

fertility <- rbind(case_temp, case_temp_neq)
fertility <- fertility[with(fertility, order(region, year)),]
row.names(fertility) <- NULL

# check region differences for 1997-2017
regions_fert <- fertility[,c("region", "year")]
regions_fert$year <- NULL
regions_fert <- unique(regions_fert)
regions_fert$df <- 1

temp <- merge(regions_df_1, regions_fert, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

load("../generated_data//etr_pioneer_1997_2017.RData")

df_1997_2017 <- merge(exposure_to_robots, fertility, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))











# 2. EMPLOYMENT RATES

# read the original tsv from Eurostat
employment_rates <- read.delim("../original_data/employment_rates.tsv")
# split one column separated by commas, not semicolons
employment_rates <- cSplit(employment_rates, 1, sep=",", type.convert=FALSE)
# dispose of columns that are not needed
employment_rates <- employment_rates[,c(1:19, 25, 27, 29)]
# rename columns
colnames(employment_rates) <- c("1999", "2000", "2001", "2002", "2003", "2004",
                                "2005", "2006", "2007", "2008", "2009", "2010",
                                "2011", "2012", "2013", "2014", "2015", "2016",
                                "2017", "isced11", "sex", "region")
# delete non-NUTS2 units
employment_rates <- employment_rates[(nchar(as.character(employment_rates$region)) == 4),]
# dispose of flags from Eurostat
for(i in 1:19){
  employment_rates[[i]] <- str_replace_all(employment_rates[[i]], "b", "")
  employment_rates[[i]] <- str_replace_all(employment_rates[[i]], "u", "")
  employment_rates[[i]] <- str_replace_all(employment_rates[[i]], "e", "")
  employment_rates[[i]] <- str_replace_all(employment_rates[[i]], " ", "")
  employment_rates[[i]] <- str_replace_all(employment_rates[[i]], ":", "")
}
# convert strings to numeric values
for(i in 1:19){
  employment_rates[[i]] <- as.numeric(employment_rates[[i]])
}

# calculate the fraction of NAs
na <- is.na(employment_rates)
sum(na)/sum(1-na)

employment_rates <- employment_rates %>%
  gather(year, dummy, -region, -sex, -isced11) %>%
  spread(sex, dummy) %>%
  melt(id.vars = c("isced11", "region", "year"), measure.vars = c("T", "F", "M")) %>%
  dcast(region + year ~  variable + isced11)

colnames(employment_rates) <- c("region", "year",
                                "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total",
                                "er_female_isced02", "er_female_isced34", "er_female_isced58", "er_female_total",
                                "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")

# remove rows with countries not of our interest
employment_rates$country <- sub('..$','',employment_rates$region)
unique_countries <- c("IE", "UK", 
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL", 
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(employment_rates$country))
employment_rates <- employment_rates[employment_rates$country %in% unique_countries, ]
length(unique(employment_rates$country))
employment_rates$country <- NULL

# compare regions between the dataframe and our list
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp_1 <- merge(regions_df_1, regions_empl, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_empl, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

# HU11 + HU12 = HU10
case_temp <- subset(employment_rates, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# IE04 = IE01
case_temp <- subset(employment_rates, region=="IE04" & year>2011)
case_temp_neq <- setdiff(employment_rates, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(employment_rates, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(employment_rates, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(employment_rates, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(employment_rates, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(employment_rates, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(employment_rates, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# compare regions between the dataframe and our list
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp <- merge(regions_df_2, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, employment_rates, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(employment_rates, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(employment_rates, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(employment_rates, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(employment_rates, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_total_isced02, case_temp$er_total_isced34, case_temp$er_total_isced58, case_temp$er_total_total, case_temp$er_female_isced02, case_temp$er_female_isced34, case_temp$er_female_isced58, case_temp$er_female_total, case_temp$er_male_isced02, case_temp$er_male_isced34, case_temp$er_male_isced58, case_temp$er_male_total), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp[,c("region", "year", "er_total_isced02", "er_total_isced34", "er_total_isced58", "er_total_total", "er_female_isced02","er_female_isced34", "er_female_isced58", "er_female_total", "er_male_isced02", "er_male_isced34", "er_male_isced58", "er_male_total")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# check region differences for 1997-2007
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp <- merge(regions_df_1, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, employment_rates, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))









# 3. EMPLOYMENT RATES BY AGE

# read the original tsv from Eurostat
employment_rates <- read.csv("../original_data/employment_rates_age.csv")

employment_rates <- employment_rates[,c("sex", "age", "geo", "TIME_PERIOD", "OBS_VALUE")]
colnames(employment_rates) <- c("sex", "age", "region", 'year', "er")

employment_rates <- employment_rates[(nchar(as.character(employment_rates$region)) == 4),]

# dispose of flags from Eurostat
employment_rates$er <- str_replace_all(employment_rates$er, "b", "")
employment_rates$er <- str_replace_all(employment_rates$er, "u", "")
employment_rates$er <- str_replace_all(employment_rates$er, "e", "")
employment_rates$er <- str_replace_all(employment_rates$er, " ", "")
employment_rates$er <- str_replace_all(employment_rates$er, ":", "")

employment_rates$er <- as.numeric(employment_rates$er)

# calculate the fraction of NAs
na <- is.na(employment_rates)
sum(na)/sum(1-na)

employment_rates <- dcast(employment_rates, region + year ~ sex + age, value.var = "er")

colnames(employment_rates) <- c("region", "year",
                                "er_female_65plus", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4554", "er_female_5564",
                                "er_male_65plus", "er_male_1524", "er_male_2534", "er_male_3544", "er_male_4554", "er_male_5564",
                                "er_total_65plus", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4554", "er_total_5564")

# check the number of NAs and do linear interpolations in subgroups
temp <- employment_rates[rowSums(is.na(employment_rates)) > 0,]

df <- data.frame()

for (region_ in unique(employment_rates$region)){
  temp <- subset(employment_rates, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

employment_rates <- df
temp <- employment_rates[rowSums(is.na(employment_rates)) > 0,]

# delete employment 65+
employment_rates$er_female_65plus <- NULL
employment_rates$er_male_65plus <- NULL
employment_rates$er_total_65plus <- NULL

temp <- employment_rates[rowSums(is.na(employment_rates)) > 0,]

employment_rates$er_female_4564 <- (employment_rates$er_female_4554 + employment_rates$er_female_5564)/2
employment_rates$er_male_4564 <- (employment_rates$er_male_4554 + employment_rates$er_male_5564)/2
employment_rates$er_total_4564 <- (employment_rates$er_total_4554 + employment_rates$er_total_5564)/2

employment_rates$er_female_4554 <- NULL
employment_rates$er_female_5564 <- NULL
employment_rates$er_male_4554 <- NULL
employment_rates$er_male_5564 <- NULL
employment_rates$er_total_4554 <- NULL
employment_rates$er_total_5564 <- NULL

# remove rows with countries not of our interest
employment_rates$country <- sub('..$','',employment_rates$region)
unique_countries <- c("IE", "UK", 
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL", 
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(employment_rates$country))
employment_rates <- employment_rates[employment_rates$country %in% unique_countries, ]
length(unique(employment_rates$country))
employment_rates$country <- NULL

# compare regions between the dataframe and our list
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp_1 <- merge(regions_df_1, regions_empl, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_empl, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

# HU11 + HU12 = HU10
case_temp <- subset(employment_rates, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# IE04 = IE01
case_temp <- subset(employment_rates, region=="IE04" & year>2011)
case_temp_neq <- setdiff(employment_rates, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(employment_rates, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(employment_rates, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(employment_rates, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(employment_rates, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(employment_rates, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(employment_rates, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# compare regions between the dataframe and our list
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp <- merge(regions_df_2, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, employment_rates, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(employment_rates, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(employment_rates, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(employment_rates, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(employment_rates, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(employment_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$er_female_1524, case_temp$er_female_2534, case_temp$er_female_3544, case_temp$er_female_4564, case_temp$er_male_1524, case_temp$er_male_2534, case_temp$er_male_3544, case_temp$er_male_4564, case_temp$er_total_1524, case_temp$er_total_2534, case_temp$er_total_3544, case_temp$er_total_4564), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp[,c("region", "year", "er_female_1524", "er_female_2534", "er_female_3544", "er_female_4564", "er_male_1524","er_male_2534", "er_male_3544", "er_male_4564", "er_total_1524", "er_total_2534", "er_total_3544", "er_total_4564")]

employment_rates <- rbind(case_temp, case_temp_neq)
employment_rates <- employment_rates[with(employment_rates, order(region, year)),]
row.names(employment_rates) <- NULL

# check region differences for 1997-2007
regions_empl <- employment_rates[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp <- merge(regions_df_1, regions_empl, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, employment_rates, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))










# 4. POPULATION STRUCTURE

# read the original tsv from Eurostat
population_structure <- read.delim("../original_data/population_structure.tsv")
# split one column separated by commas, not semicolons
population_structure <- cSplit(population_structure, 1, sep=",", type.convert=FALSE)
# dispose of columns that are not needed
population_structure <- population_structure[,c(6:28, 34, 35, 36)]
# rename columns
colnames(population_structure) <- c("1995", "1996", "1997", "1998", "1999", "2000",
                                "2001", "2002", "2003", "2004", "2005",
                                "2006", "2007", "2008", "2009", "2010",
                                "2011", "2012", "2013", "2014", "2015",
                                "2016", "2017", "sex", "age", "region")
# delete non-NUTS2 units
population_structure <- population_structure[(nchar(as.character(population_structure$region)) == 4),]
# dispose of flags from Eurostat
for(i in 1:23){
  population_structure[[i]] <- str_replace_all(population_structure[[i]], "b", "")
  population_structure[[i]] <- str_replace_all(population_structure[[i]], "u", "")
  population_structure[[i]] <- str_replace_all(population_structure[[i]], "e", "")
  population_structure[[i]] <- str_replace_all(population_structure[[i]], " ", "")
  population_structure[[i]] <- str_replace_all(population_structure[[i]], ":", "")
}
# convert strings to numeric values
for(i in 1:23){
  population_structure[[i]] <- as.numeric(population_structure[[i]])
}

# calculate the fraction of NAs
na <- is.na(population_structure)
sum(na)/sum(1-na)

population_structure <- population_structure %>%
  gather(year, dummy, -region, -sex, -age) %>%
  spread(sex, dummy) %>%
  melt(id.vars = c("age", "region", "year"), measure.vars = c("T", "F", "M")) %>%
  dcast(region + year ~  variable + age)

colnames(population_structure) <- c("region", "year",
                                "total_total", "total_y75plus", "total_y1519", "total_y2024",
                                "total_y2529", "total_y3034", "total_y3539", "total_y4044",
                                "total_y4549", "total_y5054", "total_y5559", "total_y6064",
                                "total_y6569", "total_y7074",
                                "female_total", "female_y75plus", "female_y1519", "female_y2024",
                                "female_y2529", "female_y3034", "female_y3539", "female_y4044",
                                "female_y4549", "female_y5054", "female_y5559", "female_y6064",
                                "female_y6569", "female_y7074",
                                "male_total", "male_y75plus", "male_y1519", "male_y2024",
                                "male_y2529", "male_y3034", "male_y3539", "male_y4044",
                                "male_y4549", "male_y5054", "male_y5559", "male_y6064",
                                "male_y6569", "male_y7074")

# remove rows with countries not of our interest
population_structure$country <- sub('..$','',population_structure$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(population_structure$country))
population_structure <- population_structure[population_structure$country %in% unique_countries, ]
length(unique(population_structure$country))
population_structure$country <- NULL

population_structure <- subset(population_structure, year>1996)

# do linear interpolations to calculate sums

colSums(is.na(population_structure))

df <- data.frame()

for (region_ in unique(population_structure$region)){
  temp <- subset(population_structure, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

colSums(is.na(df))

population_structure <- df
rm(df)

# calculate sums to reflect fertility groups

population_structure$pop_tot_1524 <- population_structure$total_y1519 + population_structure$total_y2024
population_structure$pop_tot_2534 <- population_structure$total_y2529 + population_structure$total_y3034
population_structure$pop_tot_3549 <- population_structure$total_y3539 + population_structure$total_y4044 + population_structure$total_y4549
population_structure$pop_tot_50plus <- population_structure$total_y5054 + population_structure$total_y5559 + population_structure$total_y6064 + population_structure$total_y6569 + population_structure$total_y7074 + population_structure$total_y75plus

population_structure$pop_fem_1524 <- population_structure$female_y1519 + population_structure$female_y2024
population_structure$pop_fem_2534 <- population_structure$female_y2529 + population_structure$female_y3034
population_structure$pop_fem_3549 <- population_structure$female_y3539 + population_structure$female_y4044 + population_structure$female_y4549
population_structure$pop_fem_50plus <- population_structure$female_y5054 + population_structure$female_y5559 + population_structure$female_y6064 + population_structure$female_y6569 + population_structure$female_y7074 + population_structure$female_y75plus

population_structure$pop_male_1524 <- population_structure$male_y1519 + population_structure$male_y2024
population_structure$pop_male_2534 <- population_structure$male_y2529 + population_structure$male_y3034
population_structure$pop_male_3549 <- population_structure$male_y3539 + population_structure$male_y4044 + population_structure$male_y4549
population_structure$pop_male_50plus <- population_structure$male_y5054 + population_structure$male_y5559 + population_structure$male_y6064 + population_structure$male_y6569 + population_structure$male_y7074 + population_structure$male_y75plus

population_structure <- population_structure[,c("region", "year",
                                                "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "total_total",
                                                "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "female_total",
                                                "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "male_total")]

colnames(population_structure)[7] <- "pop_tot"
colnames(population_structure)[12] <- "pop_fem"
colnames(population_structure)[17] <- "pop_male"

# compare regions between the dataframe and our list
regions_pop <- population_structure[,c("region", "year")]
regions_pop$year <- NULL
regions_pop <- unique(regions_pop)
regions_pop$df <- 1

temp_1 <- merge(regions_df_1, regions_pop, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_pop, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

# HU11 + HU12 = HU10
case_temp <- subset(population_structure, (region=="HU11" | region=="HU12"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# IE04 = IE01
case_temp <- subset(population_structure, region=="IE04")
case_temp_neq <- setdiff(population_structure, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(population_structure, (region=="IE05" | region=="IE06"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(population_structure, (region=="LT01" | region=="LT02"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(population_structure, (region=="PL91" | region=="PL92"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(population_structure, (region=="UKM8" | region=="UKM9"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(population_structure, (region=="UKI3" | region=="UKI4"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(population_structure, (region=="UKI5" | region=="UKI6" | region=="UKI7"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# compare regions between the dataframe and our list
regions_pop <- population_structure[,c("region", "year")]
regions_pop$year <- NULL
regions_pop <- unique(regions_pop)
regions_pop$df <- 1

temp <- merge(regions_df_2, regions_pop, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_pop, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, population_structure, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2017

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(population_structure, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(population_structure, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(population_structure, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(population_structure, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(population_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$pop_tot_1524, case_temp$pop_tot_2534, case_temp$pop_tot_3549, case_temp$pop_tot_50plus, case_temp$pop_tot, case_temp$pop_fem_1524, case_temp$pop_fem_2534, case_temp$pop_fem_3549, case_temp$pop_fem_50plus, case_temp$pop_fem, case_temp$pop_male_1524, case_temp$pop_male_2534, case_temp$pop_male_3549, case_temp$pop_male_50plus, case_temp$pop_male), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp[,c("region", "year",
                          "pop_tot_1524", "pop_tot_2534", "pop_tot_3549", "pop_tot_50plus", "pop_tot",
                          "pop_fem_1524", "pop_fem_2534", "pop_fem_3549", "pop_fem_50plus", "pop_fem",
                          "pop_male_1524", "pop_male_2534", "pop_male_3549", "pop_male_50plus", "pop_male")]

population_structure <- rbind(case_temp, case_temp_neq)
population_structure <- population_structure[with(population_structure, order(region, year)),]
row.names(population_structure) <- NULL

# check region differences for 1997-2017
regions_pop <- population_structure[,c("region", "year")]
regions_pop$year <- NULL
regions_pop <- unique(regions_pop)
regions_pop$df <- 1

temp <- merge(regions_df_1, regions_pop, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, population_structure, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))








# 5. EDUCATION STRUCTURE

# read the original tsv from Eurostat
education_structure <- read.delim("../original_data/education_structure.tsv")
# split one column separated by commas, not semicolons
education_structure <- cSplit(education_structure, 1, sep=",", type.convert=FALSE)
# dispose of columns that are not needed
education_structure <- education_structure[,c(1:18, 23, 24, 27)]
# rename columns
colnames(education_structure) <- c("2000",
                                    "2001", "2002", "2003", "2004", "2005",
                                    "2006", "2007", "2008", "2009", "2010",
                                    "2011", "2012", "2013", "2014", "2015",
                                    "2016", "2017", "sex", "isced11", "region")

# delete non-NUTS2 units
education_structure <- education_structure[(nchar(as.character(education_structure$region)) == 4),]

# dispose of flags from Eurostat
for(i in 1:18){
  education_structure[[i]] <- str_replace_all(education_structure[[i]], "b", "")
  education_structure[[i]] <- str_replace_all(education_structure[[i]], "u", "")
  education_structure[[i]] <- str_replace_all(education_structure[[i]], "e", "")
  education_structure[[i]] <- str_replace_all(education_structure[[i]], " ", "")
  education_structure[[i]] <- str_replace_all(education_structure[[i]], ":", "")
}
# convert strings to numeric values
for(i in 1:18){
  education_structure[[i]] <- as.numeric(education_structure[[i]])
}

# calculate the fraction of NAs
na <- is.na(education_structure)
sum(na)/sum(1-na)

education_structure <- education_structure %>%
  gather(year, dummy, -region, -sex, -isced11) %>%
  spread(sex, dummy) %>%
  melt(id.vars = c("isced11", "region", "year"), measure.vars = c("T", "F", "M")) %>%
  dcast(region + year ~  variable + isced11)

colnames(education_structure) <- c("region", "year",
                                "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",
                                "edu_female_isced02", "edu_female_isced34", "edu_female_isced58",
                                "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")

# remove rows with countries not of our interest
education_structure$country <- sub('..$','',education_structure$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(education_structure$country))
education_structure <- education_structure[education_structure$country %in% unique_countries, ]
length(unique(education_structure$country))
education_structure$country <- NULL

# compare regions between the dataframe and our list
regions_edu <- education_structure[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp_1 <- merge(regions_df_1, regions_edu, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_edu, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

# HU11 + HU12 = HU10
case_temp <- subset(education_structure, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,
                            case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,
                            case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# IE04 = IE01
case_temp <- subset(education_structure, region=="IE04" & year>2011)
case_temp_neq <- setdiff(education_structure, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(education_structure, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(education_structure, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(education_structure, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(education_structure, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(education_structure, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(education_structure, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# compare regions between the dataframe and our list
regions_edu <- education_structure[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, education_structure, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(education_structure, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(education_structure, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(education_structure, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(education_structure, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(education_structure, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$edu_total_isced02, case_temp$edu_total_isced34, case_temp$edu_total_isced58,                             case_temp$edu_female_isced02, case_temp$edu_female_isced34, case_temp$edu_female_isced58,                             case_temp$edu_male_isced02, case_temp$edu_male_isced34, case_temp$edu_male_isced58), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "edu_total_isced02", "edu_total_isced34", "edu_total_isced58",                                 "edu_female_isced02", "edu_female_isced34", "edu_female_isced58", "edu_male_isced02", "edu_male_isced34", "edu_male_isced58")]

education_structure <- rbind(case_temp, case_temp_neq)
education_structure <- education_structure[with(education_structure, order(region, year)),]
row.names(education_structure) <- NULL

# check region differences for 1997-2007
regions_edu <- education_structure[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, education_structure, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))









# 6. SHARE OF ECONOMICALLY ACTIVE PEOPLE/WOMEN/MEN

economic_activity_rates <- read.csv("../original_data/economic_activity_rates.csv")
economic_activity_rates <- economic_activity_rates[,c("geo", "TIME_PERIOD", "sex", "OBS_VALUE")]
economic_activity_rates <- subset(economic_activity_rates, year<2018)
colnames(economic_activity_rates) <- c("region", "year", "sex", "economic_activity_rate")

economic_activity_rates <- dcast(economic_activity_rates, region + year ~ sex, value.var = "economic_activity_rate")
colnames(economic_activity_rates) <- c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")

# remove rows with countries not of our interest
economic_activity_rates$country <- sub('..$','',economic_activity_rates$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(economic_activity_rates$country))
economic_activity_rates <- economic_activity_rates[economic_activity_rates$country %in% unique_countries, ]
length(unique(economic_activity_rates$country))
economic_activity_rates$country <- NULL

# compare regions between the dataframe and our list
regions_edu <- economic_activity_rates[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp_1 <- merge(regions_df_1, regions_edu, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_edu, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]


# HU11 + HU12 = HU10
case_temp <- subset(economic_activity_rates, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# IE04 = IE01
case_temp <- subset(economic_activity_rates, region=="IE04" & year>2011)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(economic_activity_rates, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(economic_activity_rates, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(economic_activity_rates, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(economic_activity_rates, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(economic_activity_rates, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(economic_activity_rates, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# compare regions between the dataframe and our list
regions_edu <- economic_activity_rates[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, economic_activity_rates, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(economic_activity_rates, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(economic_activity_rates, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(economic_activity_rates, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(economic_activity_rates, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(economic_activity_rates, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$ec_active_female, case_temp$ec_active_male, case_temp$ec_active_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "ec_active_female", "ec_active_male", "ec_active_tot")]

economic_activity_rates <- rbind(case_temp, case_temp_neq)
economic_activity_rates <- economic_activity_rates[with(economic_activity_rates, order(region, year)),]
row.names(economic_activity_rates) <- NULL

# check region differences for 1997-2007
regions_edu <- economic_activity_rates[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, economic_activity_rates, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))












# 7. SHARE OF PEOPLE IN MANUFACTURING/SERVICES

# 1999-2008

empl_1 <- read.csv("../original_data/employment_economic_activity_1.csv")
empl_1 <- empl_1[,c("geo", "TIME_PERIOD", "sex", "nace_r1", "OBS_VALUE")]
colnames(empl_1) <- c("region", "year", "sex", "industry", "empl")

empl_1 <- dcast(empl_1, region + year ~ sex + industry, value.var = "empl")

# check the number of NAs and do linear interpolations in subgroups
temp <- empl_1[rowSums(is.na(empl_1)) > 0,]

df <- data.frame()

for (region_ in unique(empl_1$region)){
  temp <- subset(empl_1, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

empl_1 <- df
temp <- empl_1[rowSums(is.na(empl_1)) > 0,]

empl_1$female_industry <- empl_1$`F_C-E`/empl_1$`F_TOTAL`
empl_1$female_service <- empl_1$`F_G-Q`/empl_1$`F_TOTAL`

empl_1$male_industry <- empl_1$`M_C-E`/empl_1$`M_TOTAL`
empl_1$male_service <- empl_1$`M_G-Q`/empl_1$`M_TOTAL`

empl_1$all_industry <- empl_1$`T_C-E`/empl_1$`T_TOTAL`
empl_1$all_service <- empl_1$`T_G-Q`/empl_1$`T_TOTAL`


empl_1 <- empl_1[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]



# 2008-2018

empl_2 <- read.csv("../original_data/employment_economic_activity_2.csv")
empl_2 <- empl_2[,c("geo", "TIME_PERIOD", "sex", "nace_r2", "OBS_VALUE")]
colnames(empl_2) <- c("region", "year", "sex", "industry", "empl")

empl_2 <- dcast(empl_2, region + year ~ sex + industry, value.var = "empl")

# check the number of NAs and do linear interpolations in subgroups
temp <- empl_2[rowSums(is.na(empl_2)) > 0,]

df <- data.frame()

for (region_ in unique(empl_2$region)){
  temp <- subset(empl_2, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

empl_2 <- df
temp <- empl_2[rowSums(is.na(empl_2)) > 0,]

empl_2 <- subset(empl_2, year<2018 & year>2008)

# compare regions in the two datasets

r1 <- empl_1[,c("region", "year")]
r1$year <- NULL
r1 <- unique(r1)
r1$list1 <- 1

r2 <- empl_2[,c("region", "year")]
r2$year <- NULL
r2 <- unique(r2)
r2$list2 <- 2

lists <- merge(r1, r2, by = "region", all = T)

# merge empl1 and empl2

empl_2$female_industry <- empl_2$`F_B-E`/empl_2$`F_TOTAL`
empl_2$female_service <- (empl_2$`F_G-I` + empl_2$F_J + empl_2$F_K + empl_2$F_L + empl_2$F_M_N + empl_2$`F_O-Q`)/empl_2$`F_TOTAL`

empl_2$male_industry <- empl_2$`M_B-E`/empl_2$`M_TOTAL`
empl_2$male_service <- (empl_2$`M_G-I` + empl_2$M_J + empl_2$M_K + empl_2$M_L + empl_2$M_M_N + empl_2$`M_O-Q`)/empl_2$`M_TOTAL`

empl_2$all_industry <- empl_2$`T_B-E`/empl_2$`T_TOTAL`
empl_2$all_service <- (empl_2$`T_G-I` + empl_2$T_J + empl_2$T_K + empl_2$T_L + empl_2$T_M_N + empl_2$`T_O-Q`)/empl_2$`T_TOTAL`

empl_2 <- empl_2[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

# check the number of NAs and do linear interpolations in subgroups
temp <- empl_2[rowSums(is.na(empl_2)) > 0,]

df <- data.frame()

for (region_ in unique(empl_2$region)){
  temp <- subset(empl_2, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

empl_2 <- df
temp <- empl_2[rowSums(is.na(empl_2)) > 0,]

empl <- rbind(empl_1, empl_2)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL
rm(empl_1, empl_2)

# remove rows with countries not of our interest
empl$country <- sub('..$','',empl$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(empl$country))
empl <- empl[empl$country %in% unique_countries, ]
length(unique(empl$country))
empl$country <- NULL

# compare regions between the dataframe and our list
regions_empl <- empl[,c("region", "year")]
regions_empl$year <- NULL
regions_empl <- unique(regions_empl)
regions_empl$df <- 1

temp_1 <- merge(regions_df_1, regions_empl, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_empl, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]



# HU11 + HU12 = HU10
case_temp <- subset(empl, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# IE04 = IE01
case_temp <- subset(empl, region=="IE04" & year>2011)
case_temp_neq <- setdiff(empl, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(empl, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(empl, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(empl, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(empl, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(empl, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(empl, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# compare regions between the dataframe and our list
regions_edu <- empl[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, empl, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(empl, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(empl, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(empl, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(empl, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(empl, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$female_industry, case_temp$female_service, case_temp$male_industry, case_temp$male_service, case_temp$all_industry, case_temp$all_service), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "female_industry", "female_service", "male_industry", "male_service", "all_industry", "all_service")]

empl <- rbind(case_temp, case_temp_neq)
empl <- empl[with(empl, order(region, year)),]
row.names(empl) <- NULL

# check region differences for 1997-2007
regions_edu <- empl[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, empl, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))







# 8. YOUTH (15-24) UNEMPLOYMENT RATE

youth_unemployment <- read.csv("../original_data/youth_unemployment.csv")
youth_unemployment <- youth_unemployment[,c("geo", "TIME_PERIOD", "sex", "OBS_VALUE")]
colnames(youth_unemployment) <- c("region", "year", "sex", "youth_unempl_rate")
youth_unemployment <- subset(youth_unemployment, year<2018)

youth_unemployment <- dcast(youth_unemployment, region + year ~ sex, value.var = "youth_unempl_rate")
colnames(youth_unemployment) <- c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")

# remove rows with countries not of our interest
youth_unemployment$country <- sub('..$','',youth_unemployment$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(youth_unemployment$country))
youth_unemployment <- youth_unemployment[youth_unemployment$country %in% unique_countries, ]
length(unique(youth_unemployment$country))
youth_unemployment$country <- NULL

# compare regions between the dataframe and our list
regions_youth_unemployment <- youth_unemployment[,c("region", "year")]
regions_youth_unemployment$year <- NULL
regions_youth_unemployment <- unique(regions_youth_unemployment)
regions_youth_unemployment$df <- 1

temp_1 <- merge(regions_df_1, regions_youth_unemployment, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_youth_unemployment, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]

# do a linear interpolation before calculating means for split regions

colSums(is.na(youth_unemployment))

df <- data.frame()

for (region_ in unique(youth_unemployment$region)){
  temp <- subset(youth_unemployment, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

colSums(is.na(df))

youth_unemployment <- df
rm(df)



# HU11 + HU12 = HU10
case_temp <- subset(youth_unemployment, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# IE04 = IE01
case_temp <- subset(youth_unemployment, region=="IE04" & year>2011)
case_temp_neq <- setdiff(youth_unemployment, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(youth_unemployment, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(youth_unemployment, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(youth_unemployment, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(youth_unemployment, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(youth_unemployment, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(youth_unemployment, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# compare regions between the dataframe and our list
regions_edu <- youth_unemployment[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, youth_unemployment, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(youth_unemployment, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(youth_unemployment, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(youth_unemployment, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(youth_unemployment, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(youth_unemployment, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_unempl_rate_female, case_temp$youth_unempl_rate_male, case_temp$youth_unempl_rate_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_unempl_rate_female", "youth_unempl_rate_male", "youth_unempl_rate_tot")]

youth_unemployment <- rbind(case_temp, case_temp_neq)
youth_unemployment <- youth_unemployment[with(youth_unemployment, order(region, year)),]
row.names(youth_unemployment) <- NULL

# check region differences for 1997-2007
regions_edu <- youth_unemployment[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, youth_unemployment, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))










# 9. SHARE OF PEOPLE AGED 15-29 NOT IN EMPLOYMENT, EDUCATION, OR TRAINING

youth_nemp_nedu <- read.csv("../original_data/youth_nemp_nedu.csv")
youth_nemp_nedu <- youth_nemp_nedu[,c("geo", "TIME_PERIOD", "sex", "OBS_VALUE")]
colnames(youth_nemp_nedu) <- c("region", "year", "sex", "youth_nemp_nedu")
youth_nemp_nedu <- subset(youth_nemp_nedu, year<2018)


youth_nemp_nedu <- dcast(youth_nemp_nedu, region + year ~ sex, value.var = "youth_nemp_nedu")
colnames(youth_nemp_nedu) <- c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")

# remove rows with countries not of our interest
youth_nemp_nedu$country <- sub('..$','',youth_nemp_nedu$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(youth_nemp_nedu$country))
youth_nemp_nedu <- youth_nemp_nedu[youth_nemp_nedu$country %in% unique_countries, ]
length(unique(youth_nemp_nedu$country))
youth_nemp_nedu$country <- NULL

# compare regions between the dataframe and our list
regions_edu <- youth_nemp_nedu[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp_1 <- merge(regions_df_1, regions_edu, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_edu, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]


# HU11 + HU12 = HU10
case_temp <- subset(youth_nemp_nedu, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# IE04 = IE01
case_temp <- subset(youth_nemp_nedu, region=="IE04" & year>2011)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(youth_nemp_nedu, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(youth_nemp_nedu, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(youth_nemp_nedu, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(youth_nemp_nedu, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(youth_nemp_nedu, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(youth_nemp_nedu, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# compare regions between the dataframe and our list
regions_edu <- youth_nemp_nedu[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, youth_nemp_nedu, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(youth_nemp_nedu, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(youth_nemp_nedu, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(youth_nemp_nedu, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(youth_nemp_nedu, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(youth_nemp_nedu, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$youth_nemp_nedu_female, case_temp$youth_nemp_nedu_male, case_temp$youth_nemp_nedu_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "youth_nemp_nedu_female", "youth_nemp_nedu_male", "youth_nemp_nedu_tot")]

youth_nemp_nedu <- rbind(case_temp, case_temp_neq)
youth_nemp_nedu <- youth_nemp_nedu[with(youth_nemp_nedu, order(region, year)),]
row.names(youth_nemp_nedu) <- NULL

# check region differences for 1997-2007
regions_edu <- youth_nemp_nedu[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, youth_nemp_nedu, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))









# 10. GDP PER CAPITA (CURRENT MARKET PRICES)

gdp <- read.csv("../original_data/gdp_per_capita_current_prices.csv")
gdp <- gdp[,c("geo", "TIME_PERIOD", "OBS_VALUE")]
colnames(gdp) <- c("region", "year", "gdp_per_capita")
gdp <- subset(gdp, year<2018)

# remove rows with countries not of our interest
gdp$country <- sub('..$','',gdp$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(gdp$country))
gdp <- gdp[gdp$country %in% unique_countries, ]
length(unique(gdp$country))
gdp$country <- NULL

# compare regions between the dataframe and our list
regions_gdp <- gdp[,c("region", "year")]
regions_gdp$year <- NULL
regions_gdp <- unique(regions_gdp)
regions_gdp$df <- 1

temp_1 <- merge(regions_df_1, regions_gdp, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_gdp, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]



# HU11 + HU12 = HU10
case_temp <- subset(gdp, (region=="HU11" | region=="HU12"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# IE04 = IE01
case_temp <- subset(gdp, region=="IE04")
case_temp_neq <- setdiff(gdp, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(gdp, (region=="IE05" | region=="IE06"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(gdp, (region=="LT01" | region=="LT02"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(gdp, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# compare regions between the dataframe and our list
regions_edu <- gdp[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, gdp, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(gdp, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(gdp, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(gdp, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(gdp, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(gdp, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$gdp_per_capita), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "gdp_per_capita")]

gdp <- rbind(case_temp, case_temp_neq)
gdp <- gdp[with(gdp, order(region, year)),]
row.names(gdp) <- NULL

# check region differences for 1997-2007
regions_edu <- gdp[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, gdp, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))

# calculate gdp per capita growth rate

df_2007_2017 <- ddply(df_2007_2017,"region",transform,
                      gdp_per_capita_growth_rate=c(NA,exp(diff(log(gdp_per_capita)))-1))

df_1997_2017 <- ddply(df_1997_2017,"region",transform,
                      gdp_per_capita_growth_rate=c(NA,exp(diff(log(gdp_per_capita)))-1))









# 11. SHARE OF EMPLOYED IN TECHNOLOGY AND KNOWLEDGE INTENSIVE SECTORS


empl_techn_1999_2008 <- read.csv("../original_data/empl_techn_1999_2008.csv")
empl_techn_1999_2008 <- empl_techn_1999_2008[,c("geo", "TIME_PERIOD", "sex", "OBS_VALUE")]
empl_techn_1999_2008 <- dcast(empl_techn_1999_2008, geo + TIME_PERIOD ~ sex, value.var = "OBS_VALUE")
empl_techn_1999_2008 <- subset(empl_techn_1999_2008, TIME_PERIOD!=2008)

empl_techn_2008_2021 <- read.csv("../original_data/empl_techn_2008_2021.csv")
empl_techn_2008_2021 <- empl_techn_2008_2021[,c("geo", "TIME_PERIOD", "sex", "OBS_VALUE")]
empl_techn_2008_2021 <- dcast(empl_techn_2008_2021, geo + TIME_PERIOD ~ sex, value.var = "OBS_VALUE")
empl_techn_2008_2021 <- subset(empl_techn_2008_2021, TIME_PERIOD<2018)

empl_techn <- rbind(empl_techn_1999_2008, empl_techn_2008_2021)
empl_techn <- empl_techn[with(empl_techn, order(geo, TIME_PERIOD)),]
row.names(empl_techn) <- NULL

rm(empl_techn_1999_2008, empl_techn_2008_2021)
colnames(empl_techn) <- c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")


# remove rows with countries not of our interest
empl_techn$country <- sub('..$','',empl_techn$region)
unique_countries <- c("IE", "UK",
                      "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
                      "BE", "FR", "NL",
                      "AT", "CH", "DE",
                      "DK", "FI", "NO", "SE",
                      "EL", "ES", "IT", "PT", "MT")
length(unique_countries)
length(unique(empl_techn$country))
empl_techn <- empl_techn[empl_techn$country %in% unique_countries, ]
length(unique(empl_techn$country))
empl_techn$country <- NULL

# compare regions between the dataframe and our list
regions_edu <- empl_techn[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp_1 <- merge(regions_df_1, regions_edu, by="region", all = T)
temp_1 <- temp_1[rowSums(is.na(temp_1)) > 0,]

temp_2 <- merge(regions_df_2, regions_edu, by="region", all = T)
temp_2 <- temp_2[rowSums(is.na(temp_2)) > 0,]



# HU11 + HU12 = HU10
case_temp <- subset(empl_techn, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "HU10"
case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="HU10" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="HU11" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="HU12" & year<2013))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# IE04 = IE01
case_temp <- subset(empl_techn, region=="IE04" & year>2011)
case_temp_neq <- setdiff(empl_techn, case_temp)

case_temp$region <- str_replace_all(case_temp$region, "IE04", "IE01")
case_temp_neq <- subset(case_temp_neq, !(region=="IE01" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE04" & year<2012))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# IE05 + IE06 = IE02
case_temp <- subset(empl_techn, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IE02"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="IE02" & year>2011))
case_temp_neq <- subset(case_temp_neq, !(region=="IE05" & year<2012))
case_temp_neq <- subset(case_temp_neq, !(region=="IE06" & year<2012))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# LT01 + LT02 = LT00
case_temp <- subset(empl_techn, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "LT00"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="LT00" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="LT01" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="LT02" & year<2013))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# PL91 + PL92 = PL12
case_temp <- subset(empl_techn, region=="PL91" | region=="PL92")
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "PL12"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# UKM8 + UKM9 = UKM3
case_temp <- subset(empl_techn, (region=="UKM8" | region=="UKM9") & year>2012)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKM3"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKM3" & year>2012))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM8" & year<2013))
case_temp_neq <- subset(case_temp_neq, !(region=="UKM9" & year<2013))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# UKI3 + UKI4 = UKI1
case_temp <- subset(empl_techn, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI1"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI1" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI3" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI4" & year<2010))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# UKI5 + UKI6 + UKI7 = UKI2
case_temp <- subset(empl_techn, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "UKI2"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

case_temp_neq <- subset(case_temp_neq, !(region=="UKI2" & year>2009))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI5" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI6" & year<2010))
case_temp_neq <- subset(case_temp_neq, !(region=="UKI7" & year<2010))

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# compare regions between the dataframe and our list
regions_edu <- empl_techn[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_2, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# all regions are fine

# check for 1997-2007
temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

# merge with the file for 2007-2017
df_2007_2017 <- merge(df_2007_2017, empl_techn, by=c("region", "year"), all.x = T)

# recode additional regions for the file 1997-2007

# DEB1 + DEB2 + DEB3 = DEB0
case_temp <- subset(empl_techn, (region=="DEB1" | region=="DEB2" | region=="DEB3"))
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# DED2 + DED4 + DED5 = DED0
case_temp <- subset(empl_techn, (region=="DED2" | region=="DED4" | region=="DED5"))
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# FI1B + FI1C = FI18
case_temp <- subset(empl_techn, (region=="FI1B" | region=="FI1C"))
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# ITH1 + ITH2 = IT31
case_temp <- subset(empl_techn, (region=="ITH1" | region=="ITH2"))
case_temp_neq <- setdiff(empl_techn, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(list(case_temp$empl_techn_female, case_temp$empl_techn_male, case_temp$empl_techn_tot), by = list(case_temp$year), mean)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp <- case_temp[,c("region", "year", "empl_techn_female", "empl_techn_male", "empl_techn_tot")]

empl_techn <- rbind(case_temp, case_temp_neq)
empl_techn <- empl_techn[with(empl_techn, order(region, year)),]
row.names(empl_techn) <- NULL

# check region differences for 1997-2007
regions_edu <- empl_techn[,c("region", "year")]
regions_edu$year <- NULL
regions_edu <- unique(regions_edu)
regions_edu$df <- 1

temp <- merge(regions_df_1, regions_edu, by="region", all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]
# only non-pioneer countries

df_1997_2017 <- merge(df_1997_2017, empl_techn, by=c("region", "year"), all.x = T)

rm(list=setdiff(ls(), c("df_1997_2017", "df_2007_2017", "regions_df_1", "regions_df_2")))










################## DO A LINEAR INTERPOLATION OF MISSING VALUES ################## 

# pioneers 1997-2017

colSums(is.na(df_1997_2017))

df <- data.frame()

for (region_ in unique(df_1997_2017$region)){
  temp <- subset(df_1997_2017, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

colSums(is.na(df))

df_1997_2017 <- df

na_1 <- df_1997_2017[rowSums(is.na(df_1997_2017)) > 0,]

# nonpioneers 2007-2017

colSums(is.na(df_2007_2017))

df <- data.frame()

for (region_ in unique(df_2007_2017$region)){
  temp <- subset(df_2007_2017, region==region_)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df <- rbind(df, temp)
}

colSums(is.na(df))

df_2007_2017 <- df

na_2 <- df_2007_2017[rowSums(is.na(df_2007_2017)) > 0,]

df_1997_2017$country <- sub('..$','',df_1997_2017$region)
df_2007_2017$country <- sub('..$','',df_2007_2017$region)

df_2007_2017$female_industry <- df_2007_2017$female_industry*100
df_2007_2017$female_service <- df_2007_2017$female_service*100
df_2007_2017$male_industry <- df_2007_2017$male_industry*100
df_2007_2017$male_service <- df_2007_2017$male_service*100
df_2007_2017$all_industry <- df_2007_2017$all_industry*100
df_2007_2017$all_service <- df_2007_2017$all_service*100

df_1997_2017$female_industry <- df_1997_2017$female_industry*100
df_1997_2017$female_service <- df_1997_2017$female_service*100
df_1997_2017$male_industry <- df_1997_2017$male_industry*100
df_1997_2017$male_service <- df_1997_2017$male_service*100
df_1997_2017$all_industry <- df_1997_2017$all_industry*100
df_1997_2017$all_service <- df_1997_2017$all_service*100

unique(df_2007_2017$country)

# df_2007_2017 <- df_2007_2017 %>% mutate(
#   country_group = case_when(
#     country=="AT" | country=="CH" | country=="IE"  | country=="DK" | country=="BE" | country=="NL" | country=="PT" | country=="MT" | country=="EL" ~ "western",
#     country=="CZ" | country=="HU" | country=="PL" | country=="SK" | country=="BG" | country=="EE" | country=="LT" | country=="LV" | country=="RO" ~ "cee",
#   )
# )

save(df_1997_2017, file = "../generated_data/pioneer_1997_2017_modelling.RData")
save(df_2007_2017, file = "../generated_data/nonpioneer_2007_2017_modelling.RData")

write.dta(df_1997_2017, file="../final_data/pioneer_1997_2017_modelling.dta")
write.dta(df_2007_2017, file="../final_data/nonpioneer_2007_2017_modelling.dta")

df <- rbind(df_1997_2017, df_2007_2017)
df <- df[with(df, order(region, year)),]
row.names(df) <- NULL

save(df, file = "../generated_data/all_modelling.RData")
write.dta(df, file="../final_data/all_modelling.dta")