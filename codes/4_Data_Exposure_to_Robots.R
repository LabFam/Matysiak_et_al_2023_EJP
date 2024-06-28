# # Copyright (c) 2022 Honorata Bogusz
# 
# # Permission is hereby granted, free of charge, to any person obtaining a copy
# # of this software and associated documentation files (the "Software"), to deal
# # in the Software without restriction, including without limitation the rights
# # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# # copies of the Software, and to permit persons to whom the Software is
# # furnished to do so, subject to the following conditions:
# 
# # The above copyright notice and this permission notice shall be included in all
# # copies or substantial portions of the Software.
# 
# # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# # IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# # FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# # AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# # LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# # OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# # SOFTWARE.

{ 
rm(list=ls())
gc()
options(timeout = 600)
options(scipen=999)
options(digits=2)

requiredPackages = c("splitstackshape", "tidyverse", "dplyr", "reshape2", "haven", 
                     "stringr", "viridisLite", "gridExtra", "lmreg", "mice","Hmisc",
                     "ggrepel", "viridis", "foreign", "bnstruct", "outliers","forecast",
                     "TSstudio", "zoo", "factoextra", "RStata", "plyr", "zoo", "imputeTS",
                     "readxl")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
}

# read the IFR dataset

ifr <- read.csv2("../original_data/ifr.csv")
ifr <- ifr[,c("Year", "industry", "Industry", "country", "Operational.stock")]
colnames(ifr) <- c("year", "isic", "isic_decr", "country", "robots")

# replace GR with EL

ifr$country <- str_replace_all(ifr$country, "GR", "EL")

# reclassify industries

ifr <- ifr %>% mutate(
  industry = case_when(
    isic=="91" | isic=="20-21" ~ "All other manufacturing branches/other chemical products n.e.c",

    isic=="29" | isic=="30" ~ "Automotive/Other vehicles",

    isic=="24" ~ "Basic metals",

    isic=="F" ~ "Construction",

    isic=="26-27" ~ "Electrical/electronics",

    isic=="E" ~ "Electricity, gas, water supply",

    isic=="10-12" ~ "Food and beverages",

    isic=="23" ~ "Glass, ceramics, stone, mineral products (non-automotive)",

    isic=="28" ~ "Industrial machinery",

    isic=="25" ~ "Metal products (non-automotive)",

    isic=="C" ~ "Mining and quarrying",

    isic=="17-18" ~ "Paper",

    isic=="19" ~ "Pharmaceuticals, cosmetics",

    isic=="22" ~ "Rubber and plastic products (non-automotive)",

    isic=="13-15" ~ "Textiles",

    isic=="16" ~ "Wood and furniture"
  )
)

ifr <- ifr[,c("country", "year", "industry", "robots")]
ifr <- ifr[!is.na(ifr$industry),]
ifr <- ifr[with(ifr, order(country, industry, year)),]
row.names(ifr) <- NULL

ifr <- aggregate(ifr$robots, by=list(country=ifr$country, year=ifr$year, industry=ifr$industry), FUN=sum)
ifr <- ifr[with(ifr, order(country, industry, year)),]
row.names(ifr) <- NULL
colnames(ifr)[4] <- "robots"

df <- data.frame()
for (country_ in unique(ifr$country)){
  for (industry_ in unique(ifr$industry)){
    temp <- subset(ifr, country==country_ & industry==industry_)
    temp$robots_l1 <- Lag(temp$robots, 1)
    temp$robots_l2 <- Lag(temp$robots, 2)
    df <- rbind(df, temp)
  }
}

ifr <- df
save(ifr, file="../generated_data/ifr_industries_reclassified.RData")

rm(list=ls())
gc()
load("../generated_data/ifr_industries_reclassified.RData")

# keep only countries of our interest
length(unique(ifr$country))
countries <- c("IE", "UK", 
               "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
               "BE", "FR", "NL", 
               "AT", "CH", "DE",
               "DK", "FI", "NO", "SE",
               "EL", "ES", "IT", "PT", "MT")
robots <- ifr[ifr$country %in% countries,]
length(unique(robots$country))

##### 1. CALCULATE EXPOSURE TO ROBOTS AND INSTRUMENT FOR NONPIONEERS 2007-2017 DATASET #####

robots <- subset(robots, year>1996 & year<2018)
unique(robots$year)
robots$robots <- NULL
robots$robots_l1 <- NULL

instrument <- subset(ifr, year>1996 & year<2018)
unique(instrument$year)
instrument$robots <- NULL
instrument$robots_l1 <- NULL
colnames(instrument)[4] <- "robots_l2_instr"

rm(ifr, countries)

# 9 instruments

us <- subset(instrument, country=="US")
us <- aggregate(us$robots_l2_instr, by=list(year=us$year, industry=us$industry), FUN=mean)
colnames(us)[3] <- "robots_l2_instr_us"
robots <- merge(robots, us, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

uk <- subset(instrument, country=="UK")
uk <- aggregate(uk$robots_l2_instr, by=list(year=uk$year, industry=uk$industry), FUN=mean)
colnames(uk)[3] <- "robots_l2_instr_uk"
robots <- merge(robots, uk, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

de <- subset(instrument, country=="DE")
de <- aggregate(de$robots_l2_instr, by=list(year=de$year, industry=de$industry), FUN=mean)
colnames(de)[3] <- "robots_l2_instr_de"
robots <- merge(robots, de, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

fr <- subset(instrument, country=="FR")
fr <- aggregate(fr$robots_l2_instr, by=list(year=fr$year, industry=fr$industry), FUN=mean)
colnames(fr)[3] <- "robots_l2_instr_fr"
robots <- merge(robots, fr, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

es <- subset(instrument, country=="ES")
es <- aggregate(es$robots_l2_instr, by=list(year=es$year, industry=es$industry), FUN=mean)
colnames(es)[3] <- "robots_l2_instr_es"
robots <- merge(robots, es, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

it <- subset(instrument, country=="IT")
it <- aggregate(it$robots_l2_instr, by=list(year=it$year, industry=it$industry), FUN=mean)
colnames(it)[3] <- "robots_l2_instr_it"
robots <- merge(robots, it, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

no <- subset(instrument, country=="NO")
no <- aggregate(no$robots_l2_instr, by=list(year=no$year, industry=no$industry), FUN=mean)
colnames(no)[3] <- "robots_l2_instr_no"
robots <- merge(robots, no, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

se <- subset(instrument, country=="SE")
se <- aggregate(se$robots_l2_instr, by=list(year=se$year, industry=se$industry), FUN=mean)
colnames(se)[3] <- "robots_l2_instr_se"
robots <- merge(robots, se, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no", "robots_l2_instr_se")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

fi <- subset(instrument, country=="FI")
fi <- aggregate(fi$robots_l2_instr, by=list(year=fi$year, industry=fi$industry), FUN=mean)
colnames(fi)[3] <- "robots_l2_instr_fi"
robots <- merge(robots, fi, by = c("industry", "year"), all.x = T)

robots <- subset(robots, year>2006)
robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no", "robots_l2_instr_se", "robots_l2_instr_fi")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

# load employment data nonpioneer 2007-2017

load("../generated_data/non_pioneers_2007_2017.RData")
empl <- non_pioneers_2007_2017
rm(list=setdiff(ls(), c("robots", "empl")))
empl$employment_2004 <- empl$employment_2004/10

empl_t0 <- subset(empl, year==2007)
empl_t0$year <- NULL

emp_r_t0 <- aggregate(empl_t0$employment_2004, by=list(country=empl_t0$country, region=empl_t0$region), FUN=sum)
colnames(emp_r_t0)[3] <- "emp_r_t0"
emp_r_t0$country <- NULL

emp_i_t0 <- aggregate(empl_t0$employment_2004, by=list(country=empl_t0$country, industry=empl_t0$industry), FUN=sum)
colnames(emp_i_t0)[3] <- "emp_i_t0"

rm(empl_t0)

empl <- merge(empl, emp_r_t0, by="region", all.x = T)
empl <- merge(empl, emp_i_t0, by=c("country", "industry"),all.x=T)

colnames(empl)[5] <- "emp_r_i_t0" 

rm(emp_r_t0, emp_i_t0)

exposure_to_robots <- merge(empl, robots, by=c("country", "industry", "year"), all.x = T)
exposure_to_robots[is.na(exposure_to_robots),]
rm(list=setdiff(ls(), c("exposure_to_robots")))

# unique(exposure_to_robots$industry)
# exposure_to_robots <- subset(exposure_to_robots, industry!="Automotive/Other vehicles")

length(unique(exposure_to_robots$country))
exposure_to_robots <- exposure_to_robots[with(exposure_to_robots, order(region, industry, year)),]
row.names(exposure_to_robots) <- NULL

exposure_to_robots$exposure_to_robots <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_us <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_us/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_uk <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_uk/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_de <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_de/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_fr <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_fr/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_es <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_es/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_it <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_it/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_no <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_no/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_se <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_se/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_fi <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_fi/exposure_to_robots$emp_i_t0)

etr <- aggregate(exposure_to_robots$exposure_to_robots, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_us <- aggregate(exposure_to_robots$instrument_us, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_uk <- aggregate(exposure_to_robots$instrument_uk, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_de <- aggregate(exposure_to_robots$instrument_de, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_fr <- aggregate(exposure_to_robots$instrument_fr, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_es <- aggregate(exposure_to_robots$instrument_es, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_it <- aggregate(exposure_to_robots$instrument_it, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_no <- aggregate(exposure_to_robots$instrument_no, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_se <- aggregate(exposure_to_robots$instrument_se, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_fi <- aggregate(exposure_to_robots$instrument_fi, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)

colnames(etr)[3] <- "exposure_to_robots"
colnames(instrument_us)[3] <- "instrument_us"
colnames(instrument_uk)[3] <- "instrument_uk"
colnames(instrument_de)[3] <- "instrument_de"
colnames(instrument_fr)[3] <- "instrument_fr"
colnames(instrument_es)[3] <- "instrument_es"
colnames(instrument_it)[3] <- "instrument_it"
colnames(instrument_no)[3] <- "instrument_no"
colnames(instrument_se)[3] <- "instrument_se"
colnames(instrument_fi)[3] <- "instrument_fi"

exposure_to_robots <- merge(etr, instrument_us, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_uk, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_de, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_fr, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_es, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_it, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_no, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_se, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_fi, by=c("region", "year"))

hist(exposure_to_robots$exposure_to_robots)
save(exposure_to_robots, file="../generated_data/etr_nonpioneer_2007_2017.RData")

##### 2. CALCULATE EXPOSURE TO ROBOTS AND INSTRUMENT FOR 1997-2007 DATASET #####

rm(list=ls())
gc()
load("../generated_data/ifr_industries_reclassified.RData")

# keep only countries of our interest
length(unique(ifr$country))
countries <- c("DE", "ES", "FI", "FR", "IT", "NO", "SE", "UK")
robots <- ifr[ifr$country %in% countries,]
length(unique(robots$country))

robots <- subset(robots, year>1996 & year<2018)
unique(robots$year)
robots$robots <- NULL
robots$robots_l1 <- NULL

instrument <- subset(ifr, year>1996 & year<2018)
unique(instrument$year)
instrument$robots <- NULL
instrument$robots_l1 <- NULL
colnames(instrument)[4] <- "robots_l2_instr"

# build 8 instruments

us <- subset(instrument, country=="US")
us <- aggregate(us$robots_l2_instr, by=list(year=us$year, industry=us$industry), FUN=mean)
colnames(us)[3] <- "robots_l2_instr_us"
robots <- merge(robots, us, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

uk <- subset(instrument, country=="UK")
uk <- aggregate(uk$robots_l2_instr, by=list(year=uk$year, industry=uk$industry), FUN=mean)
colnames(uk)[3] <- "robots_l2_instr_uk"
robots <- merge(robots, uk, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

de <- subset(instrument, country=="DE")
de <- aggregate(de$robots_l2_instr, by=list(year=de$year, industry=de$industry), FUN=mean)
colnames(de)[3] <- "robots_l2_instr_de"
robots <- merge(robots, de, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

fr <- subset(instrument, country=="FR")
fr <- aggregate(fr$robots_l2_instr, by=list(year=fr$year, industry=fr$industry), FUN=mean)
colnames(fr)[3] <- "robots_l2_instr_fr"
robots <- merge(robots, fr, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

es <- subset(instrument, country=="ES")
es <- aggregate(es$robots_l2_instr, by=list(year=es$year, industry=es$industry), FUN=mean)
colnames(es)[3] <- "robots_l2_instr_es"
robots <- merge(robots, es, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

it <- subset(instrument, country=="IT")
it <- aggregate(it$robots_l2_instr, by=list(year=it$year, industry=it$industry), FUN=mean)
colnames(it)[3] <- "robots_l2_instr_it"
robots <- merge(robots, it, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

no <- subset(instrument, country=="NO")
no <- aggregate(no$robots_l2_instr, by=list(year=no$year, industry=no$industry), FUN=mean)
colnames(no)[3] <- "robots_l2_instr_no"
robots <- merge(robots, no, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

se <- subset(instrument, country=="SE")
se <- aggregate(se$robots_l2_instr, by=list(year=se$year, industry=se$industry), FUN=mean)
colnames(se)[3] <- "robots_l2_instr_se"
robots <- merge(robots, se, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no", "robots_l2_instr_se")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

fi <- subset(instrument, country=="FI")
fi <- aggregate(fi$robots_l2_instr, by=list(year=fi$year, industry=fi$industry), FUN=mean)
colnames(fi)[3] <- "robots_l2_instr_fi"
robots <- merge(robots, fi, by = c("industry", "year"), all.x = T)

robots[is.na(robots),]
robots <- robots[,c("country", "year", "industry", "robots_l2", "robots_l2_instr_us", "robots_l2_instr_uk",
                    "robots_l2_instr_de", "robots_l2_instr_fr", "robots_l2_instr_es", "robots_l2_instr_it",
                    "robots_l2_instr_no", "robots_l2_instr_se", "robots_l2_instr_fi")]
robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL

# germany <- subset(instrument, country=="DE" | country=="KR" | country=="UK")
# countries <- unique(germany$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- germany %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# germany <- df
# rm(df)
# germany <- subset(germany, country=="DE")
# 
# # uk <- subset(instrument, country=="UK" | country=="AU" | country=="US")
# # countries <- unique(uk$country)
# # df <- data.frame()
# # for (country_ in countries){
# #   temp <- robots %>% filter(country==country_)
# #   instr <- uk %>% filter(country!=country_)
# #   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
# #   colnames(instr_1)[3] <- "robots_l2_instr"
# #   temp <- merge(temp, instr_1, by=c("year", "industry"))
# #   df <- rbind(df, temp)
# # }
# # uk <- df
# # rm(df)
# # uk <- subset(uk, country=="UK")
# 
# spain <- subset(instrument, country=="ES" | country=="FR" | country=="UK")
# countries <- unique(spain$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- spain %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# spain <- df
# rm(df)
# spain <- subset(spain, country=="ES")
# 
# italy <- subset(instrument, country=="DE" | country=="IT" | country=="FR" | country=="UK" | country=="FI" | country=="NO" | country=="SE")
# #italy <- subset(instrument, country=="IT" | country=="FR" | country=="UK")
# countries <- unique(italy$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- italy %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# italy <- df
# rm(df)
# italy <- subset(italy, country=="IT")
# 
# norway <- subset(instrument, country=="NO" | country=="FI" | country=="US")
# countries <- unique(norway$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- norway %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# norway <- df
# rm(df)
# norway <- subset(norway, country=="NO")
# 
# sweden <- subset(instrument, country=="SE" | country=="FI" | country=="US")
# countries <- unique(sweden$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- sweden %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# sweden <- df
# rm(df)
# sweden <- subset(sweden, country=="SE")
# 
# finland <- subset(instrument, country=="FI" | country=="US" | country=="NO")
# countries <- unique(finland$country)
# df <- data.frame()
# for (country_ in countries){
#   temp <- robots %>% filter(country==country_)
#   instr <- finland %>% filter(country!=country_)
#   instr_1 <- aggregate(instr$robots_l2_instr, by=list(year=instr$year, industry=instr$industry), FUN=mean)
#   colnames(instr_1)[3] <- "robots_l2_instr"
#   temp <- merge(temp, instr_1, by=c("year", "industry"))
#   df <- rbind(df, temp)
# }
# finland <- df
# rm(df)
# finland <- subset(finland, country=="FI")
# 
# robots <- rbind(pioneers, germany, spain, italy, norway, sweden, finland)

robots <- robots[with(robots, order(country, industry, year)),]
row.names(robots) <- NULL
rm(list=setdiff(ls(), c("robots")))

# load employment data pioneers 1997-2017

load("../generated_data/pioneers_1997_2017.RData")
empl <- pioneers_1997_2017
rm(list=setdiff(ls(), c("robots", "empl")))
empl$employment_1994 <- empl$employment_1994/10

empl_t0 <- subset(empl, year==1997)
empl_t0$year <- NULL

emp_r_t0 <- aggregate(empl_t0$employment_1994, by=list(country=empl_t0$country, region=empl_t0$region), FUN=sum)
colnames(emp_r_t0)[3] <- "emp_r_t0"
emp_r_t0$country <- NULL

emp_i_t0 <- aggregate(empl_t0$employment_1994, by=list(country=empl_t0$country, industry=empl_t0$industry), FUN=sum)
colnames(emp_i_t0)[3] <- "emp_i_t0"

rm(empl_t0)

empl <- merge(empl, emp_r_t0, by="region", all.x = T)
empl <- merge(empl, emp_i_t0, by=c("country", "industry"),all.x=T)

colnames(empl)[5] <- "emp_r_i_t0" 

rm(emp_r_t0, emp_i_t0)

exposure_to_robots <- merge(empl, robots, by=c("country", "industry", "year"), all.x = T)
exposure_to_robots[is.na(exposure_to_robots),]
rm(list=setdiff(ls(), c("exposure_to_robots")))

# # Exclude specific industry
# unique(exposure_to_robots$industry)
# exposure_to_robots <- subset(exposure_to_robots, !(country=="DE" & industry=="Automotive/Other vehicles"))
# exposure_to_robots <- subset(exposure_to_robots, !(country=="FR" & industry=="Automotive/Other vehicles"))
# exposure_to_robots <- subset(exposure_to_robots, !(country=="UK" & industry=="Automotive/Other vehicles"))
# exposure_to_robots <- subset(exposure_to_robots, !(country=="IT" & industry=="Metal products (non-automotive)"))
# exposure_to_robots <- subset(exposure_to_robots, !(country=="NO" & industry=="Electrical/electronics"))
# exposure_to_robots <- subset(exposure_to_robots, !(country=="SE" & industry=="Electrical/electronics"))

length(unique(exposure_to_robots$country))
exposure_to_robots <- exposure_to_robots[with(exposure_to_robots, order(region, industry, year)),]
row.names(exposure_to_robots) <- NULL

exposure_to_robots$exposure_to_robots <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_us <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_us/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_uk <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_uk/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_de <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_de/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_fr <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_fr/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_es <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_es/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_it <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_it/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_no <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_no/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_se <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_se/exposure_to_robots$emp_i_t0)

exposure_to_robots$instrument_fi <- (exposure_to_robots$emp_r_i_t0/exposure_to_robots$emp_r_t0)*(exposure_to_robots$robots_l2_instr_fi/exposure_to_robots$emp_i_t0)

etr <- aggregate(exposure_to_robots$exposure_to_robots, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_us <- aggregate(exposure_to_robots$instrument_us, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_uk <- aggregate(exposure_to_robots$instrument_uk, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_de <- aggregate(exposure_to_robots$instrument_de, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_fr <- aggregate(exposure_to_robots$instrument_fr, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_es <- aggregate(exposure_to_robots$instrument_es, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_it <- aggregate(exposure_to_robots$instrument_it, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_no <- aggregate(exposure_to_robots$instrument_no, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_se <- aggregate(exposure_to_robots$instrument_se, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)
instrument_fi <- aggregate(exposure_to_robots$instrument_fi, by=list(region=exposure_to_robots$region, year=exposure_to_robots$year), FUN=sum)

colnames(etr)[3] <- "exposure_to_robots"
colnames(instrument_us)[3] <- "instrument_us"
colnames(instrument_uk)[3] <- "instrument_uk"
colnames(instrument_de)[3] <- "instrument_de"
colnames(instrument_fr)[3] <- "instrument_fr"
colnames(instrument_es)[3] <- "instrument_es"
colnames(instrument_it)[3] <- "instrument_it"
colnames(instrument_no)[3] <- "instrument_no"
colnames(instrument_se)[3] <- "instrument_se"
colnames(instrument_fi)[3] <- "instrument_fi"

exposure_to_robots <- merge(etr, instrument_us, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_uk, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_de, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_fr, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_es, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_it, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_no, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_se, by=c("region", "year"))
exposure_to_robots <- merge(exposure_to_robots, instrument_fi, by=c("region", "year"))

hist(exposure_to_robots$exposure_to_robots)
save(exposure_to_robots, file="../generated_data/etr_pioneer_1997_2017.RData")
