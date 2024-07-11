# # Copyright (c) 2023 Honorata Bogusz
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

requiredPackages = c("ggplot2", "dplyr", "reshape2", "haven", "stringr",
                   "viridisLite", "viridis", "zoo", "imputeTS", "readxl",
                   "svglite", "janitor", "ggcorrplot", "corrplot", "showtext")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
gc()
}

### 1. PREPARE THE PLOT OF ROBOT PENETRATION IN 'HEAVY' INDUSTRIES

# read the IFR dataset

ifr <- read.csv2("../original_data/ifr.csv")
ifr <- ifr[,c("Year", "industry", "Industry", "country", "Operational.stock")]
colnames(ifr) <- c("year", "isic", "isic_decr", "country", "robots")

ifr <- subset(ifr, country=="DE" | country=="FR" | country=="UK" | country=="IT" | country=="PL" | country=="CZ")
unique(ifr$country)

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

ifr <- aggregate(ifr$robots, by=list(country=ifr$country, year=ifr$year), FUN=sum)
ifr <- ifr[with(ifr, order(country, year)),]
row.names(ifr) <- NULL
colnames(ifr)[3] <- "robots"

# read the employment data

empl <- read.csv("../original_data/employees.csv")
colnames(empl)

empl <- empl[,c("geo", "TIME_PERIOD", "nace_r2", "OBS_VALUE")]
colnames(empl) <- c("country", "year", "industry", "empl")

empl <- aggregate(empl$empl, by=list(country=empl$country, year=empl$year), FUN=sum)
empl <- empl[with(empl, order(country, year)),]
row.names(empl) <- NULL
colnames(empl)[3] <- "empl"

df <- merge(empl, ifr, by = c("country", "year"), all = T)

df_new <- data.frame()

for (c in unique(df$country)){
  temp <- subset(df, country==c)
  for(i in 3:ncol(temp)){
    if (!((colSums(is.na(temp)))[[i]]==nrow(temp) | (colSums(is.na(temp)))[[i]]==nrow(temp)-1)){
      temp[[i]] <- na_interpolation(temp[[i]], option = "linear")
    }
  }
  df_new <- rbind(df_new, temp)
}

df <- subset(df_new, year>1992 & year<2018)

df$country <- str_replace_all(df$country, "DE", "Germany")
df$country <- str_replace_all(df$country, "FR", "France")
df$country <- str_replace_all(df$country, "UK", "United Kingdom")
df$country <- str_replace_all(df$country, "IT", "Italy")
df$country <- str_replace_all(df$country, "PL", "Poland")
df$country <- str_replace_all(df$country, "CZ", "Czechia")

df$empl <- df$empl/10
df$robots_10000workers <- df$robots / df$empl
df_apr_manuf <- df

### 2. UNEMPLOYMENT

rm(list=setdiff(ls(), "df_apr_manuf"))

# load unemployment data

unempl <- read.csv("../original_data/unempl.csv", header=FALSE, comment.char="#")
unempl <- unempl[3:269,]
unempl <- unempl[,c(1, 38:65)]

unempl <- unempl %>%
  row_to_names(row_number = 1)

colnames(unempl)[1] <- "country"
unique(unempl$country)

unempl <- unempl %>%
  filter(country == "Czechia" | country=="France" | country == "Germany" | 
           country=="Italy" | country=="Poland" | country=="United Kingdom")

unempl <- reshape(unempl, 
                  direction = "long",
                  varying = list(names(unempl)[2:29]),
                  v.names = "unempl",
                  idvar = c("country"),
                  timevar = "year",
                  times = 1993:2020)
row.names(unempl) <- NULL

# merge with the apr dataset

df_apr_manuf <- merge(df_apr_manuf, unempl, by = c("country", "year"), all.x = T)

rm(list=setdiff(ls(), "df_apr_manuf"))

### 3. SEPARATE PLOTS WITH 2 AXES FOR ROBOT PENETRATION AND UNEMPL

rm(list=setdiff(ls(), "df_apr_manuf"))

df_apr_manuf <- df_apr_manuf[,c("country", "year", "robots_10000workers", "unempl")]
write_dta(df_apr_manuf, "../generated_data/df_apr_manuf_unempl.dta")