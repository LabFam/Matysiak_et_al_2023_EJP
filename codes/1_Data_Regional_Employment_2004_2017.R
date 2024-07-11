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

requiredPackages = c("splitstackshape", "tidyverse", "dplyr", "reshape2", "haven", 
                     "stringr", "viridisLite", "gridExtra", "lmreg", "mice","Hmisc",
                     "ggrepel", "viridis", "foreign", "bnstruct", "outliers","forecast",
                     "TSstudio", "zoo", "factoextra", "RStata", "plyr", "zoo", "imputeTS",
                     "readxl")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
}

# Gentleman's agreement: 1990-2003
# NUTS 2003: 2004-2007
# NUTS 2006: 2008-2011
# NUTS 2010: 2012-2014
# NUTS 2013: 2015-2017
# NUTS 2016: 2018-2020

############### 1. LOAD NUTS2 COUNTRY LISTS ############### 

##### 2016 #####

# load the list of 2016 NUTS2 codes
list_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx",  sheet = "NUTS2013-NUTS2016")
list_2016 <- list_2016[,2]
colnames(list_2016) <- "region"
list_2016 <- subset(list_2016, nchar(region)==4)
list_2016 <- list_2016[with(list_2016, order(region)),]

# exclude regions which are overseas
discarded_regions <- c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(list_2016$region)
`%ni%` <- Negate(`%in%`)
list_2016 <- list_2016[list_2016$region %ni% discarded_regions, ]
length(unique(list_2016$region))

list_2016$ri_list <- 1

##### 2013 #####

# load the list of 2013 NUTS2 codes
list_2013 <- read_excel("../original_data/NUTS/2010-2013.xls",  sheet = "NUTS2010-NUTS2013")
list_2013 <- list_2013[,2]
colnames(list_2013) <- "region"
list_2013 <- subset(list_2013, nchar(region)==4)
list_2013 <- list_2013[with(list_2013, order(region)),]

# exclude regions which are overseas
discarded_regions <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRA5", "PT20", "PT30", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(list_2013$region)
list_2013 <- list_2013[list_2013$region %ni% discarded_regions, ]
length(unique(list_2013$region))

list_2013$ri_list <- 1

##### 2010 #####

# load the list of 2010 NUTS2 codes
list_2010 <- read_excel("../original_data/NUTS/2006-2010.xls",  sheet = "NUTS2006-NUTS2010")
list_2010 <- list_2010[,3]
colnames(list_2010) <- "region"
list_2010 <- subset(list_2010, nchar(region)==4)
list_2010 <- list_2010[with(list_2010, order(region)),]

# exclude regions which are overseas
discarded_regions <- c("FR91", "FR92", "FR93", "FR94", "PT20", "PT30", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(list_2010$region)
list_2010 <- list_2010[list_2010$region %ni% discarded_regions, ]
length(unique(list_2010$region))

list_2010$ri_list <- 1

rm(list=setdiff(ls(), c("list_2016", "list_2013", "list_2010")))

############### 2. RECLASSIFY 2004-2017 YEARS FOR THE EMPLOYMENT DATA FILE 2000-2017 ############### 

df <- read_dta("../original_data/ESTA51546_200916_02.dta")
df <- df[,c("country", "region", "year", "na112d", "nace2d", "value")]
df$country <- str_replace_all(df$country, "GR", "EL")
df$r <- paste(df$country, df$region,sep="")
df <- df[,c("country", "r", "year", "na112d", "nace2d", "value")]
colnames(df)[2] <- "region"
df <- df[with(df, order(region, nace2d, year)),]
length(unique(df$region))
length(unique(df$country))

# discard countries not needed and aggregates
#### IMPORTANT: NO ROBOT DATA FOR IS, SI, HR, CY, LU --> DISCARD

unique(df$country)
countries <- c("IE", "UK", 
               "BG", "CZ", "EE", "HU", "LT", "LV", "PL", "RO", "SK",
               "BE", "FR", "NL", 
               "AT", "CH", "DE",
               "DK", "FI", "NO", "SE",
               "EL", "ES", "IT", "PT", "MT")

df <- df[df$country %in% countries,]
length(unique(df$country))

# exclude regions which are overseas
discarded_regions <- c("FRA1", "FRA2", "FRA3", "FRA4", "FRA5", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(unique(df$region))
`%ni%` <- Negate(`%in%`)
df <- df[df$region %ni% discarded_regions, ]
length(unique(df$region))

##### 2017 #####

df_2017 <- subset(df, year==2017)
length(unique(df_2017$region))
r_2017 <- df_2017[,c('region')]
r_2017 <- unique(r_2017)

r_2017$ri_df <- 1
temp_2017 <- merge(r_2017, list_2016, by="region", all=T)
# 2017 is classified in NUTS 2016

##### 2016 #####

df_2016 <- subset(df, year==2016)
length(unique(df_2016$region))
r_2016 <- df_2016[,c('region')]
r_2016 <- unique(r_2016)

r_2016$ri_df <- 1
temp_2016 <- merge(r_2016, list_2016, by="region", all=T)
# 2016 is classified in NUTS 2016

##### 2015 #####

df_2015 <- subset(df, year==2015)
length(unique(df_2015$region))
r_2015 <- df_2015[,c('region')]
r_2015 <- unique(r_2015)

r_2015$ri_df <- 1
temp_2015 <- merge(r_2015, list_2016, by="region", all=T)
# 2015 is classified in NUTS 2016

##### 2014 #####

df_2014 <- subset(df, year==2014)
length(unique(df_2014$region))
r_2014 <- df_2014[,c('region')]
r_2014 <- unique(r_2014)

r_2014$ri_df <- 1
temp_2014 <- merge(r_2014, list_2016, by="region", all=T)
# 2014 is classified in NUTS 2016

##### 2013 #####

df_2013 <- subset(df, year==2013)
length(unique(df_2013$region))
r_2013 <- df_2013[,c('region')]
r_2013 <- unique(r_2013)

r_2013$ri_df <- 1
temp_2013 <- merge(r_2013, list_2016, by="region", all=T)
# 2013 is classified in NUTS 2016

# merge years 2013-2017
df_2013_2017 <- rbind(df_2013, df_2014, df_2015, df_2016, df_2017)
rm(list=setdiff(ls(), c("list_2016", "list_2013", "list_2010", "df", "df_2013_2017")))

r_2013_2017 <- df_2013_2017[,c('region')]
r_2013_2017 <- unique(r_2013_2017)
r_2013_2017$ri_df <- 1

##### 2012 #####

df_2012 <- subset(df, year==2012)
length(unique(df_2012$region))
r_2012 <- df_2012[,c('region')]
r_2012 <- unique(r_2012)

r_2012$ri_df <- 1
temp_2012 <- merge(r_2012, list_2016, by="region", all=T)
# 2012 is probably classified in NUTS 2013
temp_2012 <- merge(r_2012, list_2013, by="region", all=T)
# yes, now I need to reclassify to NUTS 2016
temp_2012 <- merge(r_2012, r_2013_2017, by="region", all=T)
temp_2012 <- temp_2012[rowSums(is.na(temp_2012)) > 0,]

# regions which were reclassified in 2013/2016 were in the following countries: IE, FR, LT, HU, PL, UK
# IE is fine in df_2012, FR and part of PL straightforward (names changed)
# LT00, HU10, PL12, UKM3 (division in two regions)

# the straightforward regions
X2013_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx", sheet = "Correspondence NUTS-2")
X2013_2016 <- X2013_2016[,1:2]
colnames(X2013_2016) <- c("code_2013", "code_2016")
X2013_2016 <- X2013_2016[rowSums(is.na(X2013_2016)) == 0,]

# replace the 2013 names with the 2016 names
inds <- match(df_2012$region, X2013_2016$code_2013)
df_2012$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

r_2012_2 <- df_2012[,c('region')]
r_2012_2 <- unique(r_2012_2)
r_2012_2$ri_df <- 1
temp_2012 <- merge(r_2012_2, r_2013_2017, by="region", all=T)
temp_2012 <- temp_2012[rowSums(is.na(temp_2012)) > 0,]

##### 2010-2011 #####

df_2010_2011 <- subset(df, year==2010 | year==2011)
length(unique(df_2010_2011$region))
r_2010_2011 <- df_2010_2011[,c('region')]
r_2010_2011 <- unique(r_2010_2011)

r_2010_2011$ri_df <- 1
temp_2010_2011 <- merge(r_2010_2011, list_2016, by="region", all=T)
# 2011 is probably classified in NUTS 2013
temp_2010_2011 <- merge(r_2010_2011, list_2013, by="region", all=T)
# yes
temp_2010_2011 <- merge(r_2010_2011, r_2012, by="region", all=T)
# a difference with 2012 is that for 2011, IE is not corrected in the data
#I need to reclassify to NUTS 2016
temp_2010_2011 <- merge(r_2010_2011, r_2013_2017, by="region", all=T)
temp_2010_2011 <- temp_2010_2011[rowSums(is.na(temp_2010_2011)) > 0,]

# regions which were reclassified in 2013/2016 were in the following countries: IE, FR, LT, HU, PL, UK
# FR and part of PL straightforward (names changed)
# IE01, IE02, LT00, HU10, PL12, UKM3 (division in two regions)

# the straightforward regions
X2013_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx", sheet = "Correspondence NUTS-2")
X2013_2016 <- X2013_2016[,1:2]
colnames(X2013_2016) <- c("code_2013", "code_2016")
X2013_2016 <- X2013_2016[rowSums(is.na(X2013_2016)) == 0,]

# replace the 2013 names with the 2016 names
inds <- match(df_2010_2011$region, X2013_2016$code_2013)
df_2010_2011$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

temp_2010_2011 <- df_2010_2011[,c('region')]
temp_2010_2011 <- unique(temp_2010_2011)
temp_2010_2011$ri_df <- 1
temp_2010_2011 <- merge(temp_2010_2011, r_2013_2017, by="region", all=T)
temp_2010_2011 <- temp_2010_2011[rowSums(is.na(temp_2010_2011)) > 0,]

rm(list=setdiff(ls(), c("list_2016", "list_2013", "list_2010", "df", "df_2013_2017", "df_2012", "df_2010_2011", "r_2010_2011", "r_2012", "r_2013_2017")))

##### 2007-2009 #####

df_2007_2009 <- subset(df, year==2009 | year==2008 | year==2007)
length(unique(df_2007_2009$region))
r_2007_2009 <- df_2007_2009[,c('region')]
r_2007_2009 <- unique(r_2007_2009)

r_2007_2009$ri_df <- 1
temp_2007_2009 <- merge(r_2007_2009, list_2016, by="region", all=T)
temp_2007_2009 <- merge(r_2007_2009, list_2013, by="region", all=T)
# 2007-2009 classified in NUTS 2010: first, reclassify it to 2013, then to 2016

# recode the straightforward regions from 2010 to 2013 (EL, FR overseas territories (already in 2013/2016 in "df"), SI (lack in robot data)), EL only reclassified
# + problematic splits in London UK (later)

X2010_2013 <- read_excel("../original_data/NUTS/2010-2013.xls", sheet = "Sheet1")

# replace the 2010 names with the 2013 names
inds <- match(df_2007_2009$region, X2010_2013$code_2010)
df_2007_2009$region[!is.na(inds)] <- X2010_2013$code_2013[na.omit(inds)]

# replace the 2013 names with the 2016 names for the straightforward regions (mainly FR)
# regions which were reclassified in 2013/2016 were in the following countries: IE, FR, LT, HU, PL, UK
# FR and part of PL straightforward (names changed)
# IE01, IE02, LT00, HU10, PL12, UKM3 (division in two regions)

# the straightforward regions
X2013_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx", sheet = "Correspondence NUTS-2")
X2013_2016 <- X2013_2016[,1:2]
colnames(X2013_2016) <- c("code_2013", "code_2016")
X2013_2016 <- X2013_2016[rowSums(is.na(X2013_2016)) == 0,]

# replace the 2013 names with the 2016 names
inds <- match(df_2007_2009$region, X2013_2016$code_2013)
df_2007_2009$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

temp_2007_2009 <- df_2007_2009[,c('region')]
temp_2007_2009 <- unique(temp_2007_2009)
temp_2007_2009$ri_df <- 1
temp_2007_2009 <- merge(temp_2007_2009, r_2013_2017, by="region", all=T)
temp_2007_2009 <- temp_2007_2009[rowSums(is.na(temp_2007_2009)) > 0,]

rm(list=setdiff(ls(), c("list_2016", "list_2013", "list_2010", "df", "df_2013_2017", "df_2012", "df_2010_2011", "df_2007_2009", "r_2007_2009", "r_2010_2011", "r_2012", "r_2013_2017")))

##### 2005-2006 ####

df_2005_2006 <- subset(df, year==2006 | year==2005)
length(unique(df_2005_2006$region))
r_2005_2006 <- df_2005_2006[,c('region')]
r_2005_2006 <- unique(r_2005_2006)

r_2005_2006$ri_df <- 1
temp_2005_2006 <- merge(r_2005_2006, list_2016, by="region", all=T)
temp_2005_2006 <- merge(r_2005_2006, list_2013, by="region", all=T)
temp_2005_2006 <- merge(r_2005_2006, r_2007_2009, by="region", all=T)
# classified like 2007-2009 (in NUTS 2010) but for Denmark, there is just DK00, no NUTS2

# recode the straightforward regions from 2010 to 2013 (EL, FR overseas territories (already in 2013/2016 in "df"), SI (lack in robot data)), EL only reclassified
# + problematic splits in London UK (later)

X2010_2013 <- read_excel("../original_data/NUTS/2010-2013.xls", sheet = "Sheet1")

# replace the 2010 names with the 2013 names
inds <- match(df_2005_2006$region, X2010_2013$code_2010)
df_2005_2006$region[!is.na(inds)] <- X2010_2013$code_2013[na.omit(inds)]

# replace the 2013 names with the 2016 names for the straightforward regions (mainly FR)
# regions which were reclassified in 2013/2016 were in the following countries: IE, FR, LT, HU, PL, UK
# FR and part of PL straightforward (names changed)
# IE01, IE02, LT00, HU10, PL12, UKM3 (division in two regions)

# the straightforward regions
X2013_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx", sheet = "Correspondence NUTS-2")
X2013_2016 <- X2013_2016[,1:2]
colnames(X2013_2016) <- c("code_2013", "code_2016")
X2013_2016 <- X2013_2016[rowSums(is.na(X2013_2016)) == 0,]

# replace the 2013 names with the 2016 names
inds <- match(df_2005_2006$region, X2013_2016$code_2013)
df_2005_2006$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

temp_2005_2006 <- df_2005_2006[,c('region')]
temp_2005_2006 <- unique(temp_2005_2006)
temp_2005_2006$ri_df <- 1
temp_2005_2006 <- merge(temp_2005_2006, r_2013_2017, by="region", all=T)
temp_2005_2006 <- temp_2005_2006[rowSums(is.na(temp_2005_2006)) > 0,]

rm(list=setdiff(ls(), c("list_2016", "list_2013", "list_2010", "df", "df_2013_2017", "df_2012", "df_2010_2011", "df_2007_2009", "df_2005_2006", "r_2005_2006", "r_2007_2009", "r_2010_2011", "r_2012", "r_2013_2017")))

##### 2004 #####

df_2004 <- subset(df, year==2004)
length(unique(df_2004$region))
r_2004 <- df_2004[,c('region')]
r_2004 <- unique(r_2004)

r_2004$ri_df <- 1
temp_2004 <- merge(r_2004, list_2016, by="region", all=T)
temp_2004 <- merge(r_2004, list_2013, by="region", all=T)
temp_2004 <- merge(r_2004, list_2010, by="region", all=T)
# classified in NUTS 2006 + for Denmark, there is just DK00, no NUTS2

# first recode straightforward regions to NUTS 2010, then to 2013, then to 2016
# straightforward changes 2006/2020:
# GR --> EL (already fine in df_2004)
# IT, UK, DE

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_2004$region, X2006_2010$code_2006)
df_2004$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

X2010_2013 <- read_excel("../original_data/NUTS/2010-2013.xls", sheet = "Sheet1")
# replace the 2010 names with the 2013 names
inds <- match(df_2004$region, X2010_2013$code_2010)
df_2004$region[!is.na(inds)] <- X2010_2013$code_2013[na.omit(inds)]

# replace the 2013 names with the 2016 names for the straightforward regions (mainly FR)
X2013_2016 <- read_excel("../original_data/NUTS/2013-2016.xlsx", sheet = "Correspondence NUTS-2")
X2013_2016 <- X2013_2016[,1:2]
colnames(X2013_2016) <- c("code_2013", "code_2016")
X2013_2016 <- X2013_2016[rowSums(is.na(X2013_2016)) == 0,]

# replace the 2013 names with the 2016 names
inds <- match(df_2004$region, X2013_2016$code_2013)
df_2004$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

temp_2004 <- df_2004[,c('region')]
temp_2004 <- unique(temp_2004)
temp_2004$ri_df <- 1
temp_2004 <- merge(temp_2004, r_2013_2017, by="region", all=T)
temp_2004 <- temp_2004[rowSums(is.na(temp_2004)) > 0,]

# another thing to fix: temp_2004 has 3 codes from NUTS 2003 classification instead of one from the NUTS 2006 classification:
# (2003) DEE1 + DEE2 + DEE3 = (2006) DEE0

############### 3. MERGE DATA FILES AND DRAW TIME SERIES TO CHOOSE SPLIT REGIONS ############### 

df_merged <- rbind(df_2004, df_2005_2006, df_2007_2009, df_2010_2011, df_2012, df_2013_2017)
rm(list=setdiff(ls(), c("df", "df_2013_2017", "df_2012", "df_2010_2011", "df_2007_2009", "df_2005_2006", "df_2004", "df_merged", "temp_2004")))
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]

# # keep only split regions for plots
# split_regions <- unique(temp_2004$region)
# length(split_regions)
# length(unique(df_merged$region))
# df_merged_split <- df_merged[df_merged$region %in% split_regions, ]
# length(unique(df_merged_split$region))
# 
# # replace NA with 0
# df_merged_split$value[is.na(df_merged_split$value)] <- 0
# 
# # sum by region and year
# df_agg <- aggregate(df_merged_split$value, by=list(region=df_merged_split$region, year=df_merged_split$year), FUN=sum)
# df_agg <- df_agg[with(df_agg, order(region, year)),]
# colnames(df_agg)[3] <- "value"
# 
# region_plots <- list()
# 
# for (region_ in split_regions){
#   region_plots[[region_]] <- ggplot(df_agg %>% filter(region==region_), aes(x=year, y=value)) +
#     geom_line(size=3, color="#FDE725FF") +
#     theme_bw() +
#     labs(title=region_, y="1000 Employees", x="Year") +
#     theme(plot.title = element_text(size=20, hjust = (0.5)),
#           plot.subtitle = element_text(size=12, hjust=(0.5)),
#           axis.text.x = element_text(size=12, angle=90, vjust = 0.5),
#           axis.text.y = element_text(size=12),
#           axis.title.x = element_text(size=14),
#           axis.title.y = element_text(size = 14),
#           legend.position = "bottom",
#           legend.text = element_text(size=12)) +
#     scale_x_continuous(limits = c(2004, 2017), breaks = seq(2004, 2017, 1)) +
#     geom_text(aes(label=round(value, digit=1)), size=4, colour="black", position=position_dodge(width=1), vjust=0)
#     print(region_plots[[region_]])
#     ggsave(region_plots[[region_]], file=paste0("region_plots_ts_2004_2017/", region_, ".png"))
# }

rm(list=setdiff(ls(), c("df_merged")))
# discard lines where there is no industry in any classification
df_merged <- subset(df_merged, na112d!="No answer" | nace2d!="No answer")

############### 4. RECODE SPLIT REGIONS FOR THE INDUSTRY CODES IN THE DATA ############### 
# GOING CASE BY CASE BASED ON THE "region_splits" SPREADHEET ACTION

df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]

# CASE 1

df_merged$region <- str_replace_all(df_merged$region, "IE04", "IE01")

# CASE 2

case_temp <- subset(df_merged, (region=="IE05" | region=="IE06") & year>2011)
case_temp_neq <- subset(df_merged, !((region=="IE05" | region=="IE06") & year>2011))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "IE02"
case_temp$na112d <- "No answer"
case_temp$country <- "IE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 3

case_temp <- subset(df_merged, (region=="LT01" | region=="LT02") & year>2012)
case_temp_neq <- subset(df_merged, !((region=="LT01" | region=="LT02") & year>2012))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }


case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "LT00"
case_temp$na112d <- "No answer"
case_temp$country <- "LT"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 4

case_temp <- subset(df_merged, (region=="HU11" | region=="HU12") & year>2012)
case_temp_neq <- subset(df_merged, !((region=="HU11" | region=="HU12") & year>2012))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "HU10"
case_temp$na112d <- "No answer"
case_temp$country <- "HU"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 5

case_temp <- subset(df_merged, (region=="PL91" | region=="PL92") & year>2012)
case_temp_neq <- subset(df_merged, !((region=="PL91" | region=="PL92") & year>2012))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }


case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "PL12"
case_temp$na112d <- "No answer"
case_temp$country <- "PL"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 6

case_temp <- subset(df_merged, (region=="UKM8" | region=="UKM9") & year>2011)
case_temp_neq <- subset(df_merged, !((region=="UKM8" | region=="UKM9") & year>2011))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "UKM3"
case_temp$na112d <- "No answer"
case_temp$country <- "UK"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 7

case_temp <- subset(df_merged, (region=="UKI3" | region=="UKI4") & year>2009)
case_temp_neq <- subset(df_merged, !((region=="UKI3" | region=="UKI4") & year>2009))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }


case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "UKI1"
case_temp$na112d <- "No answer"
case_temp$country <- "UK"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 8

case_temp <- subset(df_merged, (region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009)
case_temp_neq <- subset(df_merged, !((region=="UKI5" | region=="UKI6" | region=="UKI7") & year>2009))

unique(case_temp$na112d)
# everything coded in nace2d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$nace2d)){
    temp <- case_temp %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, nace2d, year)),]
case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, nace2d=case_temp$nace2d), FUN=sum)
case_temp$region <- "UKI2"
case_temp$na112d <- "No answer"
case_temp$country <- "UK"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 9

case_temp <- subset(df_merged, (region=="DE41" | region=="DE42") & year==2004)
case_temp_neq <- subset(df_merged, !((region=="DE41" | region=="DE42") & year==2004))

unique(case_temp$na112d)
unique(case_temp$nace2d)
# everything coded in na112d

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DE40"
case_temp$nace2d <- "No answer"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 10

case_temp <- subset(df_merged, (region=="FI13" | region=="FI1A") & year==2004)
case_temp_neq <- subset(df_merged, !((region=="FI13" | region=="FI1A") & year==2004))

unique(case_temp$na112d)
unique(case_temp$nace2d)
# everything coded in na112d

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "FI1D"
case_temp$nace2d <- "No answer"
case_temp$country <- "FI"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 11/12

case_temp <- subset(df_merged, (region=="FI1B" | region=="FI1C") & year<2008)
case_temp_neq <- subset(df_merged, !((region=="FI1B" | region=="FI1C") & year<2008))
unique(case_temp$year)

# expand to include 2004
line <- c("FI", "FI1B", 2004, "01", "No answer", NA)
case_temp <- rbind(case_temp, line)
combinations <- expand(case_temp, region, year, na112d)

case_temp <- merge(combinations, case_temp, by=c("region", "year", "na112d"), all.x=T)
rm(combinations)
case_temp <- case_temp[with(case_temp, order(region, na112d, year)),]
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]
case_temp$nace2d <- "No answer"
case_temp$value <- as.numeric(case_temp$value)

unique(case_temp$na112d)
# everything coded in na112d

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(case_temp$region)){
  for (industry_ in unique(case_temp$na112d)){
    temp <- case_temp %>% filter(region==region_ & na112d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
        temp$value <- na_interpolation(temp$value, option = "linear")
        df_2 <- rbind(df_2, temp)
      }
    }
  }

case_temp <- rbind(df_1, df_2)
rm(df_1, df_2)
case_temp <- case_temp[with(case_temp, order(region, na112d, year)),]
case_temp$nace2d <- "No answer"
case_temp$country <- "FI"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 14

case_temp <- subset(df_merged, (region=="DEE1" | region=="DEE2" | region=="DEE3") & year==2004)
case_temp_neq <- subset(df_merged, !((region=="DEE1" | region=="DEE2" | region=="DEE3") & year==2004))

unique(case_temp$na112d)
unique(case_temp$nace2d)
# everything coded in na112d

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DEE0"
case_temp$nace2d <- "No answer"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 13

weights <- subset(df_merged, (region=="DK01" | region=="DK02" | region=="DK03" | region=="DK04" | region=="DK05") & year==2007)
sum <- sum(is.na(weights$value))
weights$value[is.na(weights$value)] <- runif(n=sum, min=0, max=1)
weights <- aggregate(weights$value, by=list(region=weights$region, year=weights$year, na112d=weights$na112d), FUN=sum)

wg <- data.frame()

# CREATE SUM BY INDUSTRY IN A LOOP HERE !!!
for (i in unique(weights$na112d)){
  temp <- weights %>% filter(na112d==i)
  sum <- sum(temp$x)
  temp$share <- temp$x/sum
  wg <- rbind(wg, temp)
}

wg <- wg[,c("region", "na112d", "share")]

case_temp <- subset(df_merged, (region=="DK00") & year<2007)
case_temp_neq <- subset(df_merged, !((region=="DK00") & year<2007))

case_temp <- rbind(case_temp, case_temp, case_temp, case_temp, case_temp)
case_temp <- case_temp[with(case_temp, order(region, na112d, year)),]
row.names(case_temp) <- NULL
case_temp$region <- rep(c("DK01", "DK02", "DK03", "DK04", "DK05"), 170)
case_temp <- merge(case_temp, wg, by=c("region", "na112d"), all.x=T)
case_temp <- case_temp[with(case_temp, order(region, year, na112d)),]
row.names(case_temp) <- NULL

case_temp <- case_temp %>% mutate_all(~ifelse(is.nan(.), NA, .))
case_temp$share <- as.numeric(case_temp$share)
case_temp$value <- case_temp$value*case_temp$share  
case_temp <- case_temp[,c("country", "region", "year", "na112d", "nace2d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
rm(list=setdiff(ls(), c("df_merged")))
row.names(df_merged) <- NULL

######### SEE IF THERE ARE COUNTRIES IN THE SAMPLE (INSTEAD OF REGIONS) #########

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df_merged$country1 <- substrRight(df_merged$region, 2)
countries <- subset(df_merged, country1=="00")
unique(countries$country)

poland <- subset(df_merged, country=="PL")
unique(poland$region)

estonia <- subset(df_merged, country=="EE")
unique(estonia$region)

lithuania <- subset(df_merged, country=="LT")
unique(lithuania$region)

latvia <- subset(df_merged, country=="LV")
unique(latvia$region)

malta <- subset(df_merged, country=="MT")
unique(malta$region)

denmark <- subset(df_merged, country=="DK")
unique(denmark$region)

# discard DK00 and PL00 from the data
x <- subset(df_merged, region!="PL00")
length(unique(x$region))
length(unique(df_merged$region))

df_merged <- x
df_merged$country1 <- NULL
rm(list=setdiff(ls(), c("df_merged")))

############### 5. RECODE NACE TO ISIC ###############   

# 1. Do a linear interpolation in groups on original NACE codes before recoding to ISIC (to later match robot data).

# NACE 1.1: 2004-2007
df_2004_2007 <- subset(df_merged, year<2008)
unique(df_2004_2007$year)
unique(df_2004_2007$nace2d)

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(df_2004_2007$region)){
  for (industry_ in unique(df_2004_2007$na112d)){
    temp <- df_2004_2007 %>% filter(region==region_ & na112d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

df_2004_2007 <- rbind(df_1, df_2)

# NACE 2: 2008-2017
df_2008_2017 <- subset(df_merged, year>2007)
unique(df_2008_2017$year)
unique(df_2008_2017$na112d)

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(df_2008_2017$region)){
  for (industry_ in unique(df_2008_2017$nace2d)){
    temp <- df_2008_2017 %>% filter(region==region_ & nace2d==industry_)
    if ((colSums(is.na(temp)))[6]==nrow(temp) | (colSums(is.na(temp)))[6]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
      }
    }
  }

df_2008_2017 <- rbind(df_1, df_2)
df_merged <- rbind(df_2004_2007, df_2008_2017)
df_merged <- df_merged[with(df_merged, order(region, nace2d, year)),]
row.names(df_merged) <- NULL
rm(list=setdiff(ls(), c("df_merged")))

# 2. Recode NACE to ISIC & KEEP ONLY MANUFACTURING INDUSTRIES

df_merged <- df_merged %>% mutate(
industry = case_when(
  na112d=="30" | na112d=="37" | nace2d=="32" | nace2d=="33" | na112d=="23" | nace2d=="19" ~ "All other manufacturing branches/other chemical products n.e.c",
  
  na112d=="34" | nace2d=="29" | na112d=="35" | nace2d=="30" ~ "Automotive/Other vehicles",
  
  na112d=="27" | nace2d=="24" ~ "Basic metals",
  
  na112d=="45" | nace2d=="41" | nace2d=="42" | nace2d=="43" ~ "Construction",
  
  na112d=="31" | na112d=="32" | na112d=="33" | nace2d=="26" | nace2d=="27" ~ "Electrical/electronics",
  
  na112d=="40" | na112d=="41" | nace2d=="35" | nace2d=="36" ~ "Electricity, gas, water supply",
  
  na112d=="15" | na112d=="16" | nace2d=="10" | nace2d=="11" | nace2d=="12" ~ "Food and beverages",
  
  na112d=="26" | nace2d=="23" ~ "Glass, ceramics, stone, mineral products (non-automotive)",
  
  na112d=="29" | nace2d=="28" ~ "Industrial machinery",
  
  na112d=="28" | nace2d=="25" ~ "Metal products (non-automotive)",
  
  na112d=="10" | na112d=="11" | na112d=="12" | na112d=="13" | na112d=="14" | nace2d=="05" | nace2d=="06" | nace2d=="07" | nace2d=="08" | nace2d=="09" ~ "Mining and quarrying",
  
  na112d=="21" | na112d=="22" | nace2d=="17" | nace2d=="18" ~ "Paper",
  
  na112d=="24" | nace2d=="20" | nace2d=="21" ~ "Pharmaceuticals, cosmetics",
  
  na112d=="25" | nace2d=="22" ~ "Rubber and plastic products (non-automotive)",
  
  na112d=="17" | na112d=="18" | na112d=="19" | nace2d=="13" | nace2d=="14" | nace2d=="15" ~ "Textiles",
  
  na112d=="20" | na112d=="36" | nace2d=="16" | nace2d=="31" ~ "Wood and furniture"
)
)

#df_merged$industry[is.na(df_merged$industry)] <- "Non-manufacturing"

df_merged <- df_merged[!is.na(df_merged$industry),]  
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL  

save(df_merged, file="../generated_data/df_merged_after_step_2.RData")

# 3. MISSINGS FOR SOME REGION-YEARS-INDUSTRIES -- DRAW A RANDOM NUMBER FROM A UNIFORM DISTRIBUTION 0-1

rm(list=ls())
gc()
load("../generated_data/df_merged_after_step_2.RData")

length(unique(df_merged$region))*length(unique(df_merged$year))*length(unique(df_merged$industry))
# max 60.928 rows possible if employment was observed in all regions-years-industries

df_merged <- df_merged[,c("country", "region", "year", "industry", "value")]

df_1 <- data.frame()
df_2 <- data.frame()
df_3 <- data.frame()

for (region_ in unique(df_merged$region)){
  for (industry_ in unique(df_merged$industry)){
    temp <- df_merged %>% filter(region==region_ & industry==industry_)
    if ((colSums(is.na(temp)))[5]==nrow(temp)){
      df_1 <- rbind(df_1, temp)
    }else{
      if ((colSums(is.na(temp)))[5]==0){
        df_2 <- rbind(df_2, temp)
      }else{
        df_3 <- rbind(df_3, temp)
      }
    }
  }
}


df_1 <- unique(df_1)
df_2 <- aggregate(df_2$value, by=list(country=df_2$country, region=df_2$region, year=df_2$year, industry=df_2$industry), FUN=sum)

sum <- sum(is.na(df_3$value))
df_3$value[is.na(df_3$value)] <- runif(n=sum, min=0, max=1)
df_3 <- aggregate(df_3$value, by=list(country=df_3$country, region=df_3$region, year=df_3$year, industry=df_3$industry), FUN=sum)

colnames(df_2)[5] <- "value"
colnames(df_3)[5] <- "value"

df_1$flag <- 1
df_2$flag <- 2
df_3$flag <- 3

df_merged <- rbind(df_1, df_2, df_3)
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL
rm(list=setdiff(ls(), c("df_merged")))

# 4. ENTIRE INDUSTRY-YEARS MISSING FOR REGIONS: IMPUTATION

# A. IMPUTE WITH MEDIAN FOR THE INDUSTRY FROM THE ENTIRE COUNTRY & NORMALIZE TO 0-1

# flag2==1 if "value" is missing
df_merged$flag2 <- ifelse(is.na(df_merged$value), 1, 0)

df <- data.frame()

# split into country-industry-year categories
for (country_ in unique(df_merged$country)){
  for (industry_ in unique(df_merged$industry)){
    for (year_ in unique(df_merged$year)){
      temp <- df_merged %>% filter(country==country_, industry==industry_, year==year_)
      temp <- temp %>% mutate(value=ifelse(is.na(value),median(value,na.rm=TRUE),value))
      df <- rbind(df, temp)
      df <- df[with(df, order(region, industry, year)),]
      row.names(df) <- NULL
    }
  }
}

# normalize the imputed medians to 0-1 range
df1_imputed <- subset(df, flag2==1 & is.na(value)==0)
df2_not_imputed <- subset(df, flag2==1 & is.na(value)==1) 
df3_not_missing <- subset(df, flag2==0)

df1 <- data.frame()

for (region_ in unique(df1_imputed$region)){
  for (industry_ in unique(df1_imputed$industry)){
    temp <- df1_imputed %>% filter(region==region_ & industry==industry_)
    temp$value <- (temp$value-min(temp$value))/(max(temp$value)-min(temp$value))
    df1 <- rbind(df1, temp)
  }
}

df_merged <- rbind(df1, df2_not_imputed, df3_not_missing)
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL
rm(list=setdiff(ls(), c("df_merged")))

save(df_merged, file="../generated_data/df_merged_after_step_4a.RData")

# B. IMPUTE WITH MEDIAN FOR THE EMPLOYMENT FROM IN ENTIRE COUNTRY, IN ALL INDUSTRIES & NORMALIZE TO 0-1   

rm(list=ls())
gc()
load("../generated_data/df_merged_after_step_4a.RData")

# flag3==1 if "value" is still missing after the first imputation
df_merged$flag3 <- ifelse(is.na(df_merged$value), 1, 0)

# calculate the fraction of missing values
sum(is.na(df_merged$value))/sum(1-is.na(df_merged$value))
na <- df_merged[rowSums(is.na(df_merged)) > 0,]

df <- data.frame()

# split into country-year categories
for (country_ in unique(df_merged$country)){
  for (year_ in unique(df_merged$year)){
    temp <- df_merged %>% filter(country==country_, year==year_)
    temp <- temp %>% mutate(value=ifelse(is.na(value),median(value,na.rm=TRUE),value))
    df <- rbind(df, temp)
    df <- df[with(df, order(region, industry, year)),]
    row.names(df) <- NULL
  }
}

sum(is.na(df$value))/sum(1-is.na(df$value))

# normalize the imputed medians to 0-1 range
df1_imputed <- subset(df, flag3==1)
df2_not_imputed <- subset(df, flag3==0) 

df1 <- data.frame()

for (region_ in unique(df1_imputed$region)){
  for (industry_ in unique(df1_imputed$industry)){
    temp <- df1_imputed %>% filter(region==region_ & industry==industry_)
    temp$value <- (temp$value-min(temp$value))/(max(temp$value)-min(temp$value))
    df1 <- rbind(df1, temp)
  }
}

df_merged <- rbind(df1, df2_not_imputed)
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL

sum(is.na(df_merged$value))/sum(1-is.na(df_merged$value))
na <- df_merged[rowSums(is.na(df_merged)) > 0,]
# only 3 obs have NA & no way to further impute --> discard them
df_merged <- subset(df_merged, region!="FI18")
df_merged <- subset(df_merged, !(region=="EL41" & industry=="Automotive/Other vehicles"))
df_merged <- subset(df_merged, !(region=="EL62" & industry=="Industrial machinery"))

na <- df_merged[rowSums(is.na(df_merged)) > 0,]

rm(list=setdiff(ls(), c("df_merged")))
df_merged$flag2 <- NULL
df_merged$flag3 <- NULL
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL

df_merged$year <- as.numeric(df_merged$year)
save(df_merged, file="../generated_data/df_merged_after_step_4b.RData")

# 5. PLOT THE TIME SERIES TO SEE IF THE IMPUTATIONS MAKE SENSE

rm(list=ls())
gc()
load("../generated_data/df_merged_after_step_4b.RData")

df_merged <- df_merged %>% mutate(
  industry2 = case_when(
    industry=="All other manufacturing branches/other chemical products n.e.c" ~ 1,

    industry=="Automotive/Other vehicles" ~ 2,

    industry=="Basic metals" ~ 3,

    industry=="Construction" ~ 4,

    industry=="Electrical/electronics" ~ 5,

    industry=="Electricity, gas, water supply" ~ 6,

    industry=="Food and beverages" ~ 7,

    industry=="Glass, ceramics, stone, mineral products (non-automotive)" ~ 8,

    industry=="Industrial machinery" ~ 9,

    industry=="Metal products (non-automotive)" ~ 10,

    industry=="Mining and quarrying" ~ 11,

    industry=="Paper" ~ 12,

    industry=="Pharmaceuticals, cosmetics" ~ 13,

    industry=="Rubber and plastic products (non-automotive)" ~ 14,

    industry=="Textiles" ~ 15,

    industry=="Wood and furniture" ~ 16
  )
)

max(df_merged$value)
min(df_merged$value)

# region_plots <- list()
# 
# for (region_ in unique(df_merged$region)){
#   for (industry_ in unique(df_merged$industry2)){
#     temp <- df_merged %>% filter(region==region_ & industry2==industry_)
#     type <- max(temp$flag)
#     colort <- ifelse(type==1, "#440154FF",
#                     ifelse(type==2, "#21908CFF","#FDE725FF"))
#     region_plots[[region_]] <- ggplot(temp, aes(x=year, y=value)) +
#       geom_line(size=3, color=colort) +
#       theme_bw() +
#       labs(title=region_, subtitle=industry_, y="1000 Employees", x="Year") +
#       theme(plot.title = element_text(size=20, hjust = (0.5)),
#             plot.subtitle = element_text(size=12, hjust=(0.5)),
#             axis.text.x = element_text(size=12, angle=90, vjust = 0.5),
#             axis.text.y = element_text(size=12),
#             axis.title.x = element_text(size=14),
#             axis.title.y = element_text(size = 14),
#             legend.position = "bottom",
#             legend.text = element_text(size=12)) +
#       scale_x_continuous(limits = c(2004, 2017), breaks = seq(2004, 2017, 1)) +
#       #scale_y_continuous(limits = c(0, 486)) +
#       geom_text(aes(label=round(value, digit=1)), size=4, colour="black", position=position_dodge(width=1), vjust=0)
#     #print(region_plots[[region_]])
#     ggsave(region_plots[[region_]], file=paste0("all_region_plots_ts_2004_2017/", region_, "_", industry_, "_", type, ".png"))
#   }
# }

df_merged$industry2 <- NULL

# 6. PREPARE DATA FOR CALCULATION OF EXPOSURE TO ROBOTS

df_merged$flag <- NULL

df_merged_2004 <- subset(df_merged, year==2004)
unique(df_merged_2004$year)
colnames(df_merged_2004)[5] <- "employment_t0"
df_merged_2004 <- df_merged_2004[,c("region", "industry", "employment_t0")]

df_merged_2007_2017 <- subset(df_merged, year>2006)  
unique(df_merged_2007_2017$year)
colnames(df_merged_2007_2017)[5] <- "employment"  

employment_2007_2017 <- merge(df_merged_2007_2017, df_merged_2004, by=c("region", "industry"), all.x = T)
na <- employment_2007_2017[rowSums(is.na(employment_2007_2017)) > 0,]
# PROBLEM: SOME REGION-INDUSTRIES DON'T HAVE EMPLOYMENT IN T0  

na <- na[,c("region", "industry")]
na <- unique(na)
length(unique(na$region))
length(unique(na$industry))
# 104 region-industries without 2004
# subtract them from df_merged

missing_t0 <- merge(na, df_merged, by=c("region", "industry"), all.x=T)
length(unique(missing_t0$region))
length(unique(missing_t0$industry))

missing_t0 <- missing_t0[with(missing_t0, order(region, industry, year)),]
row.names(missing_t0) <- NULL
unique(missing_t0$year)
missing_t0$year <- as.numeric(missing_t0$year)

df <- data.frame()
# min_year <- c()
for (region_ in unique(missing_t0$region)){
  for (industry_ in unique(missing_t0$industry)){
    temp <- missing_t0 %>% filter(region==region_ & industry==industry_)
    if (dim(temp)!=0){
      temp$year0 <- min(temp$year)
      df <- rbind(df, temp)
    }
  }
}

missing_t0_2 <- df[,c("region", "industry", "year0")]
missing_t0_2 <- unique(missing_t0_2)

n_occur <- data.frame(table(missing_t0_2$year0))
# 40 region-industries begin later than 2005 --> discard them
discard <- subset(missing_t0_2, year0!=2005)
discard <- discard[,c("region", "industry")]

keep <- subset(missing_t0_2, year0==2005)
keep <- keep[,c("region", "industry")]

rm(list=setdiff(ls(), c("df_merged", "discard", "keep")))

missing_t0 <- merge(keep, df_merged, by=c("region", "industry"), all.x=T)  
missing_t0 <- missing_t0[with(missing_t0, order(region, industry, year)),]
row.names(missing_t0) <- NULL

# get one more row
df <- data.frame()
for (region_ in unique(missing_t0$region)){
  for (industry_ in unique(missing_t0$industry)){
    temp <- missing_t0 %>% filter(region==region_ & industry==industry_)
    temp <- subset(temp, year==2005)
    df <- rbind(df, temp)
  }
}

df$year <- 2004
df$value <- NA

missing_t0 <- rbind(missing_t0, df)
missing_t0 <- missing_t0[with(missing_t0, order(region, industry, year)),]
row.names(missing_t0) <- NULL

# interpolate 2004
df <- data.frame()
for (region_ in unique(missing_t0$region)){
  for (industry_ in unique(missing_t0$industry)){
    temp <- missing_t0 %>% filter(region==region_ & industry==industry_)
    temp$value <- na_interpolation(temp$value, option = "linear")
    df <- rbind(df, temp)
  }
}

check <- df[,c("region", "industry")]
check <- unique(check)

discard <- rbind(keep, discard)

rm(list=setdiff(ls(), c("df_merged", "discard", "df")))
discard <- merge(discard, df_merged, by=c("region", "industry"), all.x = T)
df_merged <- setdiff(df_merged, discard)
rm(list=setdiff(ls(), c("df_merged")))

# extract t0==2004

df_merged_2004 <- subset(df_merged, year==2004)
unique(df_merged_2004$year)
colnames(df_merged_2004)[5] <- "employment_2004"
df_merged_2004 <- df_merged_2004[,c("region", "industry", "employment_2004")]

df_merged_2007_2017 <- subset(df_merged, year>2006)  
unique(df_merged_2007_2017$year)
colnames(df_merged_2007_2017)[5] <- "employment"  

employment_2007_2017 <- merge(df_merged_2007_2017, df_merged_2004, by=c("region", "industry"), all.x = T)
na <- employment_2007_2017[rowSums(is.na(employment_2007_2017)) > 0,]

# no NAs --> save the complete dataset, ready for EXPOSURE TO ROBOTS calculation  
save(employment_2007_2017, file="../generated_data/df_merged_final_2007_2017.RData")

length(unique(employment_2007_2017$region))
length(unique(employment_2007_2017$industry))

# 271 regions, 16 industries