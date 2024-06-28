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
options(scipen=20)
options(digits=2)

requiredPackages = c("splitstackshape", "tidyverse", "dplyr", "reshape2", "haven", 
                     "stringr", "viridisLite", "gridExtra", "lmreg", "mice","Hmisc",
                     "ggrepel", "viridis", "foreign", "bnstruct", "outliers","forecast",
                     "TSstudio", "zoo", "factoextra", "RStata", "plyr", "zoo", "imputeTS",
                     "readxl")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
gc()
}

# Gentleman's agreement: 1990-2003
# NUTS 2003: 2004-2007
# NUTS 2006: 2008-2011
# NUTS 2010: 2012-2014
# NUTS 2013: 2015-2017
# NUTS 2016: 2018-2020

############### 1. FIND REGION CHANGES FOR THE EMPLOYMENT DATA FILES 1986-1999 and 2000-2017 ############### 

##### load the 1986-1999 file ##### 

df_1 <- read_dta("../original_data/employment_1986_1999.dta")
df_1 <- df_1[,c("country", "region", "year", "na112d", "value")]
df_1$r <- paste(df_1$country, df_1$region,sep="")
df_1 <- df_1[,c("country", "r", "year", "na112d", "value")]
colnames(df_1)[2] <- "region"
df_1 <- df_1[with(df_1, order(region, na112d, year)),]
length(unique(df_1$region))
length(unique(df_1$country))

# keep years 1994-1999
df_1 <- subset(df_1, year>1993)
unique(df_1$year)
df_1 <- df_1[with(df_1, order(region, na112d, year)),]

# keep only robotization pioneers
unique(df_1$country)
countries <- c("DE", "ES", "FI", "FR", "IT", "NO", "SE", "UK", "BE")

df_1 <- df_1[df_1$country %in% countries,]
length(unique(df_1$country))

# exclude regions which are overseas
discarded_regions <- c("FR91", "FR92", "FR93", "FR94", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(unique(df_1$region))
`%ni%` <- Negate(`%in%`)
df_1 <- df_1[df_1$region %ni% discarded_regions, ]
length(unique(df_1$region))

unique(df_1$na112d)
df_1 <- subset(df_1, na112d!="No answer")

rm(list=setdiff(ls(), c("df_1", "countries")))

##### load the 2000-2017 data file ##### 

df_2 <- read_dta("../original_data/employment_2000_2017.dta")
df_2 <- df_2[,c("country", "region", "year", "na112d", "nace2d", "value")]
df_2$r <- paste(df_2$country, df_2$region,sep="")
df_2 <- df_2[,c("country", "r", "year", "na112d", "nace2d", "value")]
colnames(df_2)[2] <- "region"
df_2 <- df_2[with(df_2, order(region, na112d, year)),]
length(unique(df_2$region))
length(unique(df_2$country))

# keep only years 2000-2007
df_2 <- subset(df_2, year<2008)
unique(df_2$year)
unique(df_2$na112d)
df_2 <- subset(df_2, !(na112d=="No answer" & nace2d=="No answer"))
unique(df_2$na112d)
df_2$nace2d <- NULL
df_2 <- subset(df_2, !(na112d=="No answer"))
unique(df_2$na112d)

# keep only robotization pioneers
unique(df_2$country)
df_2 <- df_2[df_2$country %in% countries,]
length(unique(df_2$country)) # CH and NO are in the dataset

# exclude regions which are overseas
discarded_regions <- c("FR91", "FR92", "FR93", "FR94", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)
length(unique(df_2$region))
`%ni%` <- Negate(`%in%`)
df_2 <- df_2[df_2$region %ni% discarded_regions, ]
length(unique(df_2$region))

rm(list=setdiff(ls(), c("df_1", "df_2", "countries")))

##### load list of 1995 NUTS and 1999 NUTS #####

nuts_1995_1999 <- read_excel("../original_data/NUTS/1995-1999.xls")

list_1995 <- nuts_1995_1999[,c(2)]
colnames(list_1995) <- c("nuts_1995")
list_1995 <- subset(list_1995, nchar(nuts_1995)==4)

list_1999 <- nuts_1995_1999[,c(3)]
colnames(list_1999) <- c("nuts_1999")
list_1999 <- subset(list_1999, nchar(nuts_1999)==4)
  
list_1995 <- subset(list_1995, nchar(nuts_1995)==4) # 208 regions in 1995
list_1999 <- subset(list_1999, nchar(nuts_1999)==4) # 202 regions in 1999
  
list_1995$ri_list_1995 <- 1
list_1999$ri_list_1999 <- 1
  
list_1995$country <- sub('..$','',list_1995$nuts_1995)
list_1999$country <- sub('..$','',list_1999$nuts_1999)
  
# keep only pioneer countries in the list
  
list_1995 <- list_1995[list_1995$country %in% countries, ]
length(unique(list_1995$country)) # CH and NO not there
  
list_1999 <- list_1999[list_1999$country %in% countries, ]
length(unique(list_1999$country)) # CH and NO not there
  
list_1995$country <- NULL
list_1999$country <- NULL
  
colnames(list_1995)[1] <- "region"
colnames(list_1999)[1] <- "region"

#####  load list of 1999 NUTS and 2003 NUTS #####
  
nuts_1999_2003 <- read_excel("../original_data/NUTS/1999-2003.xls")

list_1999_2 <- nuts_1999_2003[,c(2)]
colnames(list_1999_2) <- c("nuts_1999")
list_1999_2 <- subset(list_1999_2, nchar(nuts_1999)==4)

list_2003 <- nuts_1999_2003[,c(3)]
colnames(list_2003) <- c("nuts_2003")
list_2003 <- subset(list_2003, nchar(nuts_2003)==4)

list_1999_2 <- subset(list_1999_2, nchar(nuts_1999)==4) # 208 regions in 1999
list_2003 <- subset(list_2003, nchar(nuts_2003)==4) # 202 regions in 2003

list_1999_2$ri_list_1999_2 <- 1
list_2003$ri_list_2003 <- 1

list_1999_2$country <- sub('..$','',list_1999_2$nuts_1999)
list_2003$country <- sub('..$','',list_2003$nuts_2003)

# keep only pioneer countries in the list

list_1999_2 <- list_1999_2[list_1999_2$country %in% countries, ]
length(unique(list_1999_2$country)) # CH and NO not there

list_2003 <- list_2003[list_2003$country %in% countries, ]
length(unique(list_2003$country)) # CH and NO not there

list_1999_2$country <- NULL
list_2003$country <- NULL

colnames(list_1999_2)[1] <- "region"
colnames(list_2003)[1] <- "region"

##### load list of 2003 NUTS and 2006 NUTS ##### 
  
nuts_2003_2006 <- read_excel("../original_data/NUTS/2003-2006.xls")

list_2003_2 <- nuts_2003_2006[,c(2)]
colnames(list_2003_2) <- c("nuts_2003")
list_2003_2 <- subset(list_2003_2, nchar(nuts_2003)==4)

list_2006 <- nuts_2003_2006[,c(3)]
colnames(list_2006) <- c("nuts_2006")
list_2006 <- subset(list_2006, nchar(nuts_2006)==4)

list_2003_2 <- subset(list_2003_2, nchar(nuts_2003)==4) # 208 regions in 2003
list_2006 <- subset(list_2006, nchar(nuts_2006)==4) # 202 regions in 2006

list_2003_2$ri_list_2003_2 <- 1
list_2006$ri_list_2006 <- 1

list_2003_2$country <- sub('..$','',list_2003_2$nuts_2003)
list_2006$country <- sub('..$','',list_2006$nuts_2006)

# keep only pioneer countries in the list

list_2003_2 <- list_2003_2[list_2003_2$country %in% countries, ]
length(unique(list_2003_2$country)) # CH and NO not there

list_2006 <- list_2006[list_2006$country %in% countries, ]
length(unique(list_2006$country)) # CH and NO not there

list_2003_2$country <- NULL
list_2006$country <- NULL

colnames(list_2003_2)[1] <- "region"
colnames(list_2006)[1] <- "region"

##### load list of 2010 NUTS ##### 

nuts_2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls")

list_2010 <- nuts_2006_2010[,c(3)]
colnames(list_2010) <- c("nuts_2010")
list_2010 <- subset(list_2010, nchar(nuts_2010)==4)

list_2010 <- subset(list_2010, nchar(nuts_2010)==4)

list_2010$ri_list_2010 <- 1

list_2010$country <- sub('..$','',list_2010$nuts_2010)

# keep only pioneer countries in the list

list_2010 <- list_2010[list_2010$country %in% countries, ]
length(unique(list_2010$country)) # CH and NO not there

list_2010$country <- NULL

colnames(list_2010)[1] <- "region"

rm(nuts_1995_1999, nuts_1999_2003, nuts_2003_2006, nuts_2006_2010, list_1999_2, list_2003_2, countries)

list_1995$zz <- endsWith(list_1995$region, 'ZZ')
list_1995 <- subset(list_1995, zz==F)
list_1995$zz <- NULL

list_1999$zz <- endsWith(list_1999$region, 'ZZ')
list_1999 <- subset(list_1999, zz==F)
list_1999$zz <- NULL

list_2003$zz <- endsWith(list_2003$region, 'ZZ')
list_2003 <- subset(list_2003, zz==F)
list_2003$zz <- NULL

list_2006$zz <- endsWith(list_2006$region, 'ZZ')
list_2006 <- subset(list_2006, zz==F)
list_2006$zz <- NULL

list_2010$zz <- endsWith(list_2010$region, 'ZZ')
list_2010 <- subset(list_2010, zz==F)
list_2010$zz <- NULL

# exclude regions which are overseas
discarded_regions <- c("FR91", "FR92", "FR93", "FR94", "ES63", "ES64", "ES70") # overseas territories
length(discarded_regions)

length(unique(list_1995$region))
`%ni%` <- Negate(`%in%`)
list_1995 <- list_1995[list_1995$region %ni% discarded_regions, ]
length(unique(list_1995$region))

length(unique(list_1999$region))
`%ni%` <- Negate(`%in%`)
list_1999 <- list_1999[list_1999$region %ni% discarded_regions, ]
length(unique(list_1999$region))

length(unique(list_2003$region))
`%ni%` <- Negate(`%in%`)
list_2003 <- list_2003[list_2003$region %ni% discarded_regions, ]
length(unique(list_2003$region))

length(unique(list_2006$region))
`%ni%` <- Negate(`%in%`)
list_2006 <- list_2006[list_2006$region %ni% discarded_regions, ]
length(unique(list_2006$region))

length(unique(list_2010$region))
`%ni%` <- Negate(`%in%`)
list_2010 <- list_2010[list_2010$region %ni% discarded_regions, ]
length(unique(list_2010$region))

rm(discarded_regions, '%ni%')

##### 2007 #####
  
df_2005_2007 <- subset(df_2, year==2007 | year==2006 | year==2005)
length(unique(df_2005_2007$region))
r_2005_2007 <- df_2005_2007[,c('region')]
r_2005_2007 <- unique(r_2005_2007)
r_2005_2007$ri_df <- 1
temp_2005_2007 <- merge(r_2005_2007, list_2010, by="region", all=T)
# 2005-2007 coded in NUTS 2010, except there is no CH and NO

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007")))

##### 2004 #####

df_2004 <- subset(df_2, year==2004)
length(unique(df_2004$region))
r_2004 <- df_2004[,c('region')]
r_2004 <- unique(r_2004)

r_2004$ri_df <- 1
temp_2004 <- merge(r_2004, list_2006, by="region", all=T)
# classified in NUTS 2006

# first recode straightforward regions to NUTS 2010
# straightforward changes 2006/2010:
# IT, UK, DE

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_2004$region, X2006_2010$code_2006)
df_2004$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

temp_2004 <- df_2004[,c('region')]
temp_2004 <- unique(temp_2004)
temp_2004$ri_df <- 1
temp_2004 <- merge(temp_2004, list_2010, by="region", all=T)
temp_2004 <- temp_2004[rowSums(is.na(temp_2004)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004")))

##### 2002-2003 #####

df_2002_2003 <- subset(df_2, year==2003 | year==2002)
length(unique(df_2002_2003$region))
r_2002_2003 <- df_2002_2003[,c('region')]
r_2002_2003 <- unique(r_2002_2003)

r_2002_2003$ri_df <- 1
temp_2002_2003 <- merge(r_2002_2003, list_2006, by="region", all=T)
# classified in NUTS 2006

# first recode straightforward regions to NUTS 2010
# straightforward changes 2006/2010:
# IT, UK, DE

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_2002_2003$region, X2006_2010$code_2006)
df_2002_2003$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

temp_2002_2003 <- df_2002_2003[,c('region')]
temp_2002_2003 <- unique(temp_2002_2003)
temp_2002_2003$ri_df <- 1
temp_2002_2003 <- merge(temp_2002_2003, list_2010, by="region", all=T)
temp_2002_2003 <- temp_2002_2003[rowSums(is.na(temp_2002_2003)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003")))

##### 2001 #####

df_2001 <- subset(df_2, year==2001)
length(unique(df_2001$region))
r_2001 <- df_2001[,c('region')]
r_2001 <- unique(r_2001)

r_2001$ri_df <- 1
temp_2001 <- merge(r_2001, list_2006, by="region", all=T)
# classified in NUTS 2006 + there is DEB0 instead of DEB1 + DEB2 + DEB3

# first recode straightforward regions to NUTS 2010
# straightforward changes 2006/2010:
# IT, UK, DE

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_2001$region, X2006_2010$code_2006)
df_2001$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

temp_2001 <- df_2001[,c('region')]
temp_2001 <- unique(temp_2001)
temp_2001$ri_df <- 1
temp_2001 <- merge(temp_2001, list_2010, by="region", all=T)
temp_2001 <- temp_2001[rowSums(is.na(temp_2001)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2001", "temp_2001")))

##### 2000 #####

df_2000 <- subset(df_2, year==2000)
length(unique(df_2000$region))
r_2000 <- df_2000[,c('region')]
r_2000 <- unique(r_2000)

r_2000$ri_df <- 1
temp_2000 <- merge(r_2000, list_2006, by="region", all=T)
# classified in NUTS 2006 + there is DEB0 instead of DEB1 + DEB2 + DEB3

# first recode straightforward regions to NUTS 2010
# straightforward changes 2006/2010:
# IT, UK, DE

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_2000$region, X2006_2010$code_2006)
df_2000$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

temp_2000 <- df_2000[,c('region')]
temp_2000 <- unique(temp_2000)
temp_2000$ri_df <- 1
temp_2000 <- merge(temp_2000, list_2010, by="region", all=T)
temp_2000 <- temp_2000[rowSums(is.na(temp_2000)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2001", "temp_2001", "df_2000", "temp_2000")))

##### 1999 #####

df_1999 <- subset(df_1, year==1999)
length(unique(df_1999$region))
r_1999 <- df_1999[,c('region')]
r_1999 <- unique(r_1999)

r_1999$ri_df <- 1
temp_1999 <- merge(r_1999, list_2006, by="region", all=T)
# classified in NUTS 2006 + changes:
# DED0 instead of DED1 + DED2 + DED3
# UKM1 instead of UKM5 and UKM4 instead of UKM6

# first recode straightforward regions to NUTS 2010
# straightforward changes 2006/2010:
# IT, UK, DE

X2003_2006 <- read_excel("../original_data/NUTS/2003-2006.xls", sheet = "Sheet1")

# replace the 2003 names with the 2006 names
inds <- match(df_1999$region, X2003_2006$code_2003)
df_1999$region[!is.na(inds)] <- X2003_2006$code_2006[na.omit(inds)]

# splits: DE, FI
X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_1999$region, X2006_2010$code_2006)
df_1999$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

temp_1999 <- df_1999[,c('region')]
temp_1999 <- unique(temp_1999)
temp_1999$ri_df <- 1
temp_1999 <- merge(temp_1999, list_2010, by="region", all=T)
temp_1999 <- temp_1999[rowSums(is.na(temp_1999)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2000", "temp_2000", "df_2001", "temp_2001", "df_1999", "temp_1999")))

##### 1998 #####

df_1998 <- subset(df_1, year==1998)
length(unique(df_1998$region))
r_1998 <- df_1998[,c('region')]
r_1998 <- unique(r_1998)

r_1998$ri_df <- 1
temp_1998 <- merge(r_1998, list_2003, by="region", all=T)
temp_1998 <- merge(temp_1998, list_2003, by="region", all=T)
temp_1998 <- merge(temp_1998, list_2006, by="region", all=T)
# classified in NUTS 1995 and NUTS 1999 ???

# first recode straightforward regions from NUTS 1995 --> 1999 --> 2003 --> 2006 --> 2010

X1995_1999 <- read_excel("../original_data/NUTS/1995-1999.xls", sheet = "Sheet1")

# replace the 1995 names with the 1999 names
inds <- match(df_1998$region, X1995_1999$code_1995)
df_1998$region[!is.na(inds)] <- X1995_1999$code_1999[na.omit(inds)]

X1999_2003 <- read_excel("../original_data/NUTS/1999-2003.xls", sheet = "Sheet1")

# replace the 1999 names with the 2003 names
inds <- match(df_1998$region, X1999_2003$code_1999)
df_1998$region[!is.na(inds)] <- X1999_2003$code_2003[na.omit(inds)]

X2003_2006 <- read_excel("../original_data/NUTS/2003-2006.xls", sheet = "Sheet1")

# replace the 2003 names with the 2006 names
inds <- match(df_1998$region, X2003_2006$code_2003)
df_1998$region[!is.na(inds)] <- X2003_2006$code_2006[na.omit(inds)]

X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_1998$region, X2006_2010$code_2006)
df_1998$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

# discard FI14 which split to 3 NUTS3 regions in 1999-2003
df_1998 <- subset(df_1998, region!="FI14")

temp_1998 <- df_1998[,c('region')]
temp_1998 <- unique(temp_1998)
temp_1998$ri_df <- 1
temp_1998 <- merge(temp_1998, list_2010, by="region", all=T)
temp_1998 <- temp_1998[rowSums(is.na(temp_1998)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2000", "temp_2000", "df_2001", "temp_2001", "df_1999", "temp_1999", "df_1998", "temp_1998")))

##### 1996-1997 #####

df_1996_1997 <- subset(df_1, year==1996 | year==1997)
length(unique(df_1996_1997$region))
r_1996_1997 <- df_1996_1997[,c('region')]
r_1996_1997 <- unique(r_1996_1997)

r_1996_1997$ri_df <- 1
temp_1996_1997 <- merge(r_1996_1997, list_2003, by="region", all=T)
temp_1996_1997 <- merge(temp_1996_1997, list_2003, by="region", all=T)
temp_1996_1997 <- merge(temp_1996_1997, list_2006, by="region", all=T)
# coded like 1998 but NO00 instead of NUTS2

# first recode straightforward regions from NUTS 1995 --> 1999 --> 2003 --> 2006 --> 2010

X1995_1999 <- read_excel("../original_data/NUTS/1995-1999.xls", sheet = "Sheet1")

# replace the 1995 names with the 1999 names
inds <- match(df_1996_1997$region, X1995_1999$code_1995)
df_1996_1997$region[!is.na(inds)] <- X1995_1999$code_1999[na.omit(inds)]

X1999_2003 <- read_excel("../original_data/NUTS/1999-2003.xls", sheet = "Sheet1")

# replace the 1999 names with the 2003 names
inds <- match(df_1996_1997$region, X1999_2003$code_1999)
df_1996_1997$region[!is.na(inds)] <- X1999_2003$code_2003[na.omit(inds)]

X2003_2006 <- read_excel("../original_data/NUTS/2003-2006.xls", sheet = "Sheet1")

# replace the 2003 names with the 2006 names
inds <- match(df_1996_1997$region, X2003_2006$code_2003)
df_1996_1997$region[!is.na(inds)] <- X2003_2006$code_2006[na.omit(inds)]

X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")

# replace the 2006 names with the 2010 names
inds <- match(df_1996_1997$region, X2006_2010$code_2006)
df_1996_1997$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]

# discard FI14 which split to 3 NUTS3 regions in 1999-2003
df_1996_1997 <- subset(df_1996_1997, region!="FI14")

temp_1996_1997 <- df_1996_1997[,c('region')]
temp_1996_1997 <- unique(temp_1996_1997)
temp_1996_1997$ri_df <- 1
temp_1996_1997 <- merge(temp_1996_1997, list_2010, by="region", all=T)
temp_1996_1997 <- temp_1996_1997[rowSums(is.na(temp_1996_1997)) > 0,]

rm(list=setdiff(ls(), c("df_1", "df_2", "list_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2000", "temp_2000", "df_2001", "temp_2001", "df_1999", "temp_1999", "df_1998", "temp_1998", "df_1996_1997", "temp_1996_1997")))

# ##### 1994-1995 #####
# 
# df_1994_1995 <- subset(df_1, year==1995 | year==1994)
# length(unique(df_1994_1995$region))
# r_1994_1995 <- df_1994_1995[,c('region')]
# r_1994_1995 <- unique(r_1994_1995)
# 
# r_1994_1995$ri_df <- 1
# temp_1994_1995 <- merge(r_1994_1995, list_2003, by="region", all=T)
# temp_1994_1995 <- merge(temp_1994_1995, list_2003, by="region", all=T)
# temp_1994_1995 <- merge(temp_1994_1995, list_2006, by="region", all=T)
# # coded like 1998 but NO00 instead of NUTS2
# 
# # first recode straightforward regions from NUTS 1995 --> 1999 --> 2003 --> 2006 --> 2010
# 
# X1995_1999 <- read_excel("../original_data/NUTS/1995-1999.xls", sheet = "Sheet1")
# 
# # replace the 1995 names with the 1999 names
# inds <- match(df_1994_1995$region, X1995_1999$code_1994_1995)
# df_1994_1995$region[!is.na(inds)] <- X1995_1999$code_1999[na.omit(inds)]
# 
# X1999_2003 <- read_excel("../original_data/NUTS/1999-2003.xls", sheet = "Sheet1")
# 
# # replace the 1999 names with the 2003 names
# inds <- match(df_1994_1995$region, X1999_2003$code_1999)
# df_1994_1995$region[!is.na(inds)] <- X1999_2003$code_2003[na.omit(inds)]
# 
# X2003_2006 <- read_excel("../original_data/NUTS/2003-2006.xls", sheet = "Sheet1")
# 
# # replace the 2003 names with the 2006 names
# inds <- match(df_1994_1995$region, X2003_2006$code_2003)
# df_1994_1995$region[!is.na(inds)] <- X2003_2006$code_2006[na.omit(inds)]
# 
# X2006_2010 <- read_excel("../original_data/NUTS/2006-2010.xls", sheet = "Sheet1")
# 
# # replace the 2006 names with the 2010 names
# inds <- match(df_1994_1995$region, X2006_2010$code_2006)
# df_1994_1995$region[!is.na(inds)] <- X2006_2010$code_2010[na.omit(inds)]
# 
# # discard FI14 which split to 3 NUTS3 regions in 1999-2003
# df_1994_1995 <- subset(df_1994_1995, region!="FI14")
# 
# temp_1994_1995 <- df_1994_1995[,c('region')]
# temp_1994_1995 <- unique(temp_1994_1995)
# temp_1994_1995$ri_df <- 1
# temp_1994_1995 <- merge(temp_1994_1995, list_2010, by="region", all=T)
# temp_1994_1995 <- temp_1994_1995[rowSums(is.na(temp_1994_1995)) > 0,]
# 
# rm(list=setdiff(ls(), c("df_1", "df_2", "list_1994_1995", "list_1999", "list_2003", "list_2006", "list_2010", "df_2005_2007", "df_2004", "temp_2004", "df_2002_2003", "temp_2002_2003", "df_2001", "temp_2001", "df_1999", "temp_1999", "df_1998", "temp_1998", "df_1996_1997", "temp_1996_1997", "df_1994_1995", "temp_1994_1995")))

# huge aggregation for years 1994-1995 --> try to reclassify regions for 1997-2007 only and then extend the trend to 1994-1995

# merge datasets

df_merged <- rbind(df_1996_1997, df_1998, df_1999, df_2000, df_2001, df_2002_2003, df_2004, df_2005_2007)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
unique(df_merged$year)

rm(list=setdiff(ls(), c("df_merged")))

############### 2. RECLASSIFY 1994-2007 YEARS FOR THE EMPLOYMENT DATA FILES 1986-1999 and 2000-2017 ############### 

# CASE 1

case_temp <- subset(df_merged, (region=="DE41" | region=="DE42") & year==2004)
case_temp_neq <- setdiff(df_merged, case_temp)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DE40"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 2

case_temp <- subset(df_merged, (region=="FI13" | region=="FI1A") & year<2005)
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "FI1D"
case_temp$country <- "FI"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 3

case_temp <- subset(df_merged, (region=="FI1B" | region=="FI1C") & year>2004)
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "FI18"
case_temp$country <- "FI"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 4

case_temp <- subset(df_merged, (region=="DEE1" | region=="DEE2" | region=="DEE3") & year<2005)
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DEE0"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 5

case_temp <- subset(df_merged, (region=="DEB1" | region=="DEB2" | region=="DEB3") & (year!=2001 & year!=2000))
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DEB0"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 6

case_temp <- subset(df_merged, (region=="DED2" | region=="DED4" | region=="DED5") & year>1999)
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "DED0"
case_temp$country <- "DE"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 7

case_temp <- subset(df_merged, (region=="ITH1" | region=="ITH2") & year>1998)
case_temp_neq <- setdiff(df_merged, case_temp)

unique(case_temp$year)
unique(case_temp_neq$year)

case_temp <- aggregate(case_temp$value, by=list(year=case_temp$year, na112d=case_temp$na112d), FUN=sum)
case_temp$region <- "IT31"
case_temp$country <- "IT"
colnames(case_temp)[3] <- "value"
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))

# CASE 8

weights <- subset(df_merged, (region=="NO01" | region=="NO02" | region=="NO03" | region=="NO04" | region=="NO05" | region=="NO06" | region=="NO07") & year==1998)
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

case_temp <- subset(df_merged, (region=="NO00") & year<1998)
case_temp_neq <- subset(df_merged, !((region=="NO00") & year<1998))

case_temp <- rbind(case_temp, case_temp, case_temp, case_temp, case_temp, case_temp, case_temp)
case_temp <- case_temp[with(case_temp, order(region, na112d, year)),]
row.names(case_temp) <- NULL
case_temp$region <- rep(c("NO01", "NO02", "NO03", "NO04", "NO05", "NO06", "NO07"), 116)
case_temp <- merge(case_temp, wg, by=c("region", "na112d"), all.x=T)
case_temp <- case_temp[with(case_temp, order(region, year, na112d)),]
row.names(case_temp) <- NULL

case_temp <- case_temp %>% mutate_all(~ifelse(is.nan(.), NA, .))
case_temp$share <- as.numeric(case_temp$share)
case_temp$value <- case_temp$value*case_temp$share  
case_temp <- case_temp[,c("country", "region", "year", "na112d", "value")]

df_merged <- rbind(case_temp, case_temp_neq)
df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
rm(list=setdiff(ls(), c("df_merged")))
row.names(df_merged) <- NULL

############### 3. RECODE NACE TO ISIC ###############   

# 1. Do a linear interpolation in groups on original NACE codes before recoding to ISIC (to later match robot data).

unique(df_merged$year)
unique(df_merged$na112d)

# for each industry, do a linear interpolation before summing
df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(df_merged$region)){
  for (industry_ in unique(df_merged$na112d)){
    temp <- df_merged %>% filter(region==region_ & na112d==industry_)
    if ((colSums(is.na(temp)))[5]==nrow(temp) | (colSums(is.na(temp)))[5]==nrow(temp)-1){
      df_1 <- rbind(df_1, temp)
    }else{
      temp$value <- na_interpolation(temp$value, option = "linear")
      df_2 <- rbind(df_2, temp)
    }
  }
}

df_merged <- rbind(df_1, df_2)

df_merged <- df_merged[with(df_merged, order(region, na112d, year)),]
row.names(df_merged) <- NULL
rm(list=setdiff(ls(), c("df_merged")))

# 2. Recode NACE to ISIC & KEEP ONLY MANUFACTURING INDUSTRIES

df_merged <- df_merged %>% mutate(
  industry = case_when(
    na112d=="30" | na112d=="37" | na112d=="23" ~ "All other manufacturing branches/other chemical products n.e.c",
    
    na112d=="34" | na112d=="35" ~ "Automotive/Other vehicles",
    
    na112d=="27" ~ "Basic metals",
    
    na112d=="45" ~ "Construction",
    
    na112d=="31" | na112d=="32" | na112d=="33" ~ "Electrical/electronics",
    
    na112d=="40" | na112d=="41" ~ "Electricity, gas, water supply",
    
    na112d=="15" | na112d=="16" ~ "Food and beverages",
    
    na112d=="26" ~ "Glass, ceramics, stone, mineral products (non-automotive)",
    
    na112d=="29" ~ "Industrial machinery",
    
    na112d=="28" ~ "Metal products (non-automotive)",
    
    na112d=="10" | na112d=="11" | na112d=="12" | na112d=="13" | na112d=="14" ~ "Mining and quarrying",
    
    na112d=="21" | na112d=="22" ~ "Paper",
    
    na112d=="24" ~ "Pharmaceuticals, cosmetics",
    
    na112d=="25" ~ "Rubber and plastic products (non-automotive)",
    
    na112d=="17" | na112d=="18" | na112d=="19" ~ "Textiles",
    
    na112d=="20" | na112d=="36" ~ "Wood and furniture"
  )
)

#df_merged$industry[is.na(df_merged$industry)] <- "Non-manufacturing"

df_merged <- df_merged[!is.na(df_merged$industry),]
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL  

# 3. MISSINGS FOR SOME REGION-YEARS-INDUSTRIES -- DRAW A RANDOM NUMBER FROM A UNIFORM DISTRIBUTION 0-1

length(unique(df_merged$region))*length(unique(df_merged$year))*length(unique(df_merged$industry))

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

# B. IMPUTE WITH MEDIAN FOR THE EMPLOYMENT FROM IN ENTIRE COUNTRY, IN ALL INDUSTRIES & NORMALIZE TO 0-1   

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

rm(list=setdiff(ls(), c("df_merged")))
df_merged$flag2 <- NULL
df_merged$flag3 <- NULL
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL

# 5. PLOT THE TIME SERIES TO SEE IF THE IMPUTATIONS MAKE SENSE

# df_merged <- df_merged %>% mutate(
#   industry2 = case_when(
#     industry=="All other manufacturing branches/other chemical products n.e.c" ~ 1,
#     
#     industry=="Automotive/Other vehicles" ~ 2,
#     
#     industry=="Basic metals" ~ 3,
#     
#     industry=="Construction" ~ 4,
#     
#     industry=="Electrical/electronics" ~ 5,
#     
#     industry=="Electricity, gas, water supply" ~ 6,
#     
#     industry=="Food and beverages" ~ 7,
#     
#     industry=="Glass, ceramics, stone, mineral products (non-automotive)" ~ 8,
#     
#     industry=="Industrial machinery" ~ 9,
#     
#     industry=="Metal products (non-automotive)" ~ 10,
#     
#     industry=="Mining and quarrying" ~ 11,
#     
#     industry=="Paper" ~ 12,
#     
#     industry=="Pharmaceuticals, cosmetics" ~ 13,
#     
#     industry=="Rubber and plastic products (non-automotive)" ~ 14,
#     
#     industry=="Textiles" ~ 15,
#     
#     industry=="Wood and furniture" ~ 16
#   )
# )
# 
# max(df_merged$value)
# min(df_merged$value)
# 
# region_plots <- list()
# 
# for (region_ in unique(df_merged$region)){
#   for (industry_ in unique(df_merged$industry2)){
#     temp <- df_merged %>% filter(region==region_ & industry2==industry_)
#     type <- max(temp$flag)
#     colort <- ifelse(type==1, "#440154FF",
#                      ifelse(type==2, "#21908CFF","#FDE725FF"))
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
#       scale_x_continuous(limits = c(1997, 2007), breaks = seq(1997, 2007, 1)) +
#       #scale_y_continuous(limits = c(0, 486)) +
#       geom_text(aes(label=round(value, digit=1)), size=4, colour="black", position=position_dodge(width=1), vjust=0)
#     #print(region_plots[[region_]])
#     ggsave(region_plots[[region_]], file=paste0("all_region_plots_ts_1997_2007/", region_, "_", industry_, "_", type, ".png"))
#   }
# }
# 
# df_merged$industry2 <- NULL

# 6. PREPARE DATA FOR CALCULATION OF EXPOSURE TO ROBOTS

# check out current t0

rm(list=setdiff(ls(), c("df_merged")))

df <- data.frame()

for (region_ in unique(df_merged$region)){
  for (industry_ in unique(df_merged$industry)){
    temp <- df_merged %>% filter(region==region_ & industry==industry_)
    if (dim(temp)!=0){
      temp$year0 <- min(temp$year)
      df <- rbind(df, temp)
    }
  }
}

unique(df$year0)
n_occur <- data.frame(table(df$year0))
# 191 observations have t0 later than 1996 --> discard them

df <- subset(df, year0==1996)

df_merged <- df[,c(-7)]
rm(list=setdiff(ls(), c("df_merged")))

# add NA for two more years in each series (1994-1995)

df <- data.frame()

for (region_ in unique(df_merged$region)){
  for (industry_ in unique(df_merged$industry)){
    temp <- df_merged %>% filter(region==region_ & industry==industry_)
    if (dim(temp)!=0){
      temp_1 <- subset(temp, year==1996)
      temp_1$year <- 1994
      temp_2 <- subset(temp, year==1996)
      temp_2$year <- 1995
      df <- rbind(df, temp_1, temp_2)
    }
  }
}

df$value <- NA

df_merged <- rbind(df, df_merged)
df_merged <- df_merged[with(df_merged, order(region, industry, year)),]
row.names(df_merged) <- NULL
rm(list=setdiff(ls(), c("df_merged")))

# do polynomial interpolation for years 1994 and 1995

df <- data.frame()

for (region_ in unique(df_merged$region)){
  for (industry_ in unique(df_merged$industry)){
    temp <- df_merged %>% filter(region==region_ & industry==industry_)
    temp$value <- na_interpolation(temp$value, option = "spline")
    temp$value[temp$value<0] <- NA
    temp$value <- na_interpolation(temp$value, option = "linear")
    df <- rbind(df, temp)
  }
}

df_merged <- df
rm(list=setdiff(ls(), c("df_merged")))

# check out quality of the interpolation

# df_merged <- df_merged %>% mutate(
#   industry2 = case_when(
#     industry=="All other manufacturing branches/other chemical products n.e.c" ~ 1,
# 
#     industry=="Automotive/Other vehicles" ~ 2,
# 
#     industry=="Basic metals" ~ 3,
# 
#     industry=="Construction" ~ 4,
# 
#     industry=="Electrical/electronics" ~ 5,
# 
#     industry=="Electricity, gas, water supply" ~ 6,
# 
#     industry=="Food and beverages" ~ 7,
# 
#     industry=="Glass, ceramics, stone, mineral products (non-automotive)" ~ 8,
# 
#     industry=="Industrial machinery" ~ 9,
# 
#     industry=="Metal products (non-automotive)" ~ 10,
# 
#     industry=="Mining and quarrying" ~ 11,
# 
#     industry=="Paper" ~ 12,
# 
#     industry=="Pharmaceuticals, cosmetics" ~ 13,
# 
#     industry=="Rubber and plastic products (non-automotive)" ~ 14,
# 
#     industry=="Textiles" ~ 15,
# 
#     industry=="Wood and furniture" ~ 16
#   )
# )
# 
# max(df_merged$value)
# min(df_merged$value)
# 
# region_plots <- list()
# 
# for (region_ in unique(df_merged$region)){
#   for (industry_ in unique(df_merged$industry2)){
#     temp <- df_merged %>% filter(region==region_ & industry2==industry_)
#     type <- max(temp$flag)
#     colort <- ifelse(type==1, "#440154FF",
#                      ifelse(type==2, "#21908CFF","#FDE725FF"))
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
#       scale_x_continuous(limits = c(1994, 2007), breaks = seq(1994, 2007, 1)) +
#       #scale_y_continuous(limits = c(0, 486)) +
#       geom_text(aes(label=round(value, digit=1)), size=4, colour="black", position=position_dodge(width=1), vjust=0)
#     #print(region_plots[[region_]])
#     ggsave(region_plots[[region_]], file=paste0("1994_2007_interpolation/", region_, "_", industry_, "_", type, ".png"))
#   }
# }
# 
# df_merged$industry2 <- NULL

# extract t0==1994

df_merged$flag <- NULL

df_merged_1994 <- subset(df_merged, year==1994)
unique(df_merged_1994$year)
colnames(df_merged_1994)[5] <- "employment_1994"
df_merged_1994 <- df_merged_1994[,c("region", "industry", "employment_1994")]

df_merged_1997_2007 <- subset(df_merged, year>1996)  
unique(df_merged_1997_2007$year)
colnames(df_merged_1997_2007)[5] <- "employment"  

employment_1997_2007 <- merge(df_merged_1997_2007, df_merged_1994, by=c("region", "industry"), all.x = T)
na <- employment_1997_2007[rowSums(is.na(employment_1997_2007)) > 0,]


  # recode the straightforward regions from 2010 to 2013 (EL, FR overseas territories (already in 2013/2016 in "df"), SI (lack in robot data)), EL only reclassified
  # + problematic splits in London UK (later)
  
  X2010_2013 <- read_excel("../original_data/NUTS/2010-2013.xls", sheet = "Sheet1")
  
  # replace the 2010 names with the 2013 names
  inds <- match(employment_1997_2007$region, X2010_2013$code_2010)
  employment_1997_2007$region[!is.na(inds)] <- X2010_2013$code_2013[na.omit(inds)]
  
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
  inds <- match(employment_1997_2007$region, X2013_2016$code_2013)
  employment_1997_2007$region[!is.na(inds)] <- X2013_2016$code_2016[na.omit(inds)]

# check differences between 1997-2007 and regions in 2007-2017 dataset for the same countries
load("../generated_data/df_merged_final_2007_2017.RData")
rm(list=setdiff(ls(), c("employment_1997_2007", "employment_2007_2017")))

countries <- unique(employment_1997_2007$country)

regions_1 <- employment_1997_2007[,c('region', 'country')]
regions_1 <- unique(regions_1)

regions_2 <- employment_2007_2017[,c('region', 'country')]
regions_2 <- unique(regions_2)
regions_2 <- regions_2[regions_2$country %in% countries,]

regions_1$r1 <- 1
regions_2$r2 <- 1

temp <- merge(regions_1, regions_2, by=c("region", "country"), all = T)
temp <- temp[rowSums(is.na(temp)) > 0,]

write.csv(temp, file = "region_diff.csv")

# no NAs --> save the complete dataset, ready for EXPOSURE TO ROBOTS calculation  
save(employment_1997_2007, file="../generated_data/df_merged_final_1997_2007.RData")

length(unique(employment_1997_2007$region))
length(unique(employment_1997_2007$industry))

# 159 regions, 16 industries  