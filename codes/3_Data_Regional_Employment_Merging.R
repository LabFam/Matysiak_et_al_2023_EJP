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

load("../generated_data/df_merged_final_2007_2017.RData")
load("../generated_data/df_merged_final_1997_2007.RData")

unique(employment_1997_2007$country)
employment_1997_2007 <- subset(employment_1997_2007, country!="BE")
unique(employment_1997_2007$country)

length(unique(employment_2007_2017$country))
pioneers <- c("DE", "ES", "FR", "IT", "UK", "FI", "NO", "SE")
pioneers_2007_2017 <- employment_2007_2017[employment_2007_2017$country %in% pioneers,]

# separate non_pioneers and save the file for them
non_pioneers_2007_2017 <- setdiff(employment_2007_2017, pioneers_2007_2017)

length(unique(pioneers_2007_2017$country))
length(unique(non_pioneers_2007_2017$country))

non_pioneers_2007_2017$employment <- NULL
save(non_pioneers_2007_2017, file = "../generated_data/non_pioneers_2007_2017.RData")

pioneers_1997_2007 <- employment_1997_2007
rm(list=setdiff(ls(), c("pioneers_1997_2007", "pioneers_2007_2017")))

pioneers_1997_2007$employment <- NULL
pioneers_2007_2017$employment <- NULL

# compare regions in the two files

regions_df_1 <- pioneers_1997_2007[,c("region", "year")]
regions_df_1$year <- NULL
regions_df_1 <- unique(regions_df_1)
regions_df_1$list <- 1

regions_df_2 <- pioneers_2007_2017[,c("region", "year")]
regions_df_2$year <- NULL
regions_df_2 <- unique(regions_df_2)
regions_df_2$list <- 1

temp <- merge(regions_df_1, regions_df_2, by = "region", all = T)

# SUM SOME NEW REGIONS TO OLD REGIONS

# (1) DEB0 = (2) DEB1 + DEB2 + DEB3
case_temp <- subset(pioneers_2007_2017, region=="DEB1" | region=="DEB2" | region=="DEB3")
case_temp_neq <- setdiff(pioneers_2007_2017, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(case_temp$employment_2004, by = list(case_temp$industry, case_temp$country, case_temp$year), sum)
colnames(case_temp) <- cn
case_temp$region <- "DEB0"
case_temp <- case_temp[,c("region", "industry", "country", "year", "employment_2004")]

pioneers_2007_2017 <- rbind(case_temp, case_temp_neq)
pioneers_2007_2017 <- pioneers_2007_2017[with(pioneers_2007_2017, order(region, year)),]
row.names(pioneers_2007_2017) <- NULL

# (1) DED0 = (2) DED2 + DED4 + DED5
case_temp <- subset(pioneers_2007_2017, region=="DED2" | region=="DED4" | region=="DED5")
case_temp_neq <- setdiff(pioneers_2007_2017, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(case_temp$employment_2004, by = list(case_temp$industry, case_temp$country, case_temp$year), sum)
colnames(case_temp) <- cn
case_temp$region <- "DED0"
case_temp <- case_temp[,c("region", "industry", "country", "year", "employment_2004")]

pioneers_2007_2017 <- rbind(case_temp, case_temp_neq)
pioneers_2007_2017 <- pioneers_2007_2017[with(pioneers_2007_2017, order(region, year)),]
row.names(pioneers_2007_2017) <- NULL

# (1) IT31 = (2) ITH1 + ITH2
case_temp <- subset(pioneers_2007_2017, region=="ITH1" | region=="ITH2")
case_temp_neq <- setdiff(pioneers_2007_2017, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(case_temp$employment_2004, by = list(case_temp$industry, case_temp$country, case_temp$year), sum)
colnames(case_temp) <- cn
case_temp$region <- "IT31"
case_temp <- case_temp[,c("region", "industry", "country", "year", "employment_2004")]

pioneers_2007_2017 <- rbind(case_temp, case_temp_neq)
pioneers_2007_2017 <- pioneers_2007_2017[with(pioneers_2007_2017, order(region, year)),]
row.names(pioneers_2007_2017) <- NULL

# (1) FI18 = (2) FI1B + FI1C
case_temp <- subset(pioneers_2007_2017, region=="FI1B" | region=="FI1C")
case_temp_neq <- setdiff(pioneers_2007_2017, case_temp)
cn <- colnames(case_temp)
cn <- cn[-1]

case_temp <- aggregate(case_temp$employment_2004, by = list(case_temp$industry, case_temp$country, case_temp$year), sum)
colnames(case_temp) <- cn
case_temp$region <- "FI18"
case_temp <- case_temp[,c("region", "industry", "country", "year", "employment_2004")]

pioneers_2007_2017 <- rbind(case_temp, case_temp_neq)
pioneers_2007_2017 <- pioneers_2007_2017[with(pioneers_2007_2017, order(region, year)),]
row.names(pioneers_2007_2017) <- NULL

# compare regions in the two files AGAIN

regions_df_1 <- pioneers_1997_2007[,c("region", "year")]
regions_df_1$year <- NULL
regions_df_1 <- unique(regions_df_1)
regions_df_1$list <- 1

regions_df_2 <- pioneers_2007_2017[,c("region", "year")]
regions_df_2$year <- NULL
regions_df_2 <- unique(regions_df_2)
regions_df_2$list <- 1

temp <- merge(regions_df_1, regions_df_2, by = "region", all = T)
# all fine, can merge now

pioneers_2007_2017$employment_2004 <- NULL

pioneers_1997_2017 <- merge(pioneers_1997_2007, pioneers_2007_2017, all = T)
rm(list=setdiff(ls(), c("pioneers_1997_2017")))

# carry on obs for employment_1994

df_1 <- data.frame()
df_2 <- data.frame()

for (region_ in unique(pioneers_1997_2017$region)){
  for (industry_ in unique(pioneers_1997_2017$industry)){
    temp <- pioneers_1997_2017 %>% filter(region==region_ & industry==industry_)
    if (!((colSums(is.na(temp)))[5]==nrow(temp) | (colSums(is.na(temp)))[5]==nrow(temp)-1)){
      temp$employment_1994 <- na_interpolation(temp$employment_1994, option = "linear")
      df_1 <- rbind(df_1, temp)
    }else{
      df_2 <- rbind(df_2, temp)
    }
  }
}

pioneers_1997_2017 <- df_1
save(pioneers_1997_2017, file = "../generated_data/pioneers_1997_2017.RData")