# Matysiak_et_al_2023_EJP

This repository contains codes used to create the analysis for the paper:
Matysiak, A., Bellani, D., & Bogusz, H. (2023). Industrial Robots and Regional Fertility in European Countries. *European Journal of Population* 39(11). https://doi.org/10.1332/25151088Y2024D000000024

It contains the following folders:

codes;\
final_data;\
generated_data;\
original_data;\
plots;\
tables.

The folder 'codes' contains eight Stata/R files. They should be run from 1 to 8 to generate all plots (folder 'plots') and tables (folder 'tables') displayed in the paper. The tables were manually summarized in the file '0_SUMMARY.xlsx' (folder 'tables'), which contains the final tables displayed in the publication.

The folder 'original_data' contains original data files used for the analysis. We publish the data which is publicly available at Eurostat (https://ec.europa.eu/eurostat/data/database) in the repository.

The file 'ifr.csv', which contains robot stocks obtained from International Federation of Robotics, is not in the repository. The access to that data is restricted. The data can be purchased at https://ifr.org/ and they should look like this (we replaced values with X):

![data_ifr](https://github.com/LabFam/Matysiak_et_al_2023_EJP/assets/56295276/c8d2f3c2-c4ab-4b40-a055-96ff87d97987)

The files 'ESTA51546_200916_02.dta' and 'ESTA51546_200916_03' with regional data structures (by NUTS and NACE industry) are not in the repository. These data are not publicly available and they need to be ordered at Eurostat. We ordered them with the following request:

<img width="1295" alt="Schermata 2022-02-25 alle 20 11 09" src="https://github.com/LabFam/Matysiak_et_al_2023_EJP/assets/56295276/ddf0b465-25b7-40ca-8c4f-68711b5caa5f">

The data should look like this:

![data_empl](https://github.com/LabFam/Matysiak_et_al_2023_EJP/assets/56295276/b490e25e-4ad8-4014-8ff9-86299beaaac1)

The folder 'generated_data' contains intermediate data files, while the folder 'final_data' contains two generated data files used for the analysis.
