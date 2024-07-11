/* Copyright (c) 2023 Honorata Bogusz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OTHERWISE, ARISING FROM,
OUT OF IN CONNECTION WITH THE SOFTWARE THE USE OTHER DEALINGS IN THE
SOFTWARE.*/

clear all

use "../final_data/pioneer_1997_2017_modelling.dta", clear

egen nr_r=group(region)

xtset nr_r year, yearly

g share_pop_1524=pop_tot_1524/pop_tot
g share_pop_2534=pop_tot_2534/pop_tot
g share_pop_3549=pop_tot_3549/pop_tot
g share_pop_50plus=pop_tot_50plus/pop_tot

g share_pop_2549=(pop_tot_2534+pop_tot_3549)/pop_tot
g share_pop_1549=(pop_tot_1524+pop_tot_2534+pop_tot_3549)/pop_tot

g share_fem_1524=pop_fem_1524/pop_fem
g share_fem_2534=pop_fem_2534/pop_fem
g share_fem_3549=pop_fem_3549/pop_fem
g share_fem_50plus=pop_fem_50plus/pop_fem
g share_fem_2549=(pop_fem_2534+pop_fem_3549)/pop_fem
g share_fem_1549=(pop_fem_1524+pop_fem_2534+pop_fem_3549)/pop_fem
g pop_female_ratio=pop_fem/pop_tot

g ratio_fem_1549=(pop_fem_1524+pop_fem_2534+pop_fem_3549)/(pop_male_1524+pop_male_2534+pop_male_3549)
g ratio_fem_2549=(pop_fem_2534+pop_fem_3549)/(pop_male_2534+pop_male_3549)
g ratio_fem_2549_sq=ratio_fem_2549*ratio_fem_2549

g ratio_edu=edu_female_isced58/edu_male_isced58
g ratio_edu_sq=ratio_edu*ratio_edu
recode ratio_edu (min/1=1) (1/max=2), g(ratio_hedu)
g edu_total_isced04 = edu_total_isced02+edu_total_isced34

g ratio_industry=female_industry/male_industry
g ratio_findustry=female_industry/all_industry

sort nr_r year
by nr_r: g spell=_n
g x=all_industry if spell==1
by nr_r: egen all_industry2=min(x)
replace all_industry2=100-all_industry2

g y=ratio_industry if spell==1
by nr_r: egen ratio_industry2=min(y)

g yy=female_industry if spell==1
by nr_r: egen female_industry2=min(yy)

g yyy=male_industry if spell==1
by nr_r: egen male_industry2=min(yyy)

g z=ratio_findustry if spell==1
by nr_r: egen ratio_findustry2=min(z)

* lagging

sort country nr_r

g tfr2=tfr[_n+1] if nr_r==nr_r[_n+1]

g fr2_2024=fr_2024[_n+1] if nr_r==nr_r[_n+1]
g fr2_2529=fr_2529[_n+1] if nr_r==nr_r[_n+1]
g fr2_3034=fr_3034[_n+1] if nr_r==nr_r[_n+1]
g fr2_3539=fr_3539[_n+1] if nr_r==nr_r[_n+1]
g fr2_4044=fr_4044[_n+1] if nr_r==nr_r[_n+1]
g fr2_45plus=fr_45plus[_n+1] if nr_r==nr_r[_n+1]
g exposure_to_robots2=exposure_to_robots[_n+1] if nr_r==nr_r[_n+1]

drop if year==2017
replace year=year+1
drop tfr fr_2024  fr_2529 fr_3034 fr_3539 fr_4044 fr_45plus exposure_to_robots
rename tfr2 tfr
rename fr2_2024 fr_2024
rename fr2_2529 fr_2529
rename fr2_3034 fr_3034
rename fr2_3539 fr_3539
rename fr2_4044 fr_4044
rename fr2_45plus fr_45plus
rename exposure_to_robots2 exposure_to_robots

gen interaction1 = exposure_to_robots*all_industry
gen interaction11 = exposure_to_robots*all_industry2
gen interaction2 = exposure_to_robots*ratio_industry
gen interaction22 = exposure_to_robots*ratio_industry2
gen interaction23 = exposure_to_robots*female_industry
gen interaction24 = exposure_to_robots*male_industry
gen interaction25 = exposure_to_robots*female_industry2
gen interaction26 = exposure_to_robots*male_industry2
gen interaction3 = exposure_to_robots*edu_total_isced58
gen interaction4 = exposure_to_robots*youth_unempl_rate_tot
gen interaction5 = exposure_to_robots*empl_techn_tot

gen vint_uk1 = instrument_uk*all_industry
gen vint_fr1 = instrument_fr*all_industry
gen vint_es1 = instrument_es*all_industry
gen vint_it1 = instrument_it*all_industry
gen vint_se1 = instrument_se*all_industry
gen vint_no1 = instrument_no*all_industry
gen vint_fi1 = instrument_fi*all_industry
gen vint_de1 = instrument_de*all_industry

gen vint_uk11 = instrument_uk*all_industry2
gen vint_fr11 = instrument_fr*all_industry2
gen vint_es11 = instrument_es*all_industry2
gen vint_it11 = instrument_it*all_industry2
gen vint_se11 = instrument_se*all_industry2
gen vint_no11 = instrument_no*all_industry2
gen vint_fi11 = instrument_fi*all_industry2
gen vint_de11 = instrument_de*all_industry2

gen vint_uk2 = instrument_uk*ratio_industry
gen vint_fr2 = instrument_fr*ratio_industry
gen vint_es2 = instrument_es*ratio_industry
gen vint_it2 = instrument_it*ratio_industry
gen vint_se2 = instrument_se*ratio_industry
gen vint_no2 = instrument_no*ratio_industry
gen vint_fi2 = instrument_fi*ratio_industry
gen vint_de2 = instrument_de*ratio_industry

gen vint_uk22= instrument_uk*ratio_industry2
gen vint_fr22= instrument_fr*ratio_industry2
gen vint_es22= instrument_es*ratio_industry2
gen vint_it22= instrument_it*ratio_industry2
gen vint_se22= instrument_se*ratio_industry2
gen vint_no22= instrument_no*ratio_industry2
gen vint_fi22= instrument_fi*ratio_industry2
gen vint_de22= instrument_de*ratio_industry2

gen vint_uk23 = instrument_uk*female_industry
gen vint_fr23 = instrument_fr*female_industry
gen vint_es23 = instrument_es*female_industry
gen vint_it23 = instrument_it*female_industry
gen vint_se23 = instrument_se*female_industry
gen vint_no23 = instrument_no*female_industry
gen vint_fi23 = instrument_fi*female_industry
gen vint_de23 = instrument_de*female_industry

gen vint_uk24 = instrument_uk*male_industry
gen vint_fr24 = instrument_fr*male_industry
gen vint_es24 = instrument_es*male_industry
gen vint_it24 = instrument_it*male_industry
gen vint_se24 = instrument_se*male_industry
gen vint_no24 = instrument_no*male_industry
gen vint_fi24 = instrument_fi*male_industry
gen vint_de24 = instrument_de*male_industry

gen vint_uk25 = instrument_uk*female_industry2
gen vint_fr25 = instrument_fr*female_industry2
gen vint_es25 = instrument_es*female_industry2
gen vint_it25 = instrument_it*female_industry2
gen vint_se25 = instrument_se*female_industry2
gen vint_no25 = instrument_no*female_industry2
gen vint_fi25 = instrument_fi*female_industry2
gen vint_de25 = instrument_de*female_industry2

gen vint_uk26 = instrument_uk*male_industry2
gen vint_fr26 = instrument_fr*male_industry2
gen vint_es26 = instrument_es*male_industry2
gen vint_it26 = instrument_it*male_industry2
gen vint_se26 = instrument_se*male_industry2
gen vint_no26 = instrument_no*male_industry2
gen vint_fi26 = instrument_fi*male_industry2
gen vint_de26 = instrument_de*male_industry2

gen vint_uk3 = instrument_uk*edu_total_isced58
gen vint_fr3 = instrument_fr*edu_total_isced58
gen vint_es3 = instrument_es*edu_total_isced58
gen vint_it3 = instrument_it*edu_total_isced58
gen vint_se3 = instrument_se*edu_total_isced58
gen vint_no3 = instrument_no*edu_total_isced58
gen vint_fi3 = instrument_fi*edu_total_isced58
gen vint_de3 = instrument_de*edu_total_isced58

gen vint_uk4 = instrument_uk*youth_unempl_rate_tot
gen vint_fr4 = instrument_fr*youth_unempl_rate_tot
gen vint_es4 = instrument_es*youth_unempl_rate_tot
gen vint_it4 = instrument_it*youth_unempl_rate_tot
gen vint_se4 = instrument_se*youth_unempl_rate_tot
gen vint_no4 = instrument_no*youth_unempl_rate_tot
gen vint_fi4 = instrument_fi*youth_unempl_rate_tot
gen vint_de4 = instrument_de*youth_unempl_rate_tot

gen vint_uk5 = instrument_uk*empl_techn_tot
gen vint_fr5 = instrument_fr*empl_techn_tot
gen vint_es5 = instrument_es*empl_techn_tot
gen vint_it5 = instrument_it*empl_techn_tot
gen vint_se5 = instrument_se*empl_techn_tot
gen vint_no5 = instrument_no*empl_techn_tot
gen vint_fi5 = instrument_fi*empl_techn_tot
gen vint_de5 = instrument_de*empl_techn_tot

*** DE ****

* TFR
xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE2

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE2a

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE2b

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE2c

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE2d

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE3

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE4

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_DE5

* 20-24
xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE1

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE2

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE2a

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2   i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE2b

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE2c

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE2d

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE3

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE4

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_DE5

* 25-29
xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE1

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE2

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2    i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE2a

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE2b

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE2c

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE2d

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE3

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE4

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_DE5

* 30-34
xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE1

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE2

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE2a

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2 i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE2b

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE2c

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE2d

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE3

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE4

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_DE5

* 35-39
xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE1

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE2

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE2a

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2   i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE2b

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE2c

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE2d

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE3

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE4

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_DE5

* 40-44
xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE1

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE2

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE2a

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE2b

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE2c

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE2d

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE3

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE4

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_DE5

* 45+
xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE1

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE2

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE2a

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2   i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_fi25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE2b

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2   i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE2c

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE2d

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE3

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE4

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="DE", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_DE5

esttab tfr_DE fr_2024_DE fr_2529_DE fr_3034_DE fr_3539_DE fr_4044_DE fr_45plus_DE using "../tables/DE.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE1 fr_2024_DE1 fr_2529_DE1 fr_3034_DE1 fr_3539_DE1 fr_4044_DE1 fr_45plus_DE1 using "../tables/DE1.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE2 fr_2024_DE2 fr_2529_DE2 fr_3034_DE2 fr_3539_DE2 fr_4044_DE2 fr_45plus_DE2 using "../tables/DE2.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE2a fr_2024_DE2a fr_2529_DE2a fr_3034_DE2a fr_3539_DE2a fr_4044_DE2a fr_45plus_DE2a using "../tables/DE2a.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE2b fr_2024_DE2b fr_2529_DE2b fr_3034_DE2b fr_3539_DE2b fr_4044_DE2b fr_45plus_DE2b using "../tables/DE2b.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE2c fr_2024_DE2c fr_2529_DE2c fr_3034_DE2c fr_3539_DE2c fr_4044_DE2c fr_45plus_DE2c using "../tables/DE2c.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE2d fr_2024_DE2d fr_2529_DE2d fr_3034_DE2d fr_3539_DE2d fr_4044_DE2d fr_45plus_DE2d using "../tables/DE2d.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE3 fr_2024_DE3 fr_2529_DE3 fr_3034_DE3 fr_3539_DE3 fr_4044_DE3 fr_45plus_DE3 using "../tables/DE3.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE4 fr_2024_DE4 fr_2529_DE4 fr_3034_DE4 fr_3539_DE4 fr_4044_DE4 fr_45plus_DE4 using "../tables/DE4.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_DE5 fr_2024_DE5 fr_2529_DE5 fr_3034_DE5 fr_3539_DE5 fr_4044_DE5 fr_45plus_DE5 using "../tables/DE5.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 

***FR

* TFR

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR2

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR2a

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR2b

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR2c

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store tfr_FR2d

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR3

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR4

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_FR5

*20-24

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR1

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR2

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 ) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR2a

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR2b

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR2c

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_2024_FR2d

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR3

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR4

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_FR5

*25-29

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR1

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR2

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR2a

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR2b

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR2c

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_2529_FR2d

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR3

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR4

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_FR5

*30-34

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR1

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR2

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR2a

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR2b

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR2c

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_3034_FR2d


xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR3

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR4

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_FR5

*35-39

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR1

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR2

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR2a

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR2b

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR2c

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_3539_FR2d

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR3

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR4

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_FR5

*40-44

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR1

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR2

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR2a

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR2b

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR2c

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_4044_FR2d

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR3

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR4

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_FR5

*45+

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR1

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR2

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk25 vint_de25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR2a

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk26 vint_de26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR2b

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it  vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR2c

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk11 vint_de11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store fr_45plus_FR2d

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk3 vint_de3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR3

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk4 vint_de4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR4

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_it vint_uk5 vint_de5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5) if country=="FR", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_FR5

esttab tfr_FR fr_2024_FR fr_2529_FR fr_3034_FR fr_3539_FR fr_4044_FR fr_45plus_FR using "../tables/FR.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR1 fr_2024_FR1 fr_2529_FR1 fr_3034_FR1 fr_3539_FR1 fr_4044_FR1 fr_45plus_FR1 using "../tables/FR1.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR2 fr_2024_FR2 fr_2529_FR2 fr_3034_FR2 fr_3539_FR2 fr_4044_FR2 fr_45plus_FR2 using "../tables/FR2.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace
esttab tfr_FR2a fr_2024_FR2a fr_2529_FR2a fr_3034_FR2a fr_3539_FR2a fr_4044_FR2a fr_45plus_FR2a using "../tables/FR2a.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR2b fr_2024_FR2b fr_2529_FR2b fr_3034_FR2b fr_3539_FR2b fr_4044_FR2b fr_45plus_FR2b using "../tables/FR2b.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR2c fr_2024_FR2c fr_2529_FR2c fr_3034_FR2c fr_3539_FR2c fr_4044_FR2c fr_45plus_FR2c using "../tables/FR2c.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace
esttab tfr_FR2d fr_2024_FR2d fr_2529_FR2d fr_3034_FR2d fr_3539_FR2d fr_4044_FR2d fr_45plus_FR2d using "../tables/FR2d.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace
esttab tfr_FR3 fr_2024_FR3 fr_2529_FR3 fr_3034_FR3 fr_3539_FR3 fr_4044_FR3 fr_45plus_FR3 using "../tables/FR3.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR4 fr_2024_FR4 fr_2529_FR4 fr_3034_FR4 fr_3539_FR4 fr_4044_FR4 fr_45plus_FR4 using "../tables/FR4.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_FR5 fr_2024_FR5 fr_2529_FR5 fr_3034_FR5 fr_3539_FR5 fr_4044_FR5 fr_45plus_FR5 using "../tables/FR5.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 

***IT

* TFR

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_2549)
estimates store tfr_IT1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT2

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT2a

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT2b

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT2c

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2)
estimates store tfr_IT2d

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT3

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_IT4

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store tfr_IT5

*20-24

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT1

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT2

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT2a

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT2b

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT2c

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT2d

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT3

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_IT4

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_2024_IT5

*25-29

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT1

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT2

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT2a

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT2b

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT2c

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT2d

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT3

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_IT4

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_2529_IT5

*30-34

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT1

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT2

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT2a

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT2b

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT2c

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT2d

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT3

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_IT4

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_3034_IT5

*35-39

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT1

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT2

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT2a

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT2b

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT2c

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT2d

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT3

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_IT4

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_3539_IT5

*40-44

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT1

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT2

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT2a

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT2b

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT2c

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT2d

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT3

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_IT4

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_4044_IT5

*45+

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT1

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT2

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT2a

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_uk26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT2b

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT2c

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2 i.year (exposure_to_robots interaction11 interaction22=instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_uk22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22 ) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT2d

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT3

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_IT4

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_uk5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="IT", fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_45plus_IT5

esttab tfr_IT fr_2024_IT fr_2529_IT fr_3034_IT fr_3539_IT fr_4044_IT fr_45plus_IT using "../tables/IT.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT1 fr_2024_IT1 fr_2529_IT1 fr_3034_IT1 fr_3539_IT1 fr_4044_IT1 fr_45plus_IT1 using "../tables/IT1.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT2 fr_2024_IT2 fr_2529_IT2 fr_3034_IT2 fr_3539_IT2 fr_4044_IT2 fr_45plus_IT2 using "../tables/IT2.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT2a fr_2024_IT2a fr_2529_IT2a fr_3034_IT2a fr_3539_IT2a fr_4044_IT2a fr_45plus_IT2a using "../tables/IT2a.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT2b fr_2024_IT2b fr_2529_IT2b fr_3034_IT2b fr_3539_IT2b fr_4044_IT2b fr_45plus_IT2b using "../tables/IT2b.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT2c fr_2024_IT2c fr_2529_IT2c fr_3034_IT2c fr_3539_IT2c fr_4044_IT2c fr_45plus_IT2c using "../tables/IT2c.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT2d fr_2024_IT2d fr_2529_IT2d fr_3034_IT2d fr_3539_IT2d fr_4044_IT2d fr_45plus_IT2d using "../tables/IT2d.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT3 fr_2024_IT3 fr_2529_IT3 fr_3034_IT3 fr_3539_IT3 fr_4044_IT3 fr_45plus_IT3 using "../tables/IT3.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT4 fr_2024_IT4 fr_2529_IT4 fr_3034_IT4 fr_3539_IT4 fr_4044_IT4 fr_45plus_IT4 using "../tables/IT4.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_IT5 fr_2024_IT5 fr_2529_IT5 fr_3034_IT5 fr_3539_IT5 fr_4044_IT5 fr_45plus_IT5 using "../tables/IT5.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 

***UK

* TFR

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK2

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2   i.year (exposure_to_robots interaction25  =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK2a

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK2b

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK2c

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK2d

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK3

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK4

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr_UK5

* 20-24

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK1

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK2

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2  i.year (exposure_to_robots interaction25  =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK2a

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK2b

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK2c

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK2d

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK3

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK4

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024_UK5

* 25-29

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK1

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK2

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 ) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK2a

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK2b

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK2c

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK2d

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK3

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK4

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529_UK5

* 30-34

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK1

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK2

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK2a

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK2b

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK2c

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK2d

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK3

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK4

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034_UK5

* 35-39

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK1

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK2

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK2a

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2  i.year (exposure_to_robots  interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK2b

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK2c

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK2d

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK3

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK4

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539_UK5

* 40-44

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK1

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK2

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 ) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK2a

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK2b

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK2c

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK2d

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK3

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK4

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044_UK5

* 45+

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq  ec_active_female  i.year (exposure_to_robots =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK1

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2  i.year (exposure_to_robots interaction25 interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25 vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK2

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it25 vint_de25 vint_es25 vint_fr25 vint_se25 vint_no25 vint_fi25) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK2a

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  male_industry2  i.year (exposure_to_robots  interaction26 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr  vint_it26 vint_de26 vint_es26 vint_fr26 vint_se26 vint_no26 vint_fi26) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK2b

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK2c

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it11 vint_de11 vint_es11 vint_fr11 vint_se11 vint_no11 vint_fi11 vint_it22 vint_de22 vint_es22 vint_fr22 vint_se22 vint_no22 vint_fi22) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK2d

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it3 vint_de3 vint_es3 vint_fr3 vint_se3 vint_no3 vint_fi3) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK3

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it4 vint_de4 vint_es4 vint_fr4 vint_se4 vint_no4 vint_fi4) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK4

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq   ec_active_female empl_techn_tot  i.year (exposure_to_robots interaction5 =instrument_it instrument_es instrument_fi instrument_se instrument_no instrument_de instrument_fr vint_it5 vint_de5 vint_es5 vint_fr5 vint_se5 vint_no5 vint_fi5) if country=="UK", fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus_UK5

esttab tfr_UK fr_2024_UK fr_2529_UK fr_3034_UK fr_3539_UK fr_4044_UK fr_45plus_UK using "../tables/UK.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK1 fr_2024_UK1 fr_2529_UK1 fr_3034_UK1 fr_3539_UK1 fr_4044_UK1 fr_45plus_UK1 using "../tables/UK1.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK2 fr_2024_UK2 fr_2529_UK2 fr_3034_UK2 fr_3539_UK2 fr_4044_UK2 fr_45plus_UK2 using "../tables/UK2.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK2a fr_2024_UK2a fr_2529_UK2a fr_3034_UK2a fr_3539_UK2a fr_4044_UK2a fr_45plus_UK2a using "../tables/UK2a.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK2b fr_2024_UK2b fr_2529_UK2b fr_3034_UK2b fr_3539_UK2b fr_4044_UK2b fr_45plus_UK2b using "../tables/UK2b.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK2c fr_2024_UK2c fr_2529_UK2c fr_3034_UK2c fr_3539_UK2c fr_4044_UK2c fr_45plus_UK2c using "../tables/UK2c.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK2d fr_2024_UK2d fr_2529_UK2d fr_3034_UK2d fr_3539_UK2d fr_4044_UK2d fr_45plus_UK2d using "../tables/UK2d.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace
esttab tfr_UK3 fr_2024_UK3 fr_2529_UK3 fr_3034_UK3 fr_3539_UK3 fr_4044_UK3 fr_45plus_UK3 using "../tables/UK3.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK4 fr_2024_UK4 fr_2529_UK4 fr_3034_UK4 fr_3539_UK4 fr_4044_UK4 fr_45plus_UK4 using "../tables/UK4.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr_UK5 fr_2024_UK5 fr_2529_UK5 fr_3034_UK5 fr_3539_UK5 fr_4044_UK5 fr_45plus_UK5 using "../tables/UK5.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 

clear all
use "../final_data/nonpioneer_2007_2017_modelling.dta", clear

egen nr_r=group(region)

keep if country=="PL" | country=="CZ"

xtset nr_r year, yearly

g share_pop_1524=pop_tot_1524/pop_tot
g share_pop_2534=pop_tot_2534/pop_tot
g share_pop_3549=pop_tot_3549/pop_tot
g share_pop_50plus=pop_tot_50plus/pop_tot

g share_pop_2549=(pop_tot_2534+pop_tot_3549)/pop_tot
g share_pop_1549=(pop_tot_1524+pop_tot_2534+pop_tot_3549)/pop_tot

g share_fem_1524=pop_fem_1524/pop_fem
g share_fem_2534=pop_fem_2534/pop_fem
g share_fem_3549=pop_fem_3549/pop_fem
g share_fem_50plus=pop_fem_50plus/pop_fem
g share_fem_2549=(pop_fem_2534+pop_fem_3549)/pop_fem
g share_fem_1549=(pop_fem_1524+pop_fem_2534+pop_fem_3549)/pop_fem
g pop_female_ratio=pop_fem/pop_tot

g ratio_fem_1549=(pop_fem_1524+pop_fem_2534+pop_fem_3549)/(pop_male_1524+pop_male_2534+pop_male_3549)
g ratio_fem_2549=(pop_fem_2534+pop_fem_3549)/(pop_male_2534+pop_male_3549)
g ratio_fem_2549_sq=ratio_fem_2549*ratio_fem_2549

g ratio_edu=edu_female_isced58/edu_male_isced58
g ratio_edu_sq=ratio_edu*ratio_edu
recode ratio_edu (min/1=1) (1/max=2), g(ratio_hedu)
g edu_total_isced04 = edu_total_isced02+edu_total_isced34

g ratio_industry=female_industry/male_industry
g ratio_findustry=female_industry/all_industry

sort nr_r year
by nr_r: g spell=_n
g x=all_industry if spell==1
by nr_r: egen all_industry2=min(x)
replace all_industry2=100-all_industry2

g y=ratio_industry if spell==1
by nr_r: egen ratio_industry2=min(y)

g yy=female_industry if spell==1
by nr_r: egen female_industry2=min(yy)

g yyy=male_industry if spell==1
by nr_r: egen male_industry2=min(yyy)

g z=ratio_findustry if spell==1
by nr_r: egen ratio_findustry2=min(z)

* lagging

sort country nr_r

g tfr2=tfr[_n+1] if nr_r==nr_r[_n+1]

g fr2_2024=fr_2024[_n+1] if nr_r==nr_r[_n+1]
g fr2_2529=fr_2529[_n+1] if nr_r==nr_r[_n+1]
g fr2_3034=fr_3034[_n+1] if nr_r==nr_r[_n+1]
g fr2_3539=fr_3539[_n+1] if nr_r==nr_r[_n+1]
g fr2_4044=fr_4044[_n+1] if nr_r==nr_r[_n+1]
g fr2_45plus=fr_45plus[_n+1] if nr_r==nr_r[_n+1]
g exposure_to_robots2=exposure_to_robots[_n+1] if nr_r==nr_r[_n+1]

drop if year==2017
replace year=year+1
drop tfr fr_2024  fr_2529 fr_3034 fr_3539 fr_4044 fr_45plus exposure_to_robots
rename tfr2 tfr
rename fr2_2024 fr_2024
rename fr2_2529 fr_2529
rename fr2_3034 fr_3034
rename fr2_3539 fr_3539
rename fr2_4044 fr_4044
rename fr2_45plus fr_45plus
rename exposure_to_robots2 exposure_to_robots

gen interaction1 = exposure_to_robots*all_industry
gen interaction11 = exposure_to_robots*all_industry2
gen interaction2 = exposure_to_robots*ratio_industry
gen interaction22 = exposure_to_robots*ratio_industry2
gen interaction23 = exposure_to_robots*female_industry
gen interaction24 = exposure_to_robots*male_industry
gen interaction25 = exposure_to_robots*female_industry2
gen interaction26 = exposure_to_robots*male_industry2
gen interaction3 = exposure_to_robots*edu_total_isced58
gen interaction4 = exposure_to_robots*youth_unempl_rate_tot
gen interaction5 = exposure_to_robots*empl_techn_tot

gen vint_uk1 = instrument_uk*all_industry
gen vint_fr1 = instrument_fr*all_industry
gen vint_es1 = instrument_es*all_industry
gen vint_it1 = instrument_it*all_industry
gen vint_se1 = instrument_se*all_industry
gen vint_no1 = instrument_no*all_industry
gen vint_fi1 = instrument_fi*all_industry
gen vint_de1 = instrument_de*all_industry
gen vint_us1 = instrument_us*all_industry

gen vint_uk11 = instrument_uk*all_industry2
gen vint_fr11 = instrument_fr*all_industry2
gen vint_es11 = instrument_es*all_industry2
gen vint_it11 = instrument_it*all_industry2
gen vint_se11 = instrument_se*all_industry2
gen vint_no11 = instrument_no*all_industry2
gen vint_fi11 = instrument_fi*all_industry2
gen vint_de11 = instrument_de*all_industry2
gen vint_us11 = instrument_us*all_industry2

gen vint_uk2 = instrument_uk*ratio_industry
gen vint_fr2 = instrument_fr*ratio_industry
gen vint_es2 = instrument_es*ratio_industry
gen vint_it2 = instrument_it*ratio_industry
gen vint_se2 = instrument_se*ratio_industry
gen vint_no2 = instrument_no*ratio_industry
gen vint_fi2 = instrument_fi*ratio_industry
gen vint_de2 = instrument_de*ratio_industry
gen vint_us2 = instrument_us*ratio_industry

gen vint_uk22= instrument_uk*ratio_industry2
gen vint_fr22= instrument_fr*ratio_industry2
gen vint_es22= instrument_es*ratio_industry2
gen vint_it22= instrument_it*ratio_industry2
gen vint_se22= instrument_se*ratio_industry2
gen vint_no22= instrument_no*ratio_industry2
gen vint_fi22= instrument_fi*ratio_industry2
gen vint_de22= instrument_de*ratio_industry2
gen vint_us22 = instrument_us*ratio_industry2

gen vint_uk23 = instrument_uk*female_industry
gen vint_fr23 = instrument_fr*female_industry
gen vint_es23 = instrument_es*female_industry
gen vint_it23 = instrument_it*female_industry
gen vint_se23 = instrument_se*female_industry
gen vint_no23 = instrument_no*female_industry
gen vint_fi23 = instrument_fi*female_industry
gen vint_de23 = instrument_de*female_industry
gen vint_us23 = instrument_us*female_industry

gen vint_uk24 = instrument_uk*male_industry
gen vint_fr24 = instrument_fr*male_industry
gen vint_es24 = instrument_es*male_industry
gen vint_it24 = instrument_it*male_industry
gen vint_se24 = instrument_se*male_industry
gen vint_no24 = instrument_no*male_industry
gen vint_fi24 = instrument_fi*male_industry
gen vint_de24 = instrument_de*male_industry
gen vint_us24 = instrument_us*male_industry

gen vint_uk25 = instrument_uk*female_industry2
gen vint_fr25 = instrument_fr*female_industry2
gen vint_es25 = instrument_es*female_industry2
gen vint_it25 = instrument_it*female_industry2
gen vint_se25 = instrument_se*female_industry2
gen vint_no25 = instrument_no*female_industry2
gen vint_fi25 = instrument_fi*female_industry2
gen vint_de25 = instrument_de*female_industry2
gen vint_us25 = instrument_us*female_industry2

gen vint_uk26 = instrument_uk*male_industry2
gen vint_fr26 = instrument_fr*male_industry2
gen vint_es26 = instrument_es*male_industry2
gen vint_it26 = instrument_it*male_industry2
gen vint_se26 = instrument_se*male_industry2
gen vint_no26 = instrument_no*male_industry2
gen vint_fi26 = instrument_fi*male_industry2
gen vint_de26 = instrument_de*male_industry2
gen vint_us26 = instrument_us*male_industry2

gen vint_uk3 = instrument_uk*edu_total_isced58
gen vint_fr3 = instrument_fr*edu_total_isced58
gen vint_es3 = instrument_es*edu_total_isced58
gen vint_it3 = instrument_it*edu_total_isced58
gen vint_se3 = instrument_se*edu_total_isced58
gen vint_no3 = instrument_no*edu_total_isced58
gen vint_fi3 = instrument_fi*edu_total_isced58
gen vint_de3 = instrument_de*edu_total_isced58
gen vint_us3 = instrument_us*edu_total_isced58

gen vint_uk4 = instrument_uk*youth_unempl_rate_tot
gen vint_fr4 = instrument_fr*youth_unempl_rate_tot
gen vint_es4 = instrument_es*youth_unempl_rate_tot
gen vint_it4 = instrument_it*youth_unempl_rate_tot
gen vint_se4 = instrument_se*youth_unempl_rate_tot
gen vint_no4 = instrument_no*youth_unempl_rate_tot
gen vint_fi4 = instrument_fi*youth_unempl_rate_tot
gen vint_de4 = instrument_de*youth_unempl_rate_tot
gen vint_us4 = instrument_us*youth_unempl_rate_tot


gen vint_uk5 = instrument_uk*empl_techn_tot
gen vint_fr5 = instrument_fr*empl_techn_tot
gen vint_es5 = instrument_es*empl_techn_tot
gen vint_it5 = instrument_it*empl_techn_tot
gen vint_se5 = instrument_se*empl_techn_tot
gen vint_no5 = instrument_no*empl_techn_tot
gen vint_fi5 = instrument_fi*empl_techn_tot
gen vint_de5 = instrument_de*empl_techn_tot
gen vint_us5 = instrument_us*empl_techn_tot

*** PL CZ ****

* TFR
xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr1

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr2

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr2a

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr2b

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr2c

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr2d

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr3

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store tfr4

xi: xtivreg2 tfr share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store tfr5

* 20-24
xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2024

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20241

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20242

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20242a

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20242b

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20242c

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20242d

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20243

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_20244

xi: xtivreg2 fr_2024 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_20245

* 25-29
xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_2529

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25291

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25292

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25292a

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25292b

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25292c

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25292d

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25293

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_25294

xi: xtivreg2 fr_2529 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_25295

* 30-34
xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3034

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30341

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30342

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30342a

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30342b

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30342c

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30342d

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30343

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_30344

xi: xtivreg2 fr_3034 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_30345

* 35-39
xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_3539

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35391

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35392

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35392a

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35392b

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35392c

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35392d

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35393

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_35394

xi: xtivreg2 fr_3539 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_35395

* 40-44
xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_4044

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40441

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40442

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40442a

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40442b

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40442c

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40442d

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40443

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_40444

xi: xtivreg2 fr_4044 share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_40445

* 45+
xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us), fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus1

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 male_industry2   i.year (exposure_to_robots interaction25 interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25 vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus2

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female female_industry2 i.year (exposure_to_robots interaction25  =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk25 vint_fr25 vint_es25 vint_it25 vint_se25 vint_no25 vint_fi25 vint_de25 vint_us25) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus2a

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female male_industry2   i.year (exposure_to_robots interaction26 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk26 vint_fr26 vint_es26 vint_it26 vint_se26 vint_no26 vint_fi26 vint_de26 vint_us26) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus2b

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2 ratio_industry2  i.year (exposure_to_robots interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus2c

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female all_industry2  i.year (exposure_to_robots interaction11  interaction22 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk11 vint_fr11 vint_es11 vint_it11 vint_se11 vint_no11 vint_fi11 vint_de11 vint_us11 vint_uk22 vint_fr22 vint_es22 vint_it22 vint_se22 vint_no22 vint_fi22 vint_de22 vint_us22) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus2d

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  i.year (exposure_to_robots interaction3 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk3 vint_fr3 vint_es3 vint_it3 vint_se3 vint_no3 vint_fi3 vint_de3 vint_us3 vint_de3 vint_us3) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus3

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female youth_unempl_rate_tot  i.year (exposure_to_robots interaction4 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk4 vint_fr4 vint_es4 vint_it4 vint_se4 vint_no4 vint_fi4 vint_de4 vint_us4) , fe cluster(nr_r) partial(i.year share_pop_1524)
estimates store fr_45plus4

xi: xtivreg2 fr_45plus share_pop_1524 share_pop_2549 share_pop_50plus edu_total_isced58 ratio_edu ratio_edu_sq ec_active_female  empl_techn_tot i.year (exposure_to_robots interaction5 =instrument_uk instrument_es instrument_fi instrument_se instrument_no instrument_fr instrument_it instrument_de instrument_us vint_uk5 vint_fr5 vint_es5 vint_it5 vint_se5 vint_no5 vint_fi5 vint_de5 vint_us5) , fe cluster(nr_r) partial(i.year share_pop_1524 share_pop_2549)
estimates store fr_45plus5

esttab tfr fr_2024 fr_2529 fr_3034 fr_3539 fr_4044 fr_45plus using "../tables/CEE.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab  tfr1 fr_20241 fr_25291 fr_30341 fr_35391 fr_40441 fr_45plus1 using "../tables/CEE1.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr2 fr_20242 fr_25292 fr_30342 fr_35392 fr_40442 fr_45plus2 using "../tables/CEE2.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr2a fr_20242a fr_25292a fr_30342a fr_35392a fr_40442a fr_45plus2a using "../tables/CEE2a.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr2b fr_20242b fr_25292b fr_30342b fr_35392b fr_40442b fr_45plus2b using "../tables/CEE2b.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr2c fr_20242c fr_25292c fr_30342c fr_35392c fr_40442c fr_45plus2c using "../tables/CEE2c.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr2d fr_20242d fr_25292d fr_30342d fr_35392d fr_40442d fr_45plus2d using "../tables/CEE2d.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr3 fr_20243 fr_25293 fr_30343 fr_35393 fr_40443 fr_45plus3 using "../tables/CEE3.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr4 fr_20244 fr_25294 fr_30344 fr_35394 fr_40444 fr_45plus4 using "../tables/CEE4.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 
esttab tfr5 fr_20245 fr_25295 fr_30345 fr_35395 fr_40445 fr_45plus5 using "../tables/CEE5.csv", se star(* 0.10 ** 0.05 *** 0.01) aic bic scalar(ll k) replace 