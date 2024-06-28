/* Copyright (c) 2022 Honorata Bogusz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.*/

clear all
graph set window fontface "Atkinson Hyperlegible"

* Figure 1

use "../final_data/all_modelling.dta", replace

keep if country=="DE" | country=="FR" | country=="IT" | country=="UK" | country=="PL" | country=="CZ"

	bysort country year: egen m_etr=mean(exposure_to_robots)
	
	keep country year m_etr
	duplicates drop
		
	egen nr_c=group(country)
	xtset nr_c year, yearly

set scheme virdis, permanently
graph twoway (tsline m_etr if country=="DE", lwidth(vthick  ..)) || (tsline m_etr if country=="FR", lwidth(vthick  ..)) || (tsline m_etr if country=="IT", lwidth(vthick  ..)) || (tsline m_etr if country=="UK", lwidth(vthick  ..)) || (tsline m_etr if country=="PL", lwidth(vthick  ..)) || (tsline m_etr if country=="CZ", lwidth(vthick  ..)), tlabel(1997 2002 2007 2012 2017) ytitle("robots per 10.000 workers") legend(label(1 "Germany") label(2 "France") label(3 "Italy") label(4 "UK") label(5 "Poland") label(6 "Czechia") position(0) bplacement(nwest))
graph export "../plots/Figure_1.png", replace
graph export "../plots/Figure_1.eps", replace

* Figure 2

clear all

use "../generated_data/df_apr_manuf_unempl.dta", replace 

egen nr_c = group(country)
xtset nr_c year

twoway (tsline robots_10000workers if nr_c == 1 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 1,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("Czechia") name(czechia, replace)

twoway (tsline robots_10000workers if nr_c == 2 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 2,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("France") name(france, replace)

twoway (tsline robots_10000workers if nr_c == 3 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 3,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("Germany") name(germany, replace)

twoway (tsline robots_10000workers if nr_c == 4 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 4,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("Italy") name(italy, replace)

twoway (tsline robots_10000workers if nr_c == 5 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 5,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("Poland") name(poland, replace)

twoway (tsline robots_10000workers if nr_c == 6 ,yaxis(1) lc(gs10) lwidth(thick)) (tsline unempl if nr_c == 6,yaxis(2) lc(gs2) lwidth(thick)), tlabel(1993 2005 2017) xtitle("") ytitle("", axis(1)) ytitle("", axis(2)) ylabel(0(20)100, axis(1)) yscale(range(0(20)100) axis(1)) ylabel(0(5)20, axis(2)) yscale(range(0(5)20) axis(2)) legend(off label(1 "Robots per 10.000 Workers") label(2 "Unemployment Rate") position(0) bplacement(nwest) size(large)) title("United Kingdom") name(uk, replace)

grc1leg2 czechia france germany italy poland uk
graph export "../plots/Figure_2.png", replace
graph export "../plots/Figure_2.eps", replace