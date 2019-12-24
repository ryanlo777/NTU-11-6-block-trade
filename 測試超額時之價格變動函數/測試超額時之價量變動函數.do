cd "/Users/renetteay/Google 雲端硬碟/MF 108/MF 108-1/壽險經營管理實務研討_石百達/期末報告"
save "2330_TSMC.dta", replace
use "2330_TSMC.dta", clear
*tsset date, daily

gen c_volume_percent = c_volume * 100
forvalues t = 5(1)10{
 gen c_volume_high_`t' = c_volume_percent - `t'
 replace c_volume_high_`t' = c_volume_high_`t'/100
 }

gen direction = 1 if d_price/d_volume >0 & d_price/d_volume ~= .
replace direction = 0 if d_price/d_volume <= 0

gen year = year(date)
order year, after(date)
save "2330_TSMC.dta", replace

forvalues t = 5(1)10{
 reg c_price c_volume_high_`t' if direction == 1 & c_volume_high_`t' > 0
 }

forvalues t = 5(1)10{
 reg c_price c_volume_high_`t' if direction == 1 & c_volume_high_`t' > 0 & _n >= 1542 & _n <= 2042
 }

forvalues t = 5(1)10{
 reg c_price L_close L_volume c_volume_high_`t' if direction == 1 & c_volume_high_`t' > 0
 }

forvalues t = 5(1)10{
 reg c_price L_close L_volume c_volume_high_`t' if direction == 1 & c_volume_high_`t' > 0 & _n >= 1542 & _n <= 2042
 }

reg c_price c_volume if direction == 1 & c_volume_high_5 > 0
reg c_price c_volume if direction == 1 & c_volume_high_5 > 0 & _n >= 1542 & _n <= 2042
reg c_price L_close L_volume c_volume if direction == 1 & c_volume_high_5 > 0
reg c_price L_close L_volume c_volume if direction == 1 & c_volume_high_5 > 0 & _n >= 1542 & _n <= 2042

reg c_price c_volume_high_5 if direction == 1 & c_volume_high_5 > 0
reg c_price c_volume_high_5 if direction == 1 & c_volume_high_5 > 0 & _n >= 1542 & _n <= 2042
reg c_price L_close L_volume c_volume_high_5 if direction == 1 & c_volume_high_5 > 0
reg c_price L_close L_volume c_volume_high_5 if direction == 1 & c_volume_high_5 > 0 & _n >= 1542 & _n <= 2042

reg c_price c_volume_high_6 if direction == 1 & c_volume_high_6 > 0
reg c_price c_volume_high_6 if direction == 1 & c_volume_high_6 > 0 & _n >= 1542 & _n <= 2042
reg c_price L_close L_volume c_volume_high_6 if direction == 1 & c_volume_high_6 > 0
reg c_price L_close L_volume c_volume_high_6 if direction == 1 & c_volume_high_6 > 0 & _n >= 1542 & _n <= 2042
