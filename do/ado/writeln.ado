* Author: Johannes Schmieder
* July 2007
capture program drop writeln
program define writeln
	gettoken file line : 0
	tempname f
	local append append
	capture confirm file `file'
	if _rc!=0 {
		confirm new file `file'
		local append " "
	}
	file open `f' using `file', `append' write
	file write `f' `line' _n
	file close `f'
end

