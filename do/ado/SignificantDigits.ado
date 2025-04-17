* Author: Johannes Schmieder
* July 2012
program define SignificantDigits // idea taken from outreg2.ado
	args fmt value
	local d = substr("`fmt'", 2, .)
	capt confirm integer number `d'
	if _rc {
		di as err `"`fmt' not allowed"'
		exit 198
	}
// missing: format does not matter
	if `value'>=. local fmt "%9.0g"
// integer: print no decimal places
	else if (`value'-int(`value'))==0 {
		local fmt "%12.0f"
	}
// value in (-1,1): display up to 9 decimal places with d significant
// digits, then switch to e-format with d-1 decimal places
	else if abs(`value')<1 {
		local right = -int(log10(abs(`value'-int(`value')))) // zeros after dp
		local dec = max(1,`d' + `right')
		if `dec'<=9 {
			local fmt "%12.`dec'f"
		}
		else {
			local fmt "%12.`=min(9,`d'-1)'e"
		}
	}
// |values|>=1: display d+1 significant digits or more with at least one
// decimal place and up to nine digits before the decimal point, then
// switch to e-format
	else {
		local left = int(log10(abs(`value'))+1) // digits before dp
		if `left'<=9 {
			local fmt "%12.`=max(1,`d' - `left' + 1)'f"
		}
		else {
			local fmt "%12.0e" // alternatively: "%12.`=min(9,`d'-1)'e"
		}
	}
	c_local fmt "`fmt'"
end 


