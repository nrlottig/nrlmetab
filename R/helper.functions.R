
# ================================
# = Calculate sampling frequency =
# ================================
calc.freq <- function(datetime){
    freq <- round(Mode(1/diff(date2doy(datetime))))
}
# ==================
# = Calculate Mode =
# ==================
#RDB
Mode <- function(x){
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
date2doy <- function(x){
    stopifnot(any(grepl("^POSIX",class(x[1]))))
    day <- as.numeric(format(x, "%j"))
    pat <- quote("([0-9]{2}:){2}[0-9]{2}")
    midnight <- as.POSIXct(gsub(pat, "00:00:00", x), tz="GMT")
    frac <- as.numeric(difftime(x, midnight, units="days"))
    day+frac
}
