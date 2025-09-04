#
#
#
# mydatasets=idata$metadata$make_dataset_env(needs_auth = F)
#
#
#
#
# weo=mydatasets$`World Economic Outlook (WEO)`
#
#
#
# mydims=idata$metadata$get_dimension_names(weo)
# mydims
#
#
# countries=idata$metadata$make_dimension_env(dataset_or_db = mydatasets$`World Economic Outlook (WEO)`,dimension = "COUNTRY")
#
# indicators=idata$metadata$make_dimension_env(dataset_or_db = weo,dimension = "INDICATOR")
#
# frequencies=idata$metadata$make_dimension_env(dataset_or_db = weo,"FREQ")
#
#
#   mycountries = list(countries$`Netherlands, The`,countries$`United States`)
#   myindicators=list(indicators$`Unemployment rate`,indicators$`Gross domestic product (GDP), Constant prices, Domestic currency`)
#   myfrequencies=list(frequencies$Annual)
#
#
#   mykey=list(mycountries,myindicators,myfrequencies)
#
#
#   data=idata$retrieval$imfdata_by_key(dataset = weo,key=mykey)
#
#
#
#
#
#
#
#
#   mydatasets=idata$metadata$make_dataset_env(needs_auth = T)
#
#   weolive=mydatasets$`World Economic Outlook (WEO) Live`
#
#   data=idata$retrieval$imfdata_by_key(dataset = weolive,key=mykey,needs_auth = T)
#
#
#
#
#
#
#
# #
# #
# #
# #   data<-idata$retrieval$imfdata_by_countries_and_series(
# #     series = myindicators,
# #     countries = mycountries,
# #     frequency = myfrequencies,
# #     dataset = weo
# #   )


keypop="A.SP_POP_TOTL.CRI+HND+GTM+NIC+PAN+DOM+SLV"

#keypop="A.SLV"

pop=imfdata_by_key(dataset = "WB:WDI",needs_auth = T,key=keypop,needs_labels = F)



keystr="CRI+PAN.CD_T.S+SC+SD.USD.A"
bop=imfdata_by_key(dataset = "IMF.STA:BOP",needs_auth = F,key=keystr,needs_labels = T)

df=bop
df$date <- string_to_date_by_freq(df$TIME_PERIOD, df$FREQUENCY)


df$date <- as.Date(df$date, origin = "1970-01-01")


df=df %>% select(COUNTRY, INDICATOR,date,value,INDICATOR_label.en.label)


keystr="CRI+PAN.CD_T.S+SC+SD.USD.Q"
cpi=imfdata_by_key(dataset = "IMF.STA:CPI",needs_auth = F,key=keystr,needs_labels = F)




dims=get_dimension_names(mydatasets$`Bloomberg Data License `)


bloomberg=mydatasets$`Bloomberg Data License `

tickers=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "TICKER")
fields=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "MARKET_FIELD")
freqs=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "FREQ")





bb_key = list(
  c(tickers$`ALBANIAN LEK SPOT`),
  c(fields$`Ask Price`, fields$`Bid Price`),
  c(freqs$Daily)
)


bb_data=imfdata_by_key(dataset = bloomberg,needs_auth = T,key=bb_key,needs_labels = F)



daily=imfdata_by_key(dataset = bloomberg,needs_auth = T,key=bb_key,needs_labels = F)
bb_key = list(
  c(tickers$`ALBANIAN LEK SPOT`),
  c(fields$`Ask Price`, fields$`Bid Price`),
  c(freqs$Monthly)
)

# Convert TIME_PERIOD strings to Date using a FREQUENCY vector (D, M, Q, A).
# - D:  "YYYY-MM-DD" -> that exact date
# - M:  "YYYY-Mnn"   -> last day of that month (nn = 01..12)
# - Q:  "YYYY-Qn"    -> last day of that quarter (n = 1..4)
# - A:  "YYYY"       -> Dec 31 of that year
string_to_date_by_freq <- function(period, frequency) {
  if (!is.character(period)) stop("`period` must be a character vector.")
  if (length(frequency) == 1L) frequency <- rep(frequency, length(period))
  if (length(period) != length(frequency)) {
    stop("`period` and `frequency` must have the same length (or frequency length 1).")
  }

  frequency <- toupper(as.character(frequency))
  out <- vapply(seq_along(period), function(i) {
    s <- toupper(trimws(period[i]))
    f <- frequency[i]

    # Only convert D/M/Q/A; anything else => NA
    if (!f %in% c("D","M","Q","A") || is.na(s) || s == "") return(as.Date(NA))

    if (f == "D") {
      if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) {
        return(as.Date(lubridate::ymd(s)))
      } else return(as.Date(NA))
    }

    if (f == "A") {
      if (grepl("^\\d{4}$", s)) {
        y <- as.integer(s)
        return(as.Date(sprintf("%04d-12-31", y)))
      } else return(as.Date(NA))
    }

    if (f == "Q") {
      m <- regexec("^(\\d{4})-Q([1-4])$", s); g <- regmatches(s, m)[[1]]
      if (length(g)) {
        y <- as.integer(g[2]); q <- as.integer(g[3]); last_month <- q * 3
        d <- lubridate::ceiling_date(
          lubridate::ymd(sprintf("%04d-%02d-01", y, last_month)), "month"
        ) - lubridate::days(1)
        return(as.Date(d))
      } else return(as.Date(NA))
    }

    if (f == "M") {
      m <- regexec("^(\\d{4})-M(0[1-9]|1[0-2])$", s); g <- regmatches(s, m)[[1]]
      if (length(g)) {
        y <- as.integer(g[2]); mm <- as.integer(g[3])
        d <- lubridate::ceiling_date(
          lubridate::ymd(sprintf("%04d-%02d-01", y, mm)), "month"
        ) - lubridate::days(1)
        return(as.Date(d))
      } else return(as.Date(NA))
    }

    as.Date(NA)
  }, FUN.VALUE = as.Date(NA))

  out
}

# assuming string_to_date_by_freq() is defined as above

df=bop
df$date <- string_to_date_by_freq(df$TIME_PERIOD, df$FREQUENCY)


df$date <- as.Date(df$date, origin = "1970-01-01")



