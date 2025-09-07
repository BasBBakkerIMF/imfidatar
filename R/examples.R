#
#
#
#
#
#
#
#
#
#
# countries$
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# mydatasets=idata$metadata$make_dataset_env(needs_auth = F)
#
#
#
#
# weo=mydatasets$`World Economic Outlook (WEO)`
# weo
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
# mykey=list(mycountries,myindicators,myfrequencies)
# mykeystr=idata$utils$make_key_str(mykey)
# mykeystr
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
# # #
# # #   mydatasets=idata$metadata$make_dataset_env(needs_auth = T)
# # #
# # #   weolive=mydatasets$`World Economic Outlook (WEO) Live`
# # #
# # #   data=idata$retrieval$imfdata_by_key(dataset = weolive,key=mykey,needs_auth = T)
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # #
# # # #
# # # #
# # # #   data<-idata$retrieval$imfdata_by_countries_and_series(
# # # #     series = myindicators,
# # # #     countries = mycountries,
# # # #     frequency = myfrequencies,
# # # #     dataset = weo
# # # #   )
# #
# #
# # keypop="A.SP_POP_TOTL.CRI+HND+GTM+NIC+PAN+DOM+SLV"
# #
# # #keypop="A.SLV"
# #
# # pop=imfdata_by_key(dataset = "WB:WDI",needs_auth = T,key=keypop,needs_labels = F)
# #
# #
# #
# # keystr="CRI+PAN.CD_T.S+SC+SD.USD.A"
# # bop=imfdata_by_key(dataset = "IMF.STA:BOP",needs_auth = F,key=keystr,needs_labels = T)
# #
# # df=bop
# # df$date <- string_to_date_by_freq(df$TIME_PERIOD, df$FREQUENCY)
# #
# #
# # df$date <- as.Date(df$date, origin = "1970-01-01")
# #
# #
# # df=df %>% select(COUNTRY, INDICATOR,date,value,INDICATOR_label.en.label)
# #
# #
# # keystr="CRI+PAN.CD_T.S+SC+SD.USD.Q"
# # cpi=imfdata_by_key(dataset = "IMF.STA:CPI",needs_auth = F,key=keystr,needs_labels = F)
# #
# #
# #
# #
# # dims=get_dimension_names(mydatasets$`Bloomberg Data License `)
# #
# #
# # bloomberg=mydatasets$`Bloomberg Data License `
# #
# # tickers=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "TICKER")
# # fields=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "MARKET_FIELD")
# # freqs=idata$metadata$make_dimension_env(dataset_or_db = bloomberg,dimension = "FREQ")
# #
# # #
# #
# #
# #
# # bb_key = list(
# #   c(tickers$`ALBANIAN LEK SPOT`),
# #   c(fields$`Ask Price`, fields$`Bid Price`),
# #   c(freqs$Daily)
# # )
# #
# #
# # bb_data=imfdata_by_key(dataset = bloomberg,needs_auth = T,key=bb_key,needs_labels = F)
# #
# #
# #
# # daily=imfdata_by_key(dataset = bloomberg,needs_auth = T,key=bb_key,needs_labels = F)
# # bb_key = list(
# #   c(tickers$`ALBANIAN LEK SPOT`),
# #   c(fields$`Ask Price`, fields$`Bid Price`),
# #   c(freqs$Monthly)
# # )
# #
# # # Convert TIME_PERIOD strings to Date using a FREQUENCY vector (D, M, Q, A).
# # # - D:  "YYYY-MM-DD" -> that exact date
# # # - M:  "YYYY-Mnn"   -> last day of that month (nn = 01..12)
# # # - Q:  "YYYY-Qn"    -> last day of that quarter (n = 1..4)
# # # - A:  "YYYY"       -> Dec 31 of that year
# #
# # # assuming string_to_date_by_freq() is defined as above
# #
# # df=bop
# # df$date <- string_to_date_by_freq(df$TIME_PERIOD, df$FREQUENCY)
# #
# #
# # df$date <- as.Date(df$date, origin = "1970-01-01")
# #
# #
# #
