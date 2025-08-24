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
#   data=idata$retrieval$imfdata_by_key(dataset = weo,key=mykey)
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
#   data<-idata$retrieval$imfdata_by_countries_and_series(
#     series = myindicators,
#     countries = mycountries,
#     frequency = myfrequencies,
#     dataset = weo
#   )
