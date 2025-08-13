


library(reticulate)
Sys.setenv(TZ = "UTC")

sys <- import("sys")
idata  <-  import("imfidata")




myds=idata$metadata$show_imf_datasets()
View(myds)

myds=idata$metadata$show_imf_datasets(needs_auth=T)
View(myds)


mydims=idata$metadata$get_dimension_names("CPI")


indexes    = idata$metadata$get_dimension_values_env("CPI","INDEX_TYPE")
coicops    = idata$metadata$get_dimension_values_env("CPI","COICOP_1999")
transforms = idata$metadata$get_dimension_values_env("CPI","CL_CPI_TYPE_OF_TRANSFORMATION")  # or CL_TYPE_OF_TRANSFORMATION

# Assemble key
key = list(
  c("USA","NLD"),
  c(indexes$Consumer_price_index_CPI),
  c(coicops$All_Items),
  c(transforms$Index),
  c("M")
)
keystr = idata$utils$make_key_str(key)
keystr


cpi = idata$retrieval$imfdata_by_key(resource_id='CPI', key=keystr,needs_auth=FALSE,convert_dates=TRUE)
str(cpi)




temp = idata$retrieval$imfdata_by_key(
  resource_id = 'IMF.RES,WEO',
  key = 'USA+CAN.LUR.A',
  needs_auth = FALSE
)



temp = idata$retrieval$imfdata_by_key(resource_id="WEO_LIVE",
                                      key="USA+NLD.LUR.A",
                                      needs_auth=T)
str(temp)



codelists=idata$metadata$get_codelists("CPI")

subcodelist=idata$metadata$get_subcodelist("CPI","CL_CPI_TYPE_OF_TRANSFORMATION")
View(subcodelist)
