#' A list bundling IMF data functions
#'
#' This object provides grouped access to IMF data functions:
#' - retrieval functions
#' - metadata functions
#' - utility functions
#'
#' @export
idata <- list(
  retrieval = list(
    imfdata_by_key = imfdata_by_key,
    imfdata_by_countries_and_series = imfdata_by_countries_and_series
  ),
  metadata = list(
    imfdata_show_datasets = imfdata_show_datasets,
    get_dimension_names = get_dimension_names,
    get_dimension_values=get_dimension_values,
    make_dimension_env = make_dimension_env,
    make_dataset_env = make_dataset_env,
    get_codelist_names=get_codelist_names,
    get_codelist_values_by_id=get_codelist_values_by_id
  ),
  utils = list(
    make_key_str = make_key_str,
    string_to_date_by_freq = string_to_date_by_freq
  )
)
