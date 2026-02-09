function(sender, data, metadata){
  
  data_proc <- data |>
    dplyr::mutate(
      flagstate = sender$id, #inherit flagstate from sender
      time_start = as.Date(time_start),
      time_end = as.Date(time_end),
      year = lubridate::year(time_start),
      quarter = lubridate::quarter(time_start),
      fishing_area = fishing_zone
    )
  
  # Create quarter boundaries
  data_proc <- data_proc |>
    dplyr::mutate(
      quarter_start = dplyr::case_when(
        quarter == 1 ~ as.Date(paste0(year, "-01-01")),
        quarter == 2 ~ as.Date(paste0(year, "-04-01")),
        quarter == 3 ~ as.Date(paste0(year, "-07-01")),
        quarter == 4 ~ as.Date(paste0(year, "-10-01"))
      ),
      quarter_end = case_when(
        quarter == 1 ~ as.Date(paste0(year, "-03-31")),
        quarter == 2 ~ as.Date(paste0(year, "-06-30")),
        quarter == 3 ~ as.Date(paste0(year, "-09-30")),
        quarter == 4 ~ as.Date(paste0(year, "-12-31"))
      ),
      quarter_id = paste0(year, "-Q", quarter)
    )

  result <- data_proc |>
    dplyr::group_by(quarter_id, flagstate, fishing_area, species, measurement, measurement_type, measurement_unit) |>
    dplyr::summarise(
      time_start = dplyr::first(quarter_start),
      time_end = dplyr::first(quarter_end),
      measurement_value = sum(measurement_value),
      .groups = 'drop'
    ) |>
    dplyr::select(flagstate, time_start, time_end, fishing_area, species, measurement, measurement_type, measurement_value, measurement_unit)
  
  #change unit to t
  result$measurement_value = {
    vals = units::set_units(result$measurement_value, result$measurement_unit[1], mode = "standard")
    units(vals) <- "t"
    as.numeric(vals)
  }
  result$measurement_unit = "t"
  result$measurement_type = "nominal"
  
  #share metadata
  attr(result, "metadata") = metadata
  
  return(result)
}