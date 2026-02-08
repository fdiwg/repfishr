function(sender, data, metadata){
  
 data_proc = data |>
     dplyr::mutate(
      flagstate = sender$id,
      year = lubridate::year(time_end)
    ) |>
    dplyr::group_by_at(c("flagstate", "year", "species", "measurement", "measurement_type", "measurement_unit")) |>
    dplyr::summarise(
      measurement_value = sum(measurement_value)
    )
  data_proc = data_proc[,c("flagstate", "year","species", "measurement", "measurement_type", "measurement_value", "measurement_unit")]
  
  #change unit to t
  data_proc$measurement_value = {
    vals = units::set_units(data_proc$measurement_value, data_proc$measurement_unit[1], mode = "standard")
    units(vals) <- "t"
    as.numeric(vals)
  }
  
  #share metadata
  attr(data_proc, "metadata") = metadata
  
  return(data_proc)
}