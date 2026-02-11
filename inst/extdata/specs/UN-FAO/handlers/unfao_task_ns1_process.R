function(sender, data, metadata){
  
  result = NULL
  
  if(nrow(data)==0){
    result = data.frame(
      flagstate = character(0),
      year = integer(0),
      species = character(0),
      measurement = character(0),
      measurement_type = character(0),
      measurement_value = numeric(0),
      measurement_unit = character(0)
    )
    metadata$nb_records = 0
  }else{
    result = data |>
         dplyr::mutate(
          flagstate = sender$id, #inherit flagstate from sender
          year = lubridate::year(time_end) #derive year from date
        ) |>
        dplyr::group_by_at(c("flagstate", "year", "species", "measurement", "measurement_type", "measurement_unit")) |>
        dplyr::summarise(
          measurement_value = sum(measurement_value)
        )
    result = result[,c("flagstate", "year","species", "measurement", "measurement_type", "measurement_value", "measurement_unit")]
      
    #change unit to t
    result$measurement_value = {
        vals = units::set_units(result$measurement_value, result$measurement_unit[1], mode = "standard")
        units(vals) <- "t"
        as.numeric(vals)
      }
    result$measurement_unit = "t"
    metadata$nb_records = nrow(result)
  }
  
  #share metadata
  attr(result, "metadata") = metadata
  
  return(result)
}