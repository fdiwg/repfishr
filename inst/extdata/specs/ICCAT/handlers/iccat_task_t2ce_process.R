function(sender, data, metadata, params = list()){
  
  result = NULL
  
  #filter on species
  data = data[data$species %in% fdi4R::cl_iccat_species$code,]
  
  #other filters?
  
  #check params 
  if(length(params)==0 | is.null(params$fishing_zones) | !is(params$fishing_zones, "sf")){
    warning("The ICCAT T2CE process requires a spatial 'fishing_zones' object")
  }
  
  #if no data
  if(nrow(data)==0){
    result = data.frame(
      flagstate = character(0),
      year = integer(0),
      month = integer(0),
      species = character(0),
      gear_type = character(0),
      geo_grid_cd = character(0),
      quad_cd = character(0),
      lat = numeric(0),
      lon = numeric(0),
      fishing_mode = character(0),
      effort_fishing_duration = numeric(0),
      effort_fishing_duration_unit = character(0),
      effort_number_gears = numeric(0),
      effort_number_gears_unit = character(0),
      effort_number_sets = numeric(0),
      measurement = character(0),
      measurement_type = character(0),
      measurement_value = numeric(0),
      measurement_unit = character(0),
      measurement_source = character(0),
      processing_type = character(0)
      #catch_type = character(0)
    )
    metadata$nb_records = 0
  }else{
    data_proc = NULL
    
    #remove unnecessary columns
    data$fishing_activity = NULL
    data$vessel = NULL
    
    #process time
    data$year  = lubridate::year(data$time_end)
    data$month = lubridate::month(data$time_end)
    data$time_start = NULL
    data$time_end   = NULL
    
    #dissociate data with coordinates / without coordinates
    
    #DATA WITHOUT COORDINATES
    data_geomless = data[is.na(data$longitude_start) | is.na(data$latitude_start) |
                           is.na(data$longitude_end) | is.na(data$latitude_end),]
    
    #For geomless data, derive lat, lon, quad_cd and geo_grid_cd from either:
    #  1) activity_zone polygon (from params$fishing_zones) if available
    #  2) sender WJA polygon if no geopackage available
    #Intersect the polygon with the grid (1deg or 5deg depending on gear)
    #Take the cell with the highest intersection area
    sender_wja = fdisfdata::wja_level1[fdisfdata::wja_level1$code == sender$id,]
    
    for(i in 1:nrow(data_geomless)){
      
      #select reference polygon: activity_zone if available, sender_wja as fallback
      if(!is.na(data_geomless[i,]$activity_zone)){
        ref_polygon = params$fishing_zones[params$fishing_zones$code == data_geomless[i,]$activity_zone,]
      }else{
        ref_polygon = sender_wja
      }
      
      #select grid and geo_grid_cd based on gear type
      if(startsWith(data_geomless[i,]$gear_type, "LL")){
        grid = fdisfdata::grid_5deg
        data_geomless$geo_grid_cd[i] = "5x5"
      }else{
        grid = fdisfdata::grid_1deg
        data_geomless$geo_grid_cd[i] = "1x1"
      }
      
      #intersect reference polygon with grid cells
      grid_match = grid[sf::st_intersects(ref_polygon, grid, sparse = FALSE),]
      
      if(nrow(grid_match) > 0){
        #compute intersection area for each intersecting grid cell and take the one with highest area
        intersection_areas = sf::st_area(sf::st_intersection(ref_polygon, grid_match))
        best = which.max(intersection_areas)
        data_geomless$lat[i]     = as.numeric(grid_match$CWP_C[best])
        data_geomless$lon[i]     = as.numeric(grid_match$CWP_D[best])
        data_geomless$quad_cd[i] = grid_match$QUADRANT[best]
      }else{
        data_geomless$lat[i]     = NA_real_
        data_geomless$lon[i]     = NA_real_
        data_geomless$quad_cd[i] = NA_character_
      }
    }
    
    
    #PROCESS DATA WITH GEOMETRY (likely logbooks)
    #-----------------------------------------------------------------------------
    data_geom = data[!is.na(data$longitude_start) & !is.na(data$latitude_start) &
                       !is.na(data$longitude_end)   & !is.na(data$latitude_end),]
    if(nrow(data_geom)>0){
      data_geom_lines <- sf::st_sfc(
        mapply(
          function(x1, y1, x2, y2) {
            if(x1 != x2 & y1 != y2){
              sf::st_linestring(matrix(
                c(x1, y1, x2, y2),
                ncol = 2,
                byrow = TRUE
              ))
            }else{
              sf::st_point(c(x1, y2))
            }
          },
          data_geom$longitude_start,
          data_geom$latitude_start,
          data_geom$longitude_end,
          data_geom$latitude_end,
          SIMPLIFY = FALSE
        ),
        crs = 4326  # WGS84 lon/lat
      )
      
      # convert to sf
      data_geom_sf <- sf::st_sf(data_geom, geom = data_geom_lines)
      #spatial join with CWP WJA level 0 -> fishing zone
      data_geom_sf$fishing_zone = sapply(1:nrow(data_geom_sf), function(i){
        wja_code = fdisfdata::wja_level0[sf::st_intersects(data_geom_sf[i,], fdisfdata::wja_level0, sparse = FALSE),]$code
        switch(wja_code, "JA" = "EEZ", "ABNJ" = "HSEA")
      })
      
      #spatial join with CWP Grid (1x1 or 5x5 depending on gear) -> CWP_C, CWP_D, QUADRANT
      #take the grid cell with the highest intersection area
      for(i in 1:nrow(data_geom_sf)){
        if(startsWith(data_geom_sf[i,]$gear_type, "LL")){
          grid = fdisfdata::grid_5deg
          data_geom_sf$geo_grid_cd[i] = "5x5"
        }else{
          grid = fdisfdata::grid_1deg
          data_geom_sf$geo_grid_cd[i] = "1x1"
        }
        grid_match = grid[sf::st_intersects(data_geom_sf[i,], grid, sparse = FALSE),]
        if(nrow(grid_match) > 0){
          intersection_areas = sf::st_area(sf::st_intersection(data_geom_sf[i,], grid_match))
          best = which.max(intersection_areas)
          data_geom_sf$lat[i]     = as.numeric(grid_match$CWP_C[best])
          data_geom_sf$lon[i]     = as.numeric(grid_match$CWP_D[best])
          data_geom_sf$quad_cd[i] = grid_match$QUADRANT[best]
        }else{
          data_geom_sf$lat[i]     = NA_real_
          data_geom_sf$lon[i]     = NA_real_
          data_geom_sf$quad_cd[i] = NA_character_
        }
      }
      
      data_geom_sf$geom = NULL
      
      
      #Bind both datasources
      data_proc = rbind(data_geomless, data_geom_sf)
    }else{
      data_proc = data_geomless
    }
   
    #additional processing before aggregation
    data_proc = data_proc |> dplyr::rename(measurement_source = data_source)
    data_proc = cbind(
      flagstate = sender$id,
      data_proc[,c("year", "month", "species", "gear_type", "geo_grid_cd", "quad_cd", "lat", "lon", "fishing_zone", "fishing_mode", "effort_fishing_duration", "effort_fishing_duration_unit", "effort_number_gears", "effort_number_gears_unit", "effort_number_sets",
                   "measurement", "measurement_type", "measurement_value", "measurement_unit", "measurement_source", "processing_type")]
    )
    data_proc$measurement_value = sapply(1:nrow(data_proc), function(i){
      ud = units::as_units(data_proc[i,]$measurement_value, data_proc[i,]$measurement_unit)
      ud = units::set_units(ud, "kg")
      as.numeric(ud)
    })
    data_proc$measurement_unit = "kg"
    
    #Mapping for T2CE rows vs columns match:
    #aggregation by strata + species column key
    #each unique species/processing_type combination becomes a column - catch type is always "L" by now
    data_proc$sp_col_key = paste(data_proc$species, data_proc$processing_type, sep = "|")
    
    strata_cols = c("flagstate", "year", "month", "gear_type", "geo_grid_cd", "quad_cd", "lat", "lon", "fishing_zone",
                    "fishing_mode", "effort_fishing_duration_unit", "effort_number_gears_unit", "measurement_source")
    # Removed "effort_fishing_duration", "effort_number_gears" and "effort_number_sets" so they get aggregated - CHECK WITH ICCAT
    
    # When effort_number_gears is available, nullify effort_number_sets so it doesn't affect aggregation
    data_proc$effort_number_sets = ifelse(
      !is.na(data_proc$effort_number_gears),
      NA_real_,
      data_proc$effort_number_sets
    )
    
    data_agg = data_proc |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strata_cols, "sp_col_key")))) |>
      dplyr::summarise(
        measurement_value = sum(measurement_value, na.rm = TRUE),
        .groups = "drop")
    
    #aggregate effort separately
    effort_agg = data_proc |>
      dplyr::group_by(dplyr::across(dplyr::all_of(strata_cols))) |>
      dplyr::summarise(
        effort_fishing_duration = sum(effort_fishing_duration, na.rm = TRUE),
        effort_number_gears     = if(all(is.na(effort_number_gears))) NA_real_ else sum(effort_number_gears, na.rm = TRUE),
        effort_number_sets      = if(all(is.na(effort_number_sets)))  NA_real_ else sum(effort_number_sets,  na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        effort_number_sets = ifelse(!is.na(effort_number_gears), NA_real_, effort_number_sets)
      )
    
    result = data_agg |>
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(strata_cols),
        names_from  = sp_col_key,
        values_from = measurement_value,
        values_fill = NA_real_
      ) |>
      dplyr::left_join(effort_agg, by = strata_cols)
    
    metadata$nb_records = nrow(result)
  }
  
  #share metadata
  attr(result, "metadata") = metadata
  
  return(result)
}