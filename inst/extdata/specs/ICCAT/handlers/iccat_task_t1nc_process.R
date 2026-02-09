function(sender, data, metadata){
  
  result = NULL
  
  #filter on species
  data = data[data$species %in% fdi4R::cl_iccat_species$code,]
  
  #other filters?
  
  #if no data
  if(nrow(data)==0){
    result = data.frame(
      flagstate = character(0),
      year = integer(0),
      species = character(0),
      stock = character(0),
      sampling_area = character(0),
      gear_type = character(0),
      fishing_zone = character(0),
      measurement = character(0),
      measurement_type = character(0),
      measurement_value = numeric(0),
      measurement_unit = character(0),
      measurement_source = character(0)
    )
  }else{
    #remove unecessary columns
    data$fishing_activity = NULL
    data$vessel = NULL
    
    #process time
    data$year = lubridate::year(data$time_end)
    data$time_start = NULL
    data$time_end = NULL
    
    #dissociate data with coordinates / without coordinates
    #DATA WITHOUT GEOMETRY
    data_geomless = data[is.na(data$longitude_start) | is.na(data$latitude_start) |
                         is.na(data$longitude_end) | is.na(data$latitude_end),]
    
    #mapping trough assumptions based on the reporting state
    #from species -> inherit sampling areas -> inherit target sampling areas for the country and take the highest %
    #of intersection between WJA sender (eg GRD) and ICCAT sampling areas.
    #
    #Note: assumption based on the sampling area with highest % of intersects may lead to biased mappings (eg a NJA that 
    #intersects equivalently 2 or 3 sampling areas). A better assumption should be to get the intersect like this when 
    #the NJA is totally included in the sampling area (100%) but in a first instance, we should work on the proxy mapping 
    #between national fishing zones and the sampling areas
    ints = fdi4R::intersections[
      fdi4R::intersections$layer1 == "cwp:wja_level1" &
      fdi4R::intersections$code1 == sender$id &
      fdi4R::intersections$layer2 == "iccat:iccat_sa_master",]
    
    #join with ICCAT sampling areas spatial ref (inherit species)
    ints = ints |> 
      dplyr::left_join(
        y = fdi4R::iccat_sampling_areas_lowres, 
        by = dplyr::join_by(code2 == code)
      )
    ints$geom = NULL
    #by sampling area / species, we retained the sampling area mostly covered by the WJA
    ints = ints[,c("code2", "stock", "stock_asfis_code", "surface1_percent")] |> 
      dplyr::group_by_at(c("code2","stock", "stock_asfis_code")) |> 
      dplyr::summarize(max_surface = max(surface1_percent))
    ints = ints[,c("code2", "stock", "stock_asfis_code")]
    colnames(ints) = c("sampling_area", "stock", "species")
    ints[ints$species %in% c("AT-","MD"),]$species = NA #patch to be solved by Arturo at source
  
    BIL_SA = unique(ints$sampling_area[startsWith(ints$sampling_area,"BIL")])[1]
    ATorMD = substring(ints[is.na(ints$species),]$stock, 1,2)
    
    #PROCESS GEOMLESS DATA
    #-----------------------------------------------------------------------------
    #we join by species to inherit sampling_area mostly covered by WJA + stock
    data_geomless2 = data_geomless |>
      dplyr::left_join(y = ints)
    
    #PROCESS DATA WITH GEOMETRY (likely logbooks)
    #-----------------------------------------------------------------------------
    data_geom = data[!is.na(data$longitude_start) & !is.na(data$latitude_start) &
                     !is.na(data$longitude_end) & !is.na(data$latitude_end),]
    data_geom_lines <- sf::st_sfc(
      mapply(
        function(x1, y1, x2, y2) {
          sf::st_linestring(matrix(
            c(x1, y1, x2, y2),
            ncol = 2,
            byrow = TRUE
          ))
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
      wja_code = fdi4R::wja_level0[sf::st_intersects(data_geom_sf[i,], fdi4R::wja_level0, sparse = FALSE),]$code
      switch(wja_code, "JA" = "EEZ", "ABNJ" = "HSEA")
    })
    #spatial join with ICCAT sampling areas --> sampling area
    data_geom_sf$sampling_area = sapply(1:nrow(data_geom_sf), function(i){
      sa_for_sp = fdi4R::iccat_sampling_areas_lowres[fdi4R::iccat_sampling_areas_lowres$stock_asfis_code == data_geom_sf[i,]$species,]
      sa_code = sa_for_sp[sf::st_intersects(data_geom_sf[i,], sa_for_sp, sparse = FALSE),]$code
      sa_code
    })
    #spatial join with ICCAT sampling areas --> stock
    data_geom_sf$stock = sapply(1:nrow(data_geom_sf), function(i){
      sa_for_sp = fdi4R::iccat_sampling_areas_lowres[fdi4R::iccat_sampling_areas_lowres$stock_asfis_code == data_geom_sf[i,]$species,]
      stock_code = sa_for_sp[sf::st_intersects(data_geom_sf[i,], sa_for_sp, sparse = FALSE),]$stock
      stock_code
    })
    data_geom_sf$geom = NULL
  
    #Bind both datasources
    data_proc = rbind(data_geomless2, data_geom_sf)
    
    #MANAGE MISSING MAPPINGS (species with not specific sampling areas / stocks)
    #-----------------------------------------------------------------------------
    #when there is no sampling area, we assume it's a billfish area
    #that we taken from above best intersect with WJA
    if(any(is.na(data_proc$sampling_area))){
      data_proc[is.na(data_proc$sampling_area),]$sampling_area = BIL_SA
    }
    #when there is no stock information we need to inherit it
    #=> 1st choice: we use the mapping species/stocks
    if(any(is.na(data_proc$stock))){
      new_dt = data_proc[is.na(data_proc$stock),] |>
        dplyr::select(-c(stock)) |>
        dplyr::left_join(
          y = fdi4R::mapping_iccat_species__x__stocks[regexpr(ATorMD, fdi4R::mapping_iccat_species__x__stocks$stock)>0,],
          by = dplyr::join_by(species == species)
        )
      data_proc[is.na(data_proc$stock),]$stock = new_dt$stock
    }
    
    #if we still have some missing stocks
    #=> 2d choice: we use the mapping sampling_areas / stocks to derivate stock
    #we know the target BIL sampling area (BIL_SA) and if it's AT or MD
    #we can filter on the mapping to retrieve the species- non specific stock area
    if(any(is.na(data_proc$stock))){
      fdi4R::mapping_iccat_sampling_areas__x__stocks[fdi4R::mapping_iccat_sampling_areas__x__stocks$src_code == BIL_SA & regexpr(ATorMD, fdi4R::mapping_iccat_sampling_areas__x__stocks$trg_code) > 0,]$trg_code
    }
    
    #additional processing before aggregation
    data_proc = data_proc |> dplyr::rename(measurement_source = data_source)
    data_proc = cbind(
      flagstate = sender$id,
      data_proc[,c("year", "species", "stock", "sampling_area", "gear_type", "fishing_zone", 
                   "measurement", "measurement_type", "measurement_value", "measurement_unit", "measurement_source")]
    )
    data_proc$measurement_value = sapply(1:nrow(data_proc), function(i){
      ud = units::as_units(data_proc[i,]$measurement_value, data_proc[i,]$measurement_unit)
      ud = units::set_units(ud, "kg")
      as.numeric(ud)
    })
    data_proc$measurement_unit = "kg"
    
    #aggregation by strata
    series = setdiff(colnames(data_proc), "measurement_value")
    result = data_proc |> 
      dplyr::group_by_at(series) |>
      dplyr::summarise(measurement_value = sum(measurement_value, na.rm = T)) |>
      dplyr::ungroup()
  }

  #share metadata
  attr(result, "metadata") = metadata
  
  return(result)
}