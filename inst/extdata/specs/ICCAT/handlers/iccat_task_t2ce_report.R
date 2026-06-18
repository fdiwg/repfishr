function(sender, data, metadata, path){
  
  requireNamespace("openxlsx")
  
  #load workbook
  task_tpl_file = system.file("extdata/specs/ICCAT/templates/ST03-T2CE.xlsx", package = "repfishr")
  try(rm(file.path(tempdir(), basename(task_tpl_file))), silent = T)
  task_tmp_file = file.copy(from = task_tpl_file, to = tempdir())
  task_wb = openxlsx::loadWorkbook(file.path(tempdir(), basename(task_tpl_file)))
  
  #metadata
  #-> statistical correspondent
  task_wb = iccat_fill_report_statistical_correspondent(task_wb, metadata)
  #-> Data set characteristics
  task_wb = iccat_fill_report_dataset_characteristics(task_wb,
    reporting_flag = sender$name,
    from = metadata$from, to = metadata$to,
    report_version = "Preliminary", #TODO capture in metadata or new arg
    report_type = "Revision", #TODO capture in metadata or new arg
    report_coverage = "PARTIAL" #TODO capture in metadata or new arg
  )
  
  #build species column headers from the wide result column names
  strata_cols = c("flagstate", "year", "month", "gear_type", "fishing_zone",
                  "fishing_mode", "effort_fishing_duration", "effort_fishing_duration_unit", "effort_number_gears", "effort_number_gears_unit", "effort_number_sets",
                  "measurement_source")
  sp_keys = setdiff(colnames(data), strata_cols)  #I get "BUM|DR|L", "DOL|LW|L", etc.
  
  sp_header = do.call(rbind, strsplit(sp_keys, "\\|"))
  colnames(sp_header) = c("species", "product_type")
  
  #write species / product type / catch type at rows 22, 23, 24 from col O (15)
  for(j in 1:nrow(sp_header)){
    col_j = 14L + j
    openxlsx::writeData(task_wb, sheet = "ST03-T2CE", x = sp_header[j,"species"],      startRow = 22L, startCol = col_j)
    openxlsx::writeData(task_wb, sheet = "ST03-T2CE", x = sp_header[j,"product_type"], startRow = 23L, startCol = col_j)
    openxlsx::writeData(task_wb, sheet = "ST03-T2CE", "L",   startRow = 24L, startCol = col_j)
  }
  
  #data
  iccat_data = data.frame(
    FlagVesCd = if(nrow(data)>0) data$flagstate      else NA,
    FleetSuffix = NA,
    YearC = if(nrow(data)>0) data$year           else NA,
    Month = if(nrow(data)>0) data$month          else NA,
    GearCd = if(nrow(data)>0) data$gear_type      else NA,
    GeoGridCd = if(nrow(data)>0) "5x5" else NA, # 5x5 for LL, 1x1 for others / Note there is a LatLon option for specific coordinates (i.e., FADs?)
    QuadCd = if(nrow(data)>0) "NW" else NA, #this is the universal quadrant that is available at the cwp_grids QUADRANT metadata
    Lat = NA, #from as.numeric(CWP_C)
    Lon = NA, #from as.numeric(CWP_D)
    SchoolTypCd = if(nrow(data)>0) data$fishing_mode else NA,
    Effort1 = if(nrow(data)>0) data$effort_fishing_duration        else NA,
    Eff1TypeCd = if(nrow(data)>0) data$effort_fishing_duration_unit   else NA,
    Effort2 = if(nrow(data)>0) dplyr::coalesce(data$effort_number_gears, data$effort_number_sets) else NA,
    Eff2TypeCd = if(nrow(data)>0) ifelse(is.na(data$effort_number_gears), "NO.SETS", data$effort_number_gears_unit) else NA
  )
  
  #Fill each row according with the species / product type type columns:
  for(j in 1:nrow(sp_header)){
    key = sp_keys[j]
    iccat_data[[paste0("sp", sprintf("%02d", j))]] =
      if(nrow(data)>0 & key %in% colnames(data)) data[[key]] else NA
  }
  
  task_wb = iccat_fill_report_dataset(task_wb, 1, x = iccat_data, startRow = 28, startCol = 1)
  
  #save workbook
  openxlsx::saveWorkbook(task_wb, file = path, overwrite = TRUE)
  return(path)
}