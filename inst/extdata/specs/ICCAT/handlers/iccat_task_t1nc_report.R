function(sender, data, metadata, path){
  
  requireNamespace("openxlsx")
  
  #load workbook
  task_tpl_file = system.file("extdata/specs/ICCAT/templates/ST02-T1NC.xlsx", package = "repfishr")
  try(rm(file.path(tempdir(), basename(task_tpl_file))),silent = T)
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
  
  #data
  iccat_data = data.frame(
    FlagVesCd = data$flagstate,
    FleetSuffix = NA,
    YearC = data$year,
    SpeciesCd = data$species,
    SpcStockCd = data$stock,
    SaAreaCd = data$sampling_area,
    AreaT1Cd = NA,
    GearCd = data$gear_type,
    FishZoneCd = data$fishing_zone,
    qtyLkg = data$measurement_value,
    qtyDDkg = NA,
    qtyDLkg = NA,
    qtyFAkg = NA,
    TCorBC = "TC",
    CFL = NA,
    CFD = NA,
    DSourceLCd = data$measurement_source,
    corrL = NA,
    DSourceDCd = NA,
    corrD = NA
  )
  task_wb = iccat_fill_report_dataset(task_wb, 1, x = iccat_data, startRow = 26, startCol = 1)
  
  #save workbook
  openxlsx::saveWorkbook(task_wb, file = path, overwrite = TRUE)
  return(path)
}