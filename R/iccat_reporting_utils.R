#'@name iccat_fill_report_statistical_correspondent
#'@title Fills an ICCAT template workbook with statistical correspondent information
#'
#'@usage iccat_fill_report_statistical_correspondent(wb, metadata)
#'
#'@param wb a \code{Workbook} object from \link{openxlsx}
#'@param metadata a named \link{list} with business metadata
#'@return the modified \code{Workbook} object
#'
#'@export
iccat_fill_report_statistical_correspondent = function(wb, metadata){
  
  #assume metadata is a named list for now
  #TODO investigate adoption of prov4R (PROV-XML implementation for metadata provenance)
  
  if(!is.null(metadata$fullname)) openxlsx::writeData(wb, 1, x = metadata$fullname, startRow = 5, startCol = 3)
  if(!is.null(metadata$email)) openxlsx::writeData(wb, 1, x = metadata$email, startRow = 6, startCol = 3)
  if(!is.null(metadata$phonenumber)) openxlsx::writeData(wb, 1, x = metadata$phonenumber, startRow = 6, startCol = 7)
  if(!is.null(metadata$institution)) openxlsx::writeData(wb, 1, x = metadata$institution, startRow = 7, startCol = 3)
  if(!is.null(metadata$department)) openxlsx::writeData(wb, 1, x = metadata$department, startRow = 8, startCol = 3)
  if(!is.null(metadata$address)) openxlsx::writeData(wb, 1, x = metadata$address, startRow = 9, startCol = 3)
  if(!is.null(metadata$country)) openxlsx::writeData(wb, 1, x = metadata$country, startRow = 9, startCol = 7)
  
  return(wb)
}

#'@name iccat_fill_report_dataset_characteristics
#'@title Fills an ICCAT template workbook with dataset characteristics information
#'
#'@usage iccat_fill_report_dataset_characteristics(
#'  wb, reporting_flag, from, to,
#'  report_version, report_type, report_coverage
#')
#'
#'@param wb a \code{Workbook} object from \link{openxlsx}
#'@param metadata a named \link{list} with business metadata
#'@return the modified \code{Workbook} object
#'
#'@export
iccat_fill_report_dataset_characteristics = function(wb, 
                                              reporting_flag = "",
                                              from = NULL, to = NULL,
                                              report_version = c("Final", "Preliminary"),
                                              report_type = c("New", "Revision"),
                                              report_coverage = c("FULL", "PARTIAL"),
                                              notes = ""){
  
  openxlsx::writeData(wb, 1, x = reporting_flag, startRow = 12, startCol = 3)
  openxlsx::writeData(wb, 1, x = from, startRow = 13, startCol = 3)
  openxlsx::writeData(wb, 1, x = from, startRow = 13, startCol = 5)
  
  report_version = match.arg(report_version)
  openxlsx::writeData(wb, 1, x = report_version, startRow = 17, startCol = 3)
  
  report_type = match.arg(report_type)
  report_coverage = match.arg(report_coverage)
  openxlsx::writeData(wb, 1, x = paste0(report_type, " (", report_coverage,")"), startRow = 18, startCol = 3)
  openxlsx::writeData(wb, 1, x = notes, startRow = 12, startCol = 11)
  
  return(wb)
}

#'@name iccat_fill_report_dataset
#'@title Fills an ICCAT template workbook with a dataset
#'@usage iccat_fill_report_statistical_correspondent(wb, metadata)
#'
#'@param wb a \code{Workbook} object from \link{openxlsx}
#'@param sheet a sheet index
#'@param x an object of class \link{data.frame}
#'@param startRow start row index
#'@param startCol start column index
#'@return the modified \code{Workbook} object
#'
#'@export
iccat_fill_report_dataset = function(wb, sheet, x, startRow = 1, startCol = 1){
  openxlsx::writeData(
    wb = wb, sheet = sheet, x = x,  
    startRow = startRow, startCol = startCol,
    colNames = FALSE, rowNames = FALSE
  )
  return(wb)
}