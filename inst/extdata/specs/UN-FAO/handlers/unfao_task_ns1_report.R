function(sender, data, metadata, path){
  
  requireNamespace("openxlsx")
  
  task_wb = openxlsx::buildWorkbook(list(data = data), asTable = TRUE, tableStyle = "TableStyleLight8")
  
  #save workbook
  openxlsx::saveWorkbook(task_wb, file = path, overwrite = TRUE)
  return(path)
}