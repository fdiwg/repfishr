function(data, metadata, path){
  writexl::write_xlsx(list(data = data, metadata = metadata), path = path)
  return(path)
}