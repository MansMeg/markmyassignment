download_test_suites <- function(){
  test_suites <- get_config()
  temp_folder_name <- paste(tempdir(), "markmyassignment", sep="/")
  if(!file.exists(temp_folder_name)) dir.create(temp_folder_name)
  
  for(j in seq_along(test_suites$test_suite$path)){ 
    suite_folder_name <- paste(temp_folder_name, 
                               paste0("test_suite", as.character(j)), sep="/")
    if(!file.exists(suite_folder_name)) dir.create(suite_folder_name) 
    
    if(test_suites$test_suite$type[j] == "local"){
      files <- dir(test_suites$test_suite$path[j])
      for(k in seq_along(files)) {
        file.copy(from = paste0(test_suites$test_suite$path[j], files[k]),
                  to = paste(suite_folder_name, files[k], sep="/"), overwrite = TRUE)
      }
      next()
    }
    
    if(test_suites$test_suite$type[j] == "github_api"){
      
    }
    if(test_suites$test_suite$type[j] %in% c("http", "https")){
      
    }
    test_urls <-
      get_test_urls(test_suites$test_suite$path[j], 
                    test_suites$test_suite$type[j])
    
  }
}


# get_test_urls
# 
# 
# get_test_list
# 
# readLines()