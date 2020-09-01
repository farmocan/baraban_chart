# заголовок таблицы в зависимости от кол-ва результатов в неделе ->
#зависит сколько символов будет в одной строке

#data$result %>% as.character() %>% nchar
#res_name_test <- data$result[31] %>% as.character()
#n <- 4

#get_wrap_string.F(res_name_test, 1)


get_wrap_string.F <- function(string, result_in_week){

n_STRING <- 180 / result_in_week

result_in_week %>% print
n_STRING  %>% print

wraptitleRES <- sapply(string, function(str1){
  paste0(strwrap(str1, width = n_STRING), collapse = "\n")
}) %>% as.character()

abruptlines <- gregexpr("\n", wraptitleRES)
if(length(abruptlines[[1]]) > 1){
  
  seconderow <- abruptlines[[1]][2]
  wraptitleRES_abbr <- substr(wraptitleRES, 1, seconderow)
  
  wraptitleRES_final <- if(nchar(wraptitleRES) > nchar(wraptitleRES_abbr)) {
    paste0(substr(wraptitleRES_abbr, 1, nchar(wraptitleRES_abbr)-1), "*")
  } else {
    paste0(wraptitleRES_abbr, "*")
  }
} else {
  wraptitleRES_final <-wraptitleRES
}

return(wraptitleRES_final)
}
