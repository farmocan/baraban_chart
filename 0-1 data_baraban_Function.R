#data_baraban_F(df[,1])
#data_baraban_F(df)
#new function
data_baraban_F <- function(data){
  "data_baraban_F" %>% print
  require(dplyr)
  #подготовить дату для рисования барабан чарт
  #
  #data <- df
  needcolumns <-  c("p", "nomer_ned", "nedel", "result")
  result_data <- needcolumns %in% names(data)
  
  if(any(result_data) == F){#проверить все ли необходимые колонки есть
    "need columns! c(p, nomer_ned, nedel, result)" %>% print
  } else {
    
  #full course
  df1 <- aggregate(p ~ nomer_ned + nedel + result, data = data, mean)
  df1 <- df1[order(df1$nomer_ned),]
  
  
  #$resuniq
  df1 <- df1 %>% group_by(nomer_ned) %>% mutate(nomer_res = 1:n())
  df1 <- df1[c("nomer_ned" , "nomer_res", "nedel", "result", "p" )]
  df1$resuniq <- paste(df1$nomer_ned, df1$nomer_res, sep="_") #кол-во сегментов
  
  df1 <- as.data.frame(df1)#так как mutate необходимо
  
  df1$resuniq <- factor(df1$resuniq, levels = df1$resuniq[order(df1$nomer_ned)])
  df1$nomer_ned <- factor(df1$nomer_ned)
  df1$p <- round(df1$p, 2)
  df1 <- df1[c("nomer_ned", "nomer_res", "resuniq", "nedel", "result", "p")]
  df1 <- df1[order(df1$nomer_ned, df1$nomer_res),]
  
  #df1 %>% tibble
  
  numbers_week <- df1$nomer_ned %>% unique %>% length()
  numbers_result <- df1$resuniq %>% unique %>% length()
  ################
  results_in_ned <- aggregate(resuniq ~ nomer_ned, data = df1, length)
  results_in_ned$step_result <- 360/length(results_in_ned$nomer_ned)
  results_in_ned$shag <- ((results_in_ned$step_result - 1) /results_in_ned$resuniq) %>% round(.,2)
  
  results_in_ned$step_result[1] <- 0 #start 0
  results_in_ned$coord_start_ned <- cumsum(results_in_ned$step_result)
  
  mdf1 <- merge(df1, results_in_ned[c("nomer_ned", "coord_start_ned", "shag")], by = "nomer_ned")
  mdf1 <- mdf1[order(mdf1$nomer_ned, mdf1$nomer_res),]
  mdf1 %>% tibble()
  
  unique_ned <- mdf1$nomer_ned %>% unique
  myresult_l <- list()
  for(week in unique_ned){
    #week <- unique_ned[1]
    #week %>% print
    
    temp <- mdf1[mdf1$nomer_ned %in% week,]
    temp$coord_end_res <- cumsum(temp$shag) + temp$coord_start_ned
    temp$coord_start_res <- temp$coord_end_res - temp$shag
    
    temp$grad_res <- paste(0, temp$coord_start_res, temp$coord_end_res, 0, sep = ", ")
    temp$r_res <- paste(0, temp$p, temp$p, 0, sep = ", ")
    
    temp <- temp[c("nomer_ned", "nomer_res", "resuniq", "nedel", "result", "p", "grad_res", "r_res")]
    
    myresult_l[[week]] <- temp
  }
  df1 <- do.call(rbind, myresult_l)
  
  #df1 %>% str
  #df1 %>% View
  return(df1)
  }
}
