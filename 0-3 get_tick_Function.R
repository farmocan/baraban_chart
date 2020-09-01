#t <- data_baraban_F(df)
#get_tick.F(t) %>% tibble

get_tick.F <- function(data){
#вторая версия. отметки строго в зависисмости сколько результатов в неделе
n_ned <- data$nomer_ned %>% unique()

tick_l <- list()
for(n in n_ned){
  #n <- n_ned[4]
  n_result <- data$nomer_res[data$nomer_ned == n] %>% min

  coordinate <- data$grad_res[data$nomer_ned == n & data$nomer_res == n_result]
  
  name <- data$nedel[data$nomer_ned == n & data$nomer_res == n_result] %>% as.character()
  
  coord_tick <- coordinate %>% strsplit(., ", ") %>% lapply(., "[[",2) %>% as.numeric
  
  tick_l[[n]][["grad"]] <- coord_tick
  tick_l[[n]][["no_ned"]] <- n %>% as.numeric
  tick_l[[n]][["name"]] <- name
}

res_DF <- do.call("rbind", tick_l) %>% as.data.frame()
res_DF$grad <- as.numeric(as.character(res_DF$grad))
res_DF[c("no_ned", "name")] <- apply(res_DF[c("no_ned", "name")], 2, as.character)

return(res_DF)
}
