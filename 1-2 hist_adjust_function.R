#create subplot
hist_PLOT_adjust  <- function(plotDATA){
  
  colnames_NED <- plotDATA %>% names
  #plotDATA <- nedeldata
  
  plotDATA %>% names
  tab <- getTAB(plotDATA)
  
  hist_l <- list()
  for(ReS in tab$nomer_res){
    #ReS %>% print
    #result %>% print
    #ReS <- tab$nomer_res[1]
    
    one_res_table <- tab[tab$nomer_res == ReS,] #agg result table
    
    ONE_result <- plotDATA[plotDATA$nomer_res == ReS,] #get
    ONE_result <- ONE_result[order(ONE_result$nomer_zad),] #order
    
    if(one_res_table$add>0){
      mtr <- matrix(nrow = one_res_table$add, ncol = length(colnames_NED)) %>% as.data.frame()
      names(mtr) <- colnames_NED
      mtr$nomer_zad <- (max(ONE_result$nomer_zad)+1):(max(ONE_result$nomer_zad)+one_res_table$add)
      mtr$problem_text <- "some"
      mtr$p <- 0
      ########
      ONE_result_add <- rbind(ONE_result, mtr)
      ################
    } else {
      ONE_result_add <- ONE_result
    }
    
    p <- get_histplot_f(ONE_result_add, ONE_result, one_res_table)
    
    hist_l[[ReS]] <- p
  }
  
  len_histplot <- length(hist_l)
  different <- (3 - len_histplot)
  
  #add empty plots if nessesary
  if(different > 0){
    #different <- 2
    for(ind in 1:different){
      hist_l[[len_histplot + ind]] <- plotly_empty(type = "bar") %>% layout(autosize = T, margin = list(l=10, r=10, b=50, t=100, pad=4))
    }
  } else {
  }
  
  #sp <- subplot(hist_l, nrows = length(hist_l), margin = c(.1, .1, .08, .08)) %>%
  #subplot(hist_l)
  sp <- subplot(hist_l, nrows = length(hist_l),
                margin = c(.1, .01, .05, .1)) %>% 
    config(displayModeBar = F) %>% 
    layout(showlegend = FALSE) %>%
    
    add_annotations(text = "№ вопроса",
                    x = 0.5, y = 0,
                    
                    yref = "paper", xref = "paper", xanchor = "middle", yanchor = "bottom",
                    
                    font=list(size = 13), #, color = "black"
                    
                    yshift = -45, showarrow = FALSE,
                    font = list(size = 15)
    ) 
  
  
  return(sp)
}
#hist_PLOT_adjust(nedeldata)

#stat for nedel
getTAB <- function(dat){
  #dat <- plotDATA  
  tab_01 <- aggregate(p ~ nomer_res + result, data = dat, FUN= function(x){c(length(x), mean(x))})
  tab_01 <- data.frame(tab_01[,1:2], tab_01$p %>% data.frame())
  names(tab_01)[3:4] <- c("num", "mean_p")
  
  tab_02 <- aggregate(n ~ nomer_res + result, data = dat, mean)
  
  tab_03 <- aggregate(col ~ nomer_res + result, data = dat, unique)
  
  tab <- merge(merge(tab_01, tab_02, by = c("nomer_res", "result")), tab_03, by = c("nomer_res", "result"))
  tab$result <- as.character(tab$result)
  tab <- tab %>% as.data.frame()
  tab <- tab[c("result", "nomer_res", "num", "col")]
  
  tab$add <- max(tab$num) - tab$num
  return(tab)
}


#adjusted 
get_histplot_f <- function(emul_data, orig_data, oneres_data){
  #emul_data <- ONE_result_add
  #orig_data <- ONE_result
  #oneres_data <- one_res_table
  #emul_data %>% str
  
  emul_data$nomer_zad_dopzad <- paste0("zad_", emul_data$nomer_zad)
  emul_data$nomer_zad_dopzad <- factor(emul_data$nomer_zad_dopzad, levels = emul_data$nomer_zad_dopzad[order(emul_data$nomer_zad)])
 
  #hover text (assesment)
  my_text <- emul_data$problem_text
  type_of_attr <- as.character(emul_data$problem_type)
  #type_of_attr <- "проверка"
  wraptext <- sapply(my_text, function(str1){
    paste0(strwrap(str1,width = 80), collapse = "\n")
  })
  wraptext2 <- paste0(wraptext, "\n(тип: ", type_of_attr, ")")
  
  #plot addDATA
  p <- plot_ly(
    data = emul_data,
    type = "bar",
    
    x = ~nomer_zad_dopzad,
    y = ~p,
    marker = list(color = oneres_data$col), #, line = list(color = 'rgb(8,48,107)', width = 1.5)
    
    hovertext = wraptext2, hoverlabel = list(align = "left"), hoverinfo="text"  
  )
  
  for_yaxis <- list(fixedrange = TRUE, range = c(0,1), tickmode = "array", tickvals = seq(0 , 1, .2), ticktext = seq(0 , 1, .2), 
                    title = "сложность (p)")
  
  
  #+ bars in plot (by orig data)
  for_xaxis <- list(tickvals = as.character(emul_data$nomer_zad_dopzad[!is.na(emul_data$col)]),
                    ticktext = as.character(orig_data$nomer_zad),
                    tickmode = 'array',
                    
                    fixedrange=TRUE, title = "№ вопроса")
  
  
  #title
  titleRES <- oneres_data$result %>% as.character()
  #titleRES <- substring(titleRES, 1, 120)
  wraptitleRES_final <- sapply(titleRES, function(str1){
    paste0(strwrap(str1,width = 80), collapse = "\n")
  })
  
  p <- p %>% layout(yaxis = for_yaxis,
                    xaxis = for_xaxis, 
                    
                    
                    autosize = T, margin = list(l=10, r=10, b=50, t=100, pad=4),
                    barmode = 'group',
                    
                    annotations = list(text = paste0("<b>результат:</b>\n",  wraptitleRES_final), 
                                       
                                       xref = "paper", yref = "paper", yanchor = "bottom", xanchor = "left", align = "left", 
                                       x = 0, y = 1.1, showarrow = FALSE)
  )
  
  
  return(p)
}
