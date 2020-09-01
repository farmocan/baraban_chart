#оттенки
makeTransparent = function(..., alpha=0.5){
  require(grDevices)
  
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  
  return(newColor)
}

#создать цвета для каждого сегмента
#Set3	12
#Paired	12
color_segment.F <- function(twocol_data){
  #присвоить цвета каждому сегменту, кол-во оттенков в зависимости сколько сегментов в одной неделе
  require(RColorBrewer)
  require(ggplot2)
  #test
  #twocol_data <- data_baraban_F(df)[c("nomer_ned", "nomer_res")]
  
  table0 <- aggregate(nomer_res ~nomer_ned,  data = twocol_data, FUN = length) #сколько оттенков в каждом цвете
  vec <- table0$nomer_ned %>% unique() %>% length() #кол-во цветов
  #vec = 15
  
  if(vec < 13){#если менее 13, тогда один колорбревд
    namecol <- brewer.pal(n = vec, "Set3")
    #brewer.pal(n = 12, "Set3")
  } else {
    p1 <- 12
    p2 <- vec-12
    namecol <- c(brewer.pal(n = p1, "Set3"), suppressWarnings(brewer.pal(n = p2, "Paired")[1:p2]))
  }
  
  #рисунок, чтобы посмотреть цвета - основные цвета недели
  {
  df  <- data.frame(namecol)
  p   <- ggplot(data=df, aes(x=namecol, y=1, fill = namecol)) +
          geom_bar(stat="identity") +
          scale_fill_manual(values = namecol) + theme(axis.text.x = element_text(angle = 90, hjust = 0))
  #p%>% print
  }#plot
  
  #сделать оттенки 
  color_l <- list()
  for(l in 1:nrow(table0)){
    #l <- 1
    #l %>% print
    
    ned <- table0$nomer_ned[l] %>% as.numeric()
    res <- table0$nomer_res[l] %>% as.numeric()
    shag <- .6/res  
    cbind(ned, res, shag)
    
    if(res == 1){
    color_l[ned][[1]] <-  namecol[l]
    } else {
      color_l[ned][1] <-  namecol[l]
      for(r in 2:res){
      color_l[[ned]][r] <-  makeTransparent(namecol[l], alpha= 1- shag * (r-1))
      }
    }
  }
  col <- color_l %>% unlist
  
  resdatacoldf <- data.frame(twocol_data, col)
  
  p2 <- ggplot(data=resdatacoldf, aes(x=col, y=1, fill = col)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = col) + theme(axis.text.x = element_text(angle = 90, hjust = 0))
  #p2 %>% print
  
  return(resdatacoldf)
}

#color_segment.F(data_baraban_F(df)[c("nomer_ned", "nomer_res")])