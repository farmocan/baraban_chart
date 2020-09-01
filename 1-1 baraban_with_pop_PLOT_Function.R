#создание интерактивного рисунка барабанчарт
#colorDF <- lookcolor(data[c("nomer_ned", "nomer_res")])


baraban_with_pop_PLOT <- function(coursedata, data, tickdata){#, COURSENAME){

"baraban_with_pop_PLOT" %>% print
#data %>% str %>% print
#tickdata %>% str %>% print
#COURSENAME %>% print
#tickdata <- myticks
  
#coursedata <- df
#data <- mergedf_color
#tickdata <- radarticks
  
colors_pal <- data$col

l <- list()
for(r in 1:length(colors_pal)){
  l[[r]] <- rep(colors_pal[r], 4)
}

name_l <- list()
for(r in 1:length(data$result)){
  name_l[[r]] <- rep(data$result[r], 4)
}
name <- unlist(name_l)

name <- sapply(name, function(str1){
  paste0(strwrap(str1,width = 80), collapse = "\n")}
  )

my_r <- data$r_res %>% strsplit(., ", ") %>% unlist
my_theta <- data$grad_res %>% strsplit(., ", ") %>% unlist
my_color <- unlist(l)


p <- plot_ly(
  #!
  source = "baraban",
  #!
  data = data, #!!!!!!!!!
  type = 'scatterpolar',
  mode = 'lines',
  textposition = 'middle left'
  ) %>%
  add_trace(
    r = my_r,
    theta = my_theta,
    fill = 'toself',
    fillcolor = my_color,
    line = list(width = .5, color = "white"),
    name = name,
    hoverinfo = 'text'
  ) %>%
  layout(
    polar = list(radialaxis = list(range = c(0, 1),
                                                tickmode = "array",
                                                tickvals = c(0, .5, .7, 1),
                                                ticktext = c(0, .5, .7, 1),
                                                
                                                visible = T,
                                                linewidth = 1,
                                                tickwidth = 2,
                                                gridcolor = "LimeGreen",
                                                gridwidth = 1
                 ),
                 
                 
                 angularaxis = list(rotation = 90,direction = "clockwise", 
                                    tickvals = tickdata$grad, #! from 0-2 get_tickdata_from_merg_Function
                                    ticktext = paste("нед", tickdata$no_ned))
                 )
    ) %>% 
  hide_legend() %>% config(displayModeBar = F)

#title
p <- p %>% layout(autosize = T, margin = list(l=10, r=10, b=30, t=80, pad=4))

###add n

n_data <- aggregate(n ~ nomer_ned+nedel, data = coursedata, mean)
n_data$perc <- (n_data$n/n_data$n %>% max) %>% round(., 2)
n_data <- n_data[order(n_data$nomer_ned),]
#n_data%>% tibble

n_length <- n_data$nomer_ned %>% length
shag <- 360/n_length
n_data$coord <- seq(0, 359, shag)
n_data$coord <- n_data$coord + (shag/2)


p <- p %>% add_trace(
  data = n_data,
  mode = "markers+lines", theta = ~coord, r=~perc,
  text = paste0(n_data$nedel, "\nN=", round(n_data$n, 0)), hoverinfo = 'text',
  line = list(width = 2.5, color = "darkgray"),
  marker = list(labels = n_data$n, color = "darkblue", symbol = 'x')
)
"baraban_with_pop_PLOT---END" %>% print

return(p)
}


