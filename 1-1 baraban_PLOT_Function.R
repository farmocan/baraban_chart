#создание интерактивного рисунка барабанчарт
#colorDF <- lookcolor(data[c("nomer_ned", "nomer_res")])


baraban_PLOT <- function(data, tickdata){#, COURSENAME){

"baraban_PLOT" %>% print
#data %>% str %>% print
#tickdata %>% str %>% print
#COURSENAME %>% print


#tickdata <- myticks
#COURSENAME <- "ITPRO"

colors_pal <- data$col

l <- list()
for(r in 1:length(colors_pal)){
  l[[r]] <- rep(colors_pal[r], 4)# %>% print
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
                 #sector = c(0,80),
                 angularaxis = list(rotation = 90,direction = "clockwise", 
                                    tickvals = tickdata$grad, #! from 0-2 get_tickdata_from_merg_Function
                                    ticktext = paste("нед", tickdata$no_ned))
                 )
    ) %>% 
  hide_legend() %>% config(displayModeBar = F)

#title
p <- p %>% layout(autosize = T, margin = list(l=10, r=10, b=30, t=80, pad=4))#,
                  #title = list(
                  #  text = paste("<b>Образовательные результаты курса</b>", COURSENAME, sep = "\n"), y = 0.95, x=0
                  #  )
                  #)

"baraban_PLOT-END" %>% print
return(p)
}



