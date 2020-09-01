#rm(list = ls()); cat("\014")
require(magrittr); require(tibble); require(plotly); require(shiny); require(shinyjs);  require(openxlsx); require(DT); require(RColorBrewer); require(dplyr); require(shinyhelper); require(shinyWidgets); require(V8)
require(grDevices)
#bigfolder <- "D:\\cloud\\***\\"
#setwd(bigfolder)

source("funct/0-1 data_baraban_Function.R", encoding = "UTF-8")
source("funct/0-2 color_segment_Function.R", encoding = "UTF-8")
source("funct/0-3 get_tick_Function.R", encoding = "UTF-8")
source("funct/1-1 baraban_PLOT_Function.R", encoding = "UTF-8")
source("funct/1-1 baraban_with_pop_PLOT_Function.R", encoding = "UTF-8")
source("funct/1-2 hist_adjust_function.R", encoding = "UTF-8")
source("funct/1-2-1 get_wrap_string_Function.R", encoding = "UTF-8")


#course_DF <- read.csv2("data/baraban_data_UTF8.csv", encoding = "UTF-8") #course_DF %>% head; course_DF %>% tibble
course_DF <- read.csv2("data/baraban_data.csv", encoding = "UTF-8")
course_DF %>% str

course_DF$problem_type %>% table
course_DF$problem_type <- as.character(course_DF$problem_type)
course_DF$problem_type[course_DF$problem_type == "checkboxgroup"] <- "множественного выбора"
course_DF$problem_type[course_DF$problem_type == "choicegroup"] <- "выбора одного ответа"
course_DF$problem_type[course_DF$problem_type == "textline"] <- "ввода ответа"


server_radar_click7 <- function(input, output, session){

  output$info <- renderUI(a("инструкция",target="_blank",href="instruction_app2.pdf")) #guide
  observe_helpers(help_dir = "helpfiles") #!helpers
  
  COLUMNS <- c("nomer_ned", "nomer_zad", "nedel", "result", "problem_text", "n", "p")
  names(COLUMNS) <- c("номер недели",	"номер задания",	"название недели", "образовательный результат", "текст задания", 	"количество студентов",	"решаемость") #как будет называться выгрузка
  
  observeEvent(input$reset_button, {js$empty_click()}) #reset button

  df <- reactive({
    course_DF[course_DF$course == input$course_name,]
    }) #data one course
  
  name_nedel <- reactive({df()$nedel[df()$nomer_ned == selectdat()]})
  
  df1 <- reactive({
   data_baraban_F(df())
   }) #data baraban chart
  
  colorDF <- reactive({
    color_segment.F(df1()[c("nomer_ned", "nomer_res")])
    }) #color (data baraban chart)
  
  mergedf_color <- reactive({
    dat <- merge(df1(), colorDF(), by = c("nomer_ned", "nomer_res"))
    dat <- dat[order(dat$nomer_ned),]
    return(dat)
    }) #1.merge data baraban chart + color;  2.order 
  
  radarticks <- reactive({
    get_tick.F(df1())
    }) #axis position
  
  output$title_baraban <- renderUI({HTML(paste("<b>Образовательные результаты курса:</b>", 
                                               input$course_name, sep="<br/>"))})
  
  #output$courseRADAR -- baraban chart two button
  # --1
  output$courseRADAR <- renderPlotly({baraban_PLOT(mergedf_color(), radarticks())})
  
  observeEvent(input$get_population,{
    output$courseRADAR <- renderPlotly({baraban_with_pop_PLOT(df(), mergedf_color(), radarticks())})
  })
  
  observeEvent(input$get_standart,{
    output$courseRADAR <- renderPlotly({baraban_PLOT(mergedf_color(), radarticks())})
  })
  # <- depend on two button
  
  # --2
  observeEvent(input$pop, {
    if(input$pop == T){
      output$courseRADAR <- renderPlotly(baraban_with_pop_PLOT(df(), mergedf_color(), radarticks()))
      } else {
        output$courseRADAR <-  renderPlotly({baraban_PLOT(mergedf_color(), radarticks())})
  }
  })
  
  #!
  select_element <- reactive({event_data(event = "plotly_click", source = "baraban")}) #source in 1-1 baraban_PLOT_function and baraban_with_plot
  
  selectdat <- reactive({
    mergedf_color()$nomer_ned[mergedf_color()$col == levels(mergedf_color()$col)[select_element()$curveNumber]]
    })
  
  observeEvent(input$reset, {
    js$resetClick()
  })
  
  #nedel get by click on baraban chart
  nedeldata <- reactive({
    print(paste("неделя:", selectdat()))
    
    colorDF3 <- color_segment.F(df1()[c("nomer_ned", "nomer_res", "result")])
    
    dat <- merge(df(), colorDF3, by = c("nomer_ned", "result"))
    dat <- dat[c("nedel", "nomer_ned", "nomer_res", "result", "nomer_zad", "problem_attrname", "problem_type", "problem_text", "n", "p", "col")]
    dat <- dat[dat$nomer_ned == selectdat(),]
    
    return(dat)
    })
  
  TITLE_NED <- reactive({
    nedeldata()$nedel %>% unique()
  })
  
  output$title_hist <- renderUI({if (length(selectdat()) == 0) {
    HTML("При нажатии на один из цветных сегментов, 
          <br/>здесь будут представлены данные о  решаемости заданий, 
          <br/>которые направлены на измерение образовательных результатов по теме"
         )
  } else { 
    HTML(paste("<b>Все задания недели:</b>", TITLE_NED(), sep="<br/>"))}
    })
  
  output$titledashboard <- renderUI(HTML("<font size='4'>Оценка достижения образовательных <br/>результатов студентами онлайн-курсов</font>"))
  #output$titledashboard <- renderUI(HTML(""))
  
  #if not click -> NULL
  output$hist_plot <- renderPlotly({
    if(length(selectdat()) == 0){
      NULL
      } else {
        hist_PLOT_adjust(nedeldata()) #hist_PLOT_adjust(nedeldata)
      }
      })

  #download xlsx. full course or just week. data
  datasetOtput <- reactive({
    if (length(selectdat()) == 0) {
      d1 <- df()
    } else {
      d1 <- nedeldata()
    }
    d1 <- d1[order(d1$nomer_ned, d1$nomer_zad),]
    d1 <- d1[COLUMNS]
    d1$p <- round(d1$p, 2)
    names(d1) <- names(COLUMNS)
    return(d1)
  })
  
  #name of saved result
  output$downloadData <- downloadHandler(
    filename = function() {
      if (length(selectdat()) == 0) {
      paste(input$course_name, ".xlsx")
      } else {
      paste0(gsub(":", "_", name_nedel()), ".xlsx")
      }
    },
    content = function(file) {
      write.xlsx(datasetOtput(), file)#, row.names = FALSE)
      }
    )
  
  
  output$test_lab <- renderPrint(TextVariable())
  
  output$number_cells <- renderTable({
    select_element()
  })
  
  output$click <- renderText({unlist(select_element())})
  
    
  }


jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page
empty_click <- "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-A', 'null'); }"
### UI ###
ui__radar_click7 <-  fluidPage(
  useShinyjs(),                                           # Include shinyjs in the UI
  
  #titlePanel("Оценка достижения образовательных результатов студентами онлайн-курсов"),
  
  fluidRow(
    column(5, br(),
           htmlOutput("titledashboard"), br(),
           uiOutput("info")
    ),
    column(7, selectInput(inputId = 'course_name', label = 'Курс', width = '1200px', choices = unique(course_DF$course), selected = unique(course_DF$course)[2]),
           downloadButton("downloadData", "Скачать"),
           extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-baraban', 'null'); }"),
           actionButton("reset", "сбросить")
    )
  ),
  #  fluidRow(column(5, offset = 5,
  #    downloadButton("downloadData", "Скачать"),
  #    extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('plotly_click-baraban', 'null'); }"),
  #    actionButton("reset", "сбросить")
  #    )),
  hr(),
  
  fluidRow(
    column(5, 
           htmlOutput("title_hist"),
           plotlyOutput("hist_plot", height = "830px")),
    column(7,
           htmlOutput("title_baraban")  %>% helper(type='markdown', content = "baraban_ispr", buttonLabel = "ВЫХОД"), actionButton("get_population", "ВКЛ"), actionButton("get_standart", "ВЫКЛ"),
           #switchInput("pop", value = F, label = "Население", onLabel = "ВКЛ", offLabel = "ВЫКЛ"),
           plotlyOutput("courseRADAR", height = "800px"),## %>% helper(type='markdown', content = "q1", buttonLabel = "ВЫХОД"))#, textOutput("test_lab"), textOutput("number_cells")
           textOutput("click"))
  )
)

shinyApp(server= server_radar_click7, ui = ui__radar_click7)



