#app
library(shinybusy)
library(shiny)
library(shinyWidgets)
library(shinyBS)

source("utils.R")

ui <- fluidPage(
  tags$head(style = "z-index: 10000000000",
            shinybusy::add_busy_spinner(spin = "flower",
                                        color = "#27499d",
                                        onstart = FALSE,
                                        margins = c(30, 20),
                                        timeout = 500,
                                        position = "bottom-right"),
  ),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 tags$head(
                   tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; }
                                    #inline .form-group { display: table-row;}"),
                   tags$style(HTML("
                              .nav-tabs > li {
                                width: 50%;
                              }
                              .nav-tabs > li > a {
                                width: 100%;
                                text-align: center;
                                font-size: 16px;
                              }
                            ")),
                   tags$style(
                     HTML(
                       "@keyframes rainbowText {
                          0%{background-position:0% 50%}
                          50%{background-position:100% 50%}
                          100%{background-position:0% 50%}
                        }
                        .rainbow-text {
                          background: linear-gradient(45deg, red, orange, yellow, green, blue, indigo, violet);
                          background-size: 400% 400%;
                          -webkit-background-clip: text;
                          color: transparent;
                          animation: rainbowText 5s ease infinite;
                          -webkit-text-fill-color: transparent;
                          font-weight: bold;
                          font-size: 20px;
                        }"
                     )
                   )
                 ),
                 tags$div(id = "CT_wellPanel",
                          style = "border: 3px solid #3d83d6; 
                                   border-radius: 15px;
                                   padding: 15px;",
                          
                          tabsetPanel(type = "tabs", id = "tabs",
                                      tabPanel("Per centrifuge", value = "CE",
                                               br(),
                                               fluidRow(
                                                 column(6,
                                                        h4("Total slots", style = "font-weight: bold;")
                                                 ),
                                                 column(6,
                                                        numericInput(inputId = 'CE_nP',
                                                                     label = NULL,
                                                                     min = 1,
                                                                     value = 10,
                                                                     step = 1)
                                                 ),
                                               ),
                                               fluidRow(
                                                 column(6,
                                                        shinyBS::popify(
                                                          el = tags$div(
                                                            tags$h4("Tolerance", style = "display: inline; font-weight: bold;"),
                                                            icon("question-circle", style = "font-size: 18px;")
                                                          ),
                                                          title = NULL,
                                                          content = "Maximum distance <b>(as a percentage of the radius)</b> that the center of mass can move away from the center of the centrifuge."
                                                        ),
                                                 ),
                                                 column(5,
                                                        numericInput(inputId = 'CE_tolerance',
                                                                     label = NULL,
                                                                     min = 0,
                                                                     value = 0,
                                                                     step = 1)
                                                 ),
                                                 column(1,
                                                        h4("%", style = "font-weight: bold;")
                                                 ),
                                               ),
                                               
                                               br(),
                                               actionBttn(
                                                 inputId = "CE_run",
                                                 label = "Run",
                                                 color = "primary",
                                                 style = "jelly",
                                                 block = TRUE
                                               )
                                      ),
                                      tabPanel("Per tube", value = "TU",
                                               br(),
                                               fluidRow(
                                                 column(6,
                                                        h4("Total slots", style = "font-weight: bold;")
                                                 ),
                                                 column(6,
                                                        numericInput(inputId = 'TU_nP',
                                                                     label = NULL,
                                                                     min = 1,
                                                                     value = 10,
                                                                     step = 1)
                                                 )
                                               ),
                                               fluidRow(
                                                 column(6,
                                                        h4("Total tubes", style = "font-weight: bold;")
                                                 ),
                                                 column(6,
                                                        numericInput(inputId = 'TU_nT',
                                                                     label = NULL,
                                                                     min = 1,
                                                                     value = 4,
                                                                     step = 1)
                                                 )
                                               ),
                                               fluidRow(
                                                 column(6,
                                                        shinyBS::popify(
                                                          el = tags$div(
                                                            tags$h4("Tolerance", style = "display: inline; font-weight: bold;"),
                                                            icon("question-circle", style = "font-size: 18px;")
                                                          ),
                                                          title = NULL,
                                                          content = "Maximum distance <b>(as a percentage of the radius)</b> that the center of mass can move away from the center of the centrifuge."
                                                        ),
                                                 ),
                                                 column(5,
                                                        numericInput(inputId = 'TU_tolerance',
                                                                     label = NULL,
                                                                     min = 0,
                                                                     value = 0,
                                                                     step = 1)
                                                 ),
                                                 column(1,
                                                        h4("%", style = "font-weight: bold;")
                                                 ),
                                               ),
                                               
                                               br(),
                                               actionBttn(
                                                 inputId = "TU_run",
                                                 label = "Run",
                                                 color = "primary",
                                                 style = "jelly",
                                                 block = TRUE
                                               )
                                      )
                          )
                 ),
                 br(),
                 tags$div(id = "GRA_wellPanel",
                          style = "border: 3px solid #3d83d6; 
                                   border-radius: 15px;
                                   padding: 15px;",
                          
                          h4("Graphical parameters", style = "font-weight: bold; font-style: italic; text-align: center;"),
                          br(),
                          fluidRow(
                            column(6,
                                   p("Number of plot rows")
                            ),
                            column(3,
                                   numericInput(inputId = 'plot_row',
                                                label = NULL,
                                                min = 1,
                                                value = 1,
                                                step = 1)
                            ),
                            column(3, align = "center",
                                   actionGroupButtons(
                                     inputIds = c("less_row", "more_row"),
                                     labels = list("-", "+"),
                                     status = "primary",
                                     fullwidth = T
                                   )
                            )
                          ),
                          fluidRow(
                            column(6,
                                   p("Number of plot columns")
                            ),
                            column(3,
                                   numericInput(inputId = 'plot_col',
                                                label = NULL,
                                                min = 1,
                                                value = 1,
                                                step = 1)
                            ),
                            column(3, align = "center",
                                   actionGroupButtons(
                                     inputIds = c("less_col", "more_col"),
                                     labels = list("-", "+"),
                                     status = "primary",
                                     fullwidth = T
                                   )
                            )
                          ),
                          fluidRow(
                            column(6,
                                   p("Dot size")
                            ),
                            column(6,
                                   numericInput(inputId = 'dot_size',
                                                label = NULL,
                                                min = 0,
                                                value = 3,
                                                step = 0.5)
                            )
                          ),
                          fluidRow(
                            column(6,
                                   p("Fill slot color")
                            ),
                            column(6,
                                   colourpicker::colourInput("dot_color1", NULL, "#27499d"),
                            )
                          ),
                          fluidRow(
                            column(6,
                                   p("Empty slot color")
                            ),
                            column(6,
                                   colourpicker::colourInput("dot_color2", NULL, "#686464"),
                            )
                          ),
                          fluidRow(
                            column(6,
                                   p("Number size")
                            ),
                            column(6,
                                   numericInput(inputId = 'number_size',
                                                label = NULL,
                                                min = 0,
                                                value = 5,
                                                step = 1)
                            )
                          )
                 )
    ),
    
    mainPanel(width = 8, align = "center",
              fluidRow(
                column(12, align = "center",
                       HTML('<h1><a href="https://github.com/KieferBedoya/Counter-centrifuge-counterbalance" target="_blank"><i class="fab fa-github github-icon icon"></i></a> Counter-centrifuge-counterbalance <a href="https://paypal.me/KieferBedoya?country.x=PE&locale.x=es_XC" target="_blank"><i class="fab fa-paypal paypal-icon icon"></i></a></h1>'),
                       p(HTML('Developed by <a href="mailto:kieferbedoya@gmail.com" target="_blank">Kiefer Bedoya</a>'))
                )
              ),
              
              tags$hr(style = "border-color: #27499d;"),
              br(),
              fluidRow(
                column(6, align = "start",
                       uiOutput("num_comb")
                ),
                
                column(6, align = "end",
                       downloadBttn(
                         outputId = "download_plot",
                         label = "Download plot",
                         style = "fill",
                         color = "danger",
                         size = "s",
                         icon = icon("download")
                       )
                )
              ),
              br(),
              
              plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$more_row, {
    updateNumericInput(session, "plot_row", value = input$plot_row+1)
  })
  observeEvent(input$less_row, {
    new_row <- ifelse(input$plot_row==0, 0, input$plot_row-1)
    updateNumericInput(session, "plot_row", value = new_row)
  })
  observeEvent(input$more_col, {
    updateNumericInput(session, "plot_col", value = input$plot_col+1)
  })
  observeEvent(input$less_col, {
    new_col <- ifelse(input$plot_col==0, 0, input$plot_col-1)
    updateNumericInput(session, "plot_col", value = new_col)
  })
  
  #graficando una unica posibilidad por todos los tubos posibles
  CE_results <- reactiveValues(coordenadas = NULL,
                               patrones = NULL)
  observeEvent(input$CE_run, {
    CE_results$coordenadas <- regular_poly_coords(input$CE_nP)
    patrones <- lapply(1:input$CE_nP, function(nT) {
      patron <- centrifuge_positions(input$CE_nP, nT, tolerance = input$CE_tolerance)
      if(length(patron)>0) sample(patron, 1)[[1]]
      else vector()
    })
    names(patrones) <- 1:input$CE_nP
    CE_results$patrones <- patrones
    
    dim <- get_matrix_dimensions(input$CE_nP)
    
    updateNumericInput(session, "plot_row", value = dim$rows)
    updateNumericInput(session, "plot_col", value = dim$cols)
  })
  
  CENT_plot <- reactive({
    centrifuge_plot(polygon_coords = CE_results$coordenadas, 
                    selected_coords = CE_results$patrones, 
                    dot_size = input$dot_size,
                    dot_color1 = input$dot_color1, 
                    dot_color2 = input$dot_color2, 
                    number_size = input$number_size, 
                    row_num = input$plot_row,
                    col_num = input$plot_col)
  })
  
  sel_plot <- reactiveValues(type = NULL)
  observeEvent(input$CE_run, {
    output$plot <- renderPlot({
      tryCatch(CENT_plot(), error = function(e) {})
    }, width = 800, height = 500)
    
    sel_plot$type <- "CE"
    
    output$num_comb <- renderText({""})
  })
  
  # #graficando todas las posibilidades por un numero de tubos determinado
  TU_results <- reactiveValues(coordenadas = NULL,
                               patrones = NULL)
  observeEvent(input$TU_run, {
    TU_results$coordenadas <- regular_poly_coords(input$TU_nP)
    TU_results$patrones <- centrifuge_positions(input$TU_nP, input$TU_nT, tolerance = input$TU_tolerance)
    
    dim <- get_matrix_dimensions(length(TU_results$patrones))
    
    updateNumericInput(session, "plot_row", value = dim$rows)
    updateNumericInput(session, "plot_col", value = dim$cols)
  })
  
  TUBE_plot <- reactive({
    centrifuge_plot(polygon_coords = TU_results$coordenadas, 
                    selected_coords = TU_results$patrones, 
                    dot_size = input$dot_size,
                    dot_color1 = input$dot_color1, 
                    dot_color2 = input$dot_color2, 
                    number_size = input$number_size, 
                    row_num = input$plot_row,
                    col_num = input$plot_col)
  })
  
  observeEvent(input$TU_run, {
    output$plot <- renderPlot({
      tryCatch(TUBE_plot(), error = function(e) {})
    }, width = 800, height = 500)
    
    sel_plot$type <- "TU"
    
    output$num_comb <- renderUI({
      num_comb <- length(TU_results$patrones)
      if(length(num_comb)==1) num_comb <- ifelse(length(unlist(TU_results$patrones))==0, 0, num_comb)
      HTML(paste0('<span class="rainbow-text">', num_comb, " possible combinations found</span>"))
    })
  })
  
  #descarga
  output$download_plot <- downloadHandler(
    filename = function() paste('centrifuge-', Sys.time(), '.pdf', sep=''),
    content = function(file) {
      pdf(file = file, width=6, height=5)
      if(sel_plot$type=="CE") CENT_plot()
      if(sel_plot$type=="TU") TUBE_plot()
      dev.off()
    }
  )
}

shinyApp(ui, server)

