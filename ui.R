

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( # can I move this out of a fluidpage?
    # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:300italic,400,700"),
    style="font-family: 'Roboto';",
navbarPage(id = "tabs",
  theme = shinytheme("cosmo"),
title = p(strong("REM"),style=paste0("color:",ou_gold)),
  tabPanel(
    'Sign-In', icon = icon("user")
    , textInput('ou_email',
                'OU Email',
                value = 'noahpollock@oakland.edu',
                placeholder = 'example@oakland.edu')
    , actionButton("sign_in", "Sign-In / Sign-Up")
    ),

  tabPanel(
    'Schedule', icon = icon("calendar")
    , fluidPage(
       
       # # page title
       # titlePanel("Old Faithful Geyser Data"),
       
       # Sidebar with a slider input for number of bins 
       sidebarLayout(
         sidebarPanel(
           selectInput("timeslot", label = h3("Timeslot"), 
                       choices = list("11:00 - 12:00" = 11, "12:00 - 1:00" = 12, "1:00 - 2:00" = 13), 
                       selected = 12)
           , textOutput("current_time")
           , selectInput('easy_workdays',
                       'Weekday(s)',
                       multiple=TRUE,
                       workdays)
           , verbatimTextOutput('out_easy_workdays')
           , verbatimTextOutput("send_to_gs")
           , verbatimTextOutput("dt_select_test")
           , verbatimTextOutput("dt_select_test2")
           # , submitButton("Submit") # delays ALL inputs on page until press
           , actionButton("submit_sched", "Submit")
         ),
         
         mainPanel(
           DT::dataTableOutput("calendar_dt")
         )
       )
     )),
  tabPanel(
    'Analytics', icon = icon("bar-chart")
    , p("Calendar heatmap")
    , uiOutput("date_select")
    ),
  tabPanel(
    'About/Help', icon = icon("question-circle")
    , h1("Grand Idea Submission")
    , p("This web application serves a functional prototype to showcase my grand idea submission.")
    , h3("Conversation suggestions")
    , tags$li(
      tags$ul("The basics: How long at OU? What are some exciting projects you've worked on?")
    )
    , bookmarkButton()
    )
)
)
  )
