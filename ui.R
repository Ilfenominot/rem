

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( # can I move this out of a fluidpage?
    # tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
    tags$link(rel = "stylesheet", type = "text/css", href = "my_style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Roboto:300italic,400,700"),
    style="font-family: 'Roboto';",
navbarPage(
  theme = shinytheme("cosmo"),
title = 'REM',
  tabPanel(
    'Sign-In', icon = icon("user")
    , textInput('ou_email',
                'OU Email',
                placeholder = 'example@oakland.edu')
    , p("1. identity and contact: name, email, and office phnoe")
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
           textOutput("current_time")
           , selectInput('easy_workdays',
                       'Weekday(s)',
                       multiple=TRUE,
                       workdays)
           , verbatimTextOutput('out_easy_workdays')
           , verbatimTextOutput("dt_select_test")
           , verbatimTextOutput("dt_select_test2")
           # , submitButton("Submit") # delays ALL inputs on page until press
           , actionButton("submit_sched", "Submit")
         ),
         
         # Show a plot of the generated distribution
         mainPanel(
           DT::dataTableOutput("dt_test")
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
    , p("1. Conversation suggestions")
    , bookmarkButton()
    )
)
)
  )
