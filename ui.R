

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
       sidebarLayout(
         sidebarPanel(width=3
          , selectInput("timeslot", label="Timeslot",
                       choices = list("11:00 - 12:00" = 11, "12:00 - 1:00" = 12, "1:00 - 2:00" = 13), 
                       selected = 12)
          , p("Choose all the days that you are available to meet other OU faculty or staff in the selected timeslot.",
              " If you add days in a timeslot, make sure you click submit before changing timeslots or you will lose
              your selections.")
           # , textOutput("current_time")
           # , selectInput('easy_workdays',
           #             'Weekday(s)',
           #             multiple=TRUE,
           #             workdays)
           # , verbatimTextOutput('out_easy_workdays')
           # , verbatimTextOutput("send_to_gs")
           # , verbatimTextOutput("dt_select_test")
           # , verbatimTextOutput("dt_select_test2")
           # , submitButton("Submit") # delays ALL inputs on page until press
          # , verbatimTextOutput("user_email")
           , actionButton("submit_sched", "Submit")
         ),
         
         mainPanel(
           DT::dataTableOutput("calendar_dt")
           , br(), br()
         )
       )
     )),
  tabPanel(
    'Analytics', icon = icon("bar-chart")
    , h1("Days with Popular Availability")
    , p("This table showcases days that have proportionately more or less employees indicating that
        their availability.")
    , DT::dataTableOutput("analytics_dt")
    # , uiOutput("date_select")
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
