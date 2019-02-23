

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
  # Sign-in tab ##########################################################################
  
  observeEvent(input$sign_in, {
    
    if(input$ou_email == "noahpollock@oakland.edu"){
    showModal(modalDialog(
      title = "Enter or Confirm Your Details",
      textInput('full_name',
                'Full Name',
                placeholder = 'Matilda Wilson'),
      textInput('job_title',
                'Job Title',
                placeholder = 'Founder, Oakland University'),
      easyClose = TRUE,
      footer = actionButton('confirm_id','Submit')
    ))
    } else {
      showModal(modalDialog(
        title = tags$b("You must enter a valid OU Email!",style="color:red;")
      ))
    }
  })
  
  
  # Schedule tab ##################################################################################
  
  output$current_time <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  # Analytics tab ##################################################################################
  
  
  # Help tab ########################################################################################
  
  # show Random Employee Meetup, move, mingle
  

  
  output$dt_test = DT::renderDataTable({
    tdata <- date_df %>% 
      group_by(month) %>%
      mutate(abb = substr(abb,row_number(),row_number())) %>%
      bind_cols(index = group_indices(.)) %>%
      ungroup() %>%
      select(-week_of_year,-month)
    
    datatable(tdata
              , colnames = c("",workdays,"index")
              , rownames = FALSE #add to dt global options
              , selection = list(target = 'cell')
              , options = list(
                dom = 't'
                , pageLength = length(unique(date_df$week_of_year))
                , columnDefs = list(
                  list(visible=FALSE, targets=ncol(tdata)-1),
                  list(className = 'dt-center',targets=0:(ncol(tdata)-1))) # center all
                )
              ) %>%
      formatStyle(
      'abb','index',
      target = 'row',
      backgroundColor = styleEqual(
        unique(tdata$index), rep(c(v_light_tan,'white'),2)) # should always be four months
    ) %>%
      formatStyle('abb'
                  ,fontWeight = 'bold'
                  ,backgroundColor = ou_gold) %>%
      formatStyle(names(tdata)
                  ,fontSize = '14pt')
  }) 
  
  ### Iterate Month Date Selector DataTables #########################################################
  output$date_select <- renderUI({
    
    variable_output <- lapply(unique(date_df$month), function(i) {
      
      month_dt <- paste0("table1_",i)
      DT::dataTableOutput(month_dt)
      
    })
    local_reactive_inspect_vars()
    do.call(tagList, variable_output)
  })
  
  local_reactive_inspect_vars <- reactive({
    for (i in unique(date_df$month)) {
      local({
        my_i <- i
        month_dt <- paste0("table1_", my_i)
        output[[month_dt]] <-  DT::renderDataTable({
          datatable(date_df %>% filter(month==my_i))
        })
      })
    }
  })
  
  # TESTING #########################################################

  # Submit Button Actions ################################################################
  observeEvent(input$submit_sched, {
    showModal(modalDialog(
      title = "Changes Submitted!",
      # "This is an important message!",
      easyClose = TRUE
    ))
  })
  
  # only evaluate on submit press  
  out_easy_workdays <- eventReactive(input$submit_sched, {
    input$easy_workdays
  })
  
  output$out_easy_workdays <- renderPrint(
    # input$easy_workdays # without actionbutton
    out_easy_workdays()
    )
  
  output$send_to_gs <- renderPrint(
    data.frame(
      time = 
    )
  )
  
  output$dt_select_test = renderPrint(input$dt_test_cells_selected)
  
  output$dt_select_test2 = renderPrint({
    data.frame(date_df_full %>% select(-month,-week_of_year))[input$dt_test_cells_selected]
  })
  
  # # create 19 breaks and 20 rgb color values ranging from white to red
  # brks <- quantile(df, probs = seq(.05, .95, .05), na.rm = TRUE)
  # clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  # {paste0("rgb(255,", ., ",", ., ")")}
  # datatable(df) %>% formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
  
})
