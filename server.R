

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Sign-in tab ##########################################################################
  
  observe({
    # prepopulate user details if user exists
    input$sign_in
    
    if(input$ou_email %in% employee_df$ou_email){
      employee_df <- employee_df %>%
        filter(ou_email==input$ou_email)
      
      updateTextInput(session, "full_name",value = employee_df$full_name)
      updateTextInput(session, "phone",value = employee_df$phone)
      updateTextInput(session, "job_title",value = employee_df$title)
      updateTextInput(session, "department",value = employee_df$department)
    }
  })
  observeEvent(input$sign_in, {
    
    # if contains @oakland.edu
    if(grepl("@oakland.edu",input$ou_email)){
    showModal(modalDialog(
      title = "Enter or Confirm Your Details",
      textInput('full_name',
                'Full Name',
                placeholder = 'Noah Pollock'),
      textInput('phone',
                'Phone',
                placeholder = '248-370-3253'),
      textInput('job_title',
                'Job Title',
                placeholder = 'Assistant Director of Assessment'),
      textInput('department',
                'Department',
                placeholder = 'Career Services'),
      easyClose = FALSE,
      footer = actionButton('confirm_id','Looks Good!')
    ))
    } else {
      showModal(modalDialog(
        title = tags$b("You must enter a valid OU Email!",style="color:red;")
      ))
    }
  })
  
  # hide all other tabs until sign in
  observe({
    if(input$sign_in == 0 | is.null(input$sign_in)){
      hideTab(inputId = "tabs", target = 'Schedule')
      hideTab(inputId = "tabs", target = 'Analytics')
    } else {
      showTab(inputId = "tabs", target = 'Schedule')
      showTab(inputId = "tabs", target = 'Analytics')
    }
  })
  
  # events/actions when user confirms their id and details
  observeEvent(input$confirm_id, { 
    
    # round-up user details
    user_details <- c(
      input$ou_email
      ,input$full_name
      ,input$phone
      ,input$job_title
      ,input$department)
    
    # if employee already exists
    if(input$ou_email %in% employee_df$ou_email){
    # identify the employee record row number
    employee_index <- (employee_df %>%
      filter(ou_email==input$ou_email))$employee_index
    # updated user details in googlesheets
    # eventually, leave alone if details are identical
    gs_edit_cells(ss,ws = "employee",byrow = TRUE
                  ,input = user_details
                  ,anchor = paste0("A",employee_index + 1))
    } else {
      gs_add_row(ss, ws = "employee",input = user_details)
    }
    # load user availability data, if any, into schedule
    available_df <<- available_df %>%
      filter(ou_email==input$ou_email)

    if(input$ou_email %in% available_df$ou_email){
      available_df <<- available_df %>%
        filter(hour==input$timeslot) %>%
        filter(sequence==max(sequence))
    } else {
      available_df_sequence <- 0
    }
    # change tabs
    updateTabsetPanel(session, "tabs", "Schedule")
    # and close the modal window
    removeModal()
  }
  )
  
  # observe({
  #   # reset available_df every time timeslot is changed
  #     # and/or every time data is submitted
  #   input$timeslot
  #   input$submit_sched
  #   
  #   available_df <- ss %>%
  #       gs_read_csv(ws = "available") %>%
  #       filter(hour==input$timeslot) %>%
  #       filter(sequence==max(sequence))
  # })
  
  # Schedule tab ##################################################################################
  
  # available_df <- reactive({
  #   # load user availability data, if any, into schedule
  #   f_available_df <- f_available_df %>%
  #     filter(ou_email==input$ou_email)
  #   
  #   if(input$ou_email %in% f_available_df$ou_email){
  #     f_available_df <- f_available_df %>%
  #       filter(hour==input$timeslot) %>%
  #       filter(sequence==max(sequence))
  #   } else {
  #     available_df_sequence <- 0
  #   }
  #   f_available_df
  #   })
  
  output$current_time <- renderText({
    invalidateLater(1000, session)
    paste("Right Now: ", Sys.time())
  })
  
  # scheduling calendar
  output$calendar_dt = DT::renderDataTable({
    tdata <- date_df %>% 
      group_by(month) %>%
      mutate(abb = substr(abb,row_number(),row_number())) %>%
      bind_cols(index = group_indices(.)) %>%
      mutate(time_index = input$timeslot) %>%
      ungroup() %>%
      select(-week_of_year,-month)
    
    # load user availability data, if any, into schedule
    if(input$ou_email %in% available_df$ou_email){
    Sys.sleep(5)
    available_df <<- ss %>%
      gs_read_csv(ws = "available") %>%
      filter(hour==input$timeslot,
             ou_email==input$ou_email) %>%
      filter(sequence==max(sequence))
    }
    
    # identify which cells need to be colored
    if(nrow(available_df>0)){
      pre_select <- data.frame(
        row_index = NA,
        col_index = NA
      )
      for( i in 1:length(workdays)){
        i_name <- workdays[i]
        row_index <- which(grepl(
          paste( # create regex or list
            as_date(available_df$start) # strip out time part of date
            , collapse = "|"), date_df_full[[i_name]]))
        col_index <- if(length(row_index>0)) i

        pre_select <- rbind(pre_select,data.frame(row_index,col_index))
      }
      pre_select <- pre_select %>%
        filter(!is.na(row_index)) %>%
        data.matrix()
      select_list <- list(target = 'cell', selected = pre_select)
    } else {
      select_list <- list(target = 'cell')
    }
    
    datatable(tdata
              , colnames = c("",workdays,"index","time_index")
              , rownames = FALSE #add to dt global options
              , selection = select_list
              , options = list(
                dom = 't'
                , pageLength = length(unique(date_df$week_of_year))
                , columnDefs = list(
                  list(visible=FALSE, targets=c(ncol(tdata)-1,ncol(tdata)-2)),
                  list(className = 'dt-center',targets=0:(ncol(tdata)-2))) # center all
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

      # formatStyle('Tuesday','time_index',
      #             backgroundColor = styleEqual(
      #               unique(tdata$time_index), 'orange')
      # ) %>%
      # will have to implement for analytics page
      # because cannot style two backgrounds in same cell
      # formatStyle('Tuesday','index',
      #             background = styleColorBar(
      #               tdata$index, 'gray')
      #             , backgroundSize = '100% 10%'
      #             , backgroundRepeat = 'no-repeat'
      #             , backgroundPosition = 'center bottom'
      # )
  }) 
  
  observeEvent(input$submit_sched, {
    # add selections to googlesheet
    starts <- as_datetime(
      data.frame(
        date_df_full %>% 
          select(-month,-abb,-week_of_year))[input$calendar_dt_cells_selected]
      ,tz = "EST"
    )
    
    hour(starts) <- as.numeric(input$timeslot)
    
    if(nrow(available_df>0)) available_df_sequence <- unique(available_df$sequence)
    
    send_avail_df <- data.frame(
      ou_email = input$ou_email
      , hour = as.numeric(input$timeslot)
      , sequence = available_df_sequence + 1 # the next sequence
      , start = starts
      , end = starts + 60^2
    )
    gs_add_row(ss, ws = "available",input = send_avail_df)
    
    # tell the user they're changes are saved.
    showModal(modalDialog(
      title = "Changes Submitted!",
      # "This is an important message!",
      easyClose = TRUE
    ))
  })

  # Analytics tab ##################################################################################
  
  
  # Help tab ########################################################################################
  
  # show Random Employee Meetup, move, mingle
  
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

  # reactive inputs
  # file_df <- reactive({
  #   
  #   if(input$select_dataset!="Upload"){}
  #   })
  
  # Submit Button Actions ################################################################
  
  # only evaluate on submit press
  out_easy_workdays <- eventReactive(input$submit_sched, {
    input$easy_workdays
  })
  
  output$out_easy_workdays <- renderPrint(
    # input$easy_workdays # without actionbutton
    out_easy_workdays()
    )
  
  output$send_to_gs <- renderPrint({
    
    
    starts <- as_datetime(
      data.frame(
        date_df_full %>% 
          select(-month,-abb,-week_of_year))[input$calendar_dt_cells_selected]
      ,tz = "EST"
      )

    hour(starts) <- as.numeric(input$timeslot)

    send_avail_df <- data.frame(
      ou_email = input$ou_email
      , hour = as.numeric(input$timeslot)
      , sequence = unique(available_df$sequence) + 1 # the next sequence
      , start = starts
      , end = starts + 60^2
    )
    send_avail_df
  })
  
  output$dt_select_test = renderPrint(input$calendar_dt_cells_selected)
  
  output$dt_select_test2 = renderPrint({
    data.frame(date_df_full %>% select(-month,-abb,-week_of_year))[input$calendar_dt_cells_selected]
  })
  
  output$button_state = renderPrint({
    input$confirm_id
  })
  
})
