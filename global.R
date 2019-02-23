#global

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinythemes)
library(googlesheets)

# setup: https://github.com/jennybc/googlesheets/blob/master/inst/shiny-examples/10_read-write-private-sheet/global.R
# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_token.rds")
# ss <- gs_new("10_read-write-private-sheet",
#              row_extent = n, col_extent = n, input = filler)
# ss$sheet_key # 10kYZGTfXquVUwvBXH-8M-p01csXN6MNuuTzxnDdy3Pk

# # NEED TO UNCOMMENT WHEN READY TO CALL GOOGLESHEETS!!!!!!!!!!
# gs_auth(token = "shiny_app_token.rds")
# sheet_key <- "1YzlKt38qd6PbpRpucVabS6Zdb8VuyVeTGm0dE2t85fo"
# ss <- gs_key(sheet_key)

# less polluted to just use gs_read_csv
# load_gs <- function(sheet = ss, table = "employee") {
#   gs_read_csv(sheet,table)
# }

# test <- ss %>%
#   gs_read_csv(ws = "employee")

save_gs <- function(sheet = ss, data, table = "employee") {
  
  ## integrity constraints
  # 
  
  # Add the data as a new row if integrity constraints are met
  gs_add_row(sheet, input = data)
}

# time dimensions
workdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday")

month_df <- data.frame(
  stringsAsFactors = FALSE,
  full = month.name,
  abb = month.abb
)

# date_df should end before spreading
# this should be calendar_df
date_df <- data.frame(
  stringsAsFactors = FALSE
  , date = seq(as.Date("2019-01-01"),as.Date("2019-04-30"),by=1)
) %>%
  mutate(month = months(date),
         weekday = factor(
           weekdays(date)
           ,levels = workdays),
         week_of_year = strftime(date,format = "%V"),
         day = substr(date,9,10)) %>%
  left_join(month_df,by=c("month" = "full")) %>%
  mutate(month = factor(month,levels = month.name)) %>%
  select(month,abb,everything()) %>% # reorder columns
  filter(!is.na(weekday)) %>% #remove saturday and sunday
  arrange(weekday) %>%
  select(-date) %>%
  group_by(weekday) %>%
  spread(weekday,day)

# for showing actual dates respective to day of year selected in dt
date_df_full <- data.frame(
  stringsAsFactors = FALSE
  , date = seq(as.Date("2019-01-01"),as.Date("2019-04-30"),by=1)
) %>%
  mutate(month = factor(months(date),levels = month.name),
         weekday = factor(
           weekdays(date)
           ,levels = c("Monday","Tuesday","Wednesday","Thursday","Friday")),
         week_of_year = strftime(date,format = "%V")
         ) %>%
  filter(!is.na(weekday)) %>% #remove saturday and sunday
  arrange(weekday) %>%
  group_by(weekday) %>%
  spread(weekday,date)


#color pallette
v_light_tan <- '#EEEBD8' # or for print... '#EEEBD8'
med_tan <- '#E1D6B8'
ou_tan <- '#EFE8E2'
ou_light_gold <- '#d9c89e' # or for print... '#DFD3B5'
ou_gold <- '#B69A5B' # or for print... '#B59A57'
v_light_gray <- '#a3a3a3'
v_v_light_gray <- '#cdcdcd'
med_gray <- '#696969' #"#808080"
v_dark_gray <- '#323232' #'#252525'
ou_brown <- '#5e504d'
ou_light_brown <- '#776a67'

# styles
ou_line_style <- HTML(paste0("color: ",ou_gold,";background-color: ",ou_gold, ";height:10px;"))

col_format <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#252525', 'color': '#FFFFFF'});",
  "}")

# options
options(shiny.maxRequestSize=1000^3,
        shiny.sanitize.errors = TRUE,
        # shiny.trace = TRUE, # print to R console?
        DT.options = list(
          initComplete = col_format,
          pageLength = 25,
          lengthMenu = c(5,10,20,50,9999),
          bPaginate=TRUE,
          bFilter=FALSE,
          searching=FALSE,
          ordering=FALSE,
          dom = 't'
        ))

enableBookmarking(store = "url")

