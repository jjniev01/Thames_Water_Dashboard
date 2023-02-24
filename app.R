#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalt)
library(shiny)
library(sf)
library(plotly)
library(leaflet)
library(shinydashboard)
#library(reactlog)
library(purrr)
library(DT)
# options(shiny.reactlog=TRUE)
# options(shiny.error = browser)



##  STATIC VARIABLES  ------------------------------------------------------
api_root <- 'https://prod-tw-opendata-app.uk-e1.cloudhub.io/'
##  Discharge alert database endpoint:
data_endpoint <- 'data/STE/v1/DischargeAlerts'
##  Current status endpoint:
current_endpoint <- 'data/STE/v1/DischargeCurrentStatus'
##  API full endpoint addresses:
url_alert <- paste0(api_root, data_endpoint)
url_current <- paste0(api_root,current_endpoint)

os_api_root <- 'https://api.os.uk/downloads/v1'

##  Mapbox Credentials
##    This text file should have a single line containing the MapBox token.
mapboxToken <- paste(readLines("./mapboxtoken.txt",
                               warn = FALSE), 
                     collapse="")

Sys.setenv("MAPBOX_TOKEN" = mapboxToken)


##  TW API Credentials
##    This text file should have two lines. The first line should have your 
##    client ID, and the second line should have the client secret as provided 
##    by Thames Water.
tw_creds <- readLines("./TW_API_credentials.txt", warn = FALSE)
client_id <- paste(tw_creds[[1]], 
                   collapse="")
client_secret <- paste(tw_creds[[2]], 
                       collapse="")

##  OS Downloads Credentials
##    This text file should have two lines. The first line should have your 
##    client ID, and the second line should have the client secret as provided 
##    by OS
# os_creds <- readLines("./OS_API_credentials.txt", warn = FALSE)
# os_client_id <- paste(os_creds[[1]], 
#                    collapse="")
# os_client_secret <- paste(os_creds[[2]], 
#                        collapse="")


##  Define TW query templates (sprintf format):
tw_date_query_temp_start <- 'col_%s=DateTime&operand_%s=gte&value_%s=%s'
tw_date_query_temp_end <- 'col_%s=DateTime&operand_%s=lte&value_%s=%s'


##  Define template for text popups on map
popup_template <- paste0("Name: %s",
                         "</br>Permit Number: %s",
                         "</br>Receiving Watercourse: %s",
                         "</br>Current Status: %s",
                         "</br>Discharge in Last 48Hrs: %s",
                         "</br>Last Status Change: %s",
                         "</br>Length of Last Event: %s hours")

##  Load River Catchements data
riv_cat <- st_read("./Data/Catchments/WFD_River_Water_Body_Catchments_Cycle_3.shp") %>%
    ##  Dissolve by the mgmt name:
    dplyr::group_by(MNCAT_NAME) %>%
    summarise(geometry = sf::st_union(geometry))%>%
    ungroup() %>%
    ##  Convert to WGS84:
    st_transform(crs = 4326) %>%
    ##  Reduce to the columns we need (namely the Management Catchment Name):
    select(MNCAT_NAME)
    
##  Make non sf version and of the centroids:
riv_cat_df <- riv_cat %>%  
    st_centroid() %>%
    mutate(LON = st_coordinates(.)[,1],
           LAT = st_coordinates(.)[,2]) %>%
    st_drop_geometry()


##  Load initial water data to determine current/recent status and transform to 
##  WGS84 for mapbox:
res_current <-  httr::GET(url = url_current, 
                          httr::add_headers(client_id = client_id, 
                                            client_secret = client_secret))

if (httr::status_code(res_current) != 200){
    warning(paste("Request failed with status code: ", httr::status_code(res_current)))
} else {
    ##  Parse the response:
    content <- httr::content(res_current)
    
    ##  Retrieve data and transform to more suitable format 
    tw_df_current <- dplyr::bind_rows(content$items) %>%
        ##  Convert the column types:
        mutate(LocationName = factor(LocationName),
               ReceivingWaterCourse = factor(ReceivingWaterCourse),
               AlertStatus = factor(AlertStatus),
               StatusChange = as.POSIXct(StatusChange,
                                         format="%Y-%m-%dT%H:%M:%S",
                                         tz=Sys.timezone()),
               AlertPast48Hours = as.logical(AlertPast48Hours),
               MostRecentDischargeAlertStart = as.POSIXct(MostRecentDischargeAlertStart,
                                                          format="%Y-%m-%dT%H:%M:%S",
                                                          tz=Sys.timezone()),
               MostRecentDischargeAlertStop =as.POSIXct(MostRecentDischargeAlertStop,
                                                        format="%Y-%m-%dT%H:%M:%S",
                                                        tz=Sys.timezone()))%>%
        ##  Rename the columns:
        rename(GRIDREF = LocationGridRef,
               NAME = LocationName,
               PERMITNUM = PermitNumber,
               RECWATCOURS = ReceivingWaterCourse,
               DISRECENT = AlertPast48Hours,
               ALERTSTAT = AlertStatus,
               LASTCHANG = StatusChange,
               LASTSTART = MostRecentDischargeAlertStart,
               LASTSTOP = MostRecentDischargeAlertStop)%>% 
        ##  Convert to sf object:
        st_as_sf(coords = c("X","Y"), crs = 27700) %>%
        ##  Convert to WGS84:
        st_transform(crs = 4326) %>%
        ##  Retrieve the WGS84 lat lon as numeric columns:
        mutate(LON = st_coordinates(.)[,1],
               LAT = st_coordinates(.)[,2],
               ##  Calculate the length of the last event (in min initially) and
               ##  store as a string:
               LASTLEN = ifelse(is.na({as.numeric(LASTSTOP-LASTSTART)/60}), 
                                "Ongoing",
                                as.character(round(as.numeric(LASTSTOP-LASTSTART)/60,
                                                   digits = 2)))) %>%
        ##  Color code on status:
        mutate(COLOR = case_when(ALERTSTAT == "Discharging" ~ rgb(t(col2rgb("tomato3"))/255),
                                 ALERTSTAT == "Not discharging" ~ "#00b5ee",
                                 DISRECENT == "Discharge in Last 48hrs" ~ "#eeb744",
                                 TRUE ~ rgb(t(col2rgb("black"))/255))) %>%
        ##  Text Descriptive of status:
        mutate(STATLAB = case_when(ALERTSTAT == "Discharging" ~ "Discharging",
                                   ALERTSTAT == "Not discharging" ~ "Not Discharging",
                                   DISRECENT == "Discharge in Last 48hrs" ~ "Discharge in Last 48hrs",
                                   TRUE ~ "Offline")) %>%
        ##  Determine it's river catchment:
        st_join(riv_cat) %>%
        rename(CATCH = MNCAT_NAME) %>%
        mutate(CATCH = factor(CATCH))
}




##  LOCAL TEST DATA  ---  ##
##  
##  This gets you to a point where you would have chosen an end date, made a 
##  query to the api, preprocessed and renamed a bit, and joined the river 
##  catchment data, i.e. the variable 'discharge_data_sf'
##  
##  WARNING: THIS SHOULD ONLY BE UNCOMMENTED FOR USE IN TESTING THE REACTIVE 
##  ITEMS IN THE CONSOLE ONE BY ONE AND NOT  FOR REACTIVE TESTING
tw_df <- 
    readRDS("./Data/TW_Alert_Data_lim150_all_sensors_2023_01_23.RDS") %>% 
    st_as_sf() %>%
    st_transform(crs = 4326) %>%
    mutate(LON = st_coordinates(.)[,1],
           LAT = st_coordinates(.)[,2])%>%
    ##  Spatial join to the river catchments:
    st_join(riv_cat)
discharge_data_sf <- tw_df

startdate <- reactive({paste0({Sys.Date()-30},
                              "T00:00:00")})
enddate <- reactive({paste0(Sys.Date(),
                            "T00:00:00")})

##  ---  ##




# Define UI for application that draws a histogram
ui <- fluidPage(
    ## Application title
    titlePanel("Thames Water \"Discharge To Environment\" Data"),
    ##         See https://stackoverflow.com/questions/73791859/the-icon-that-hides-and-seek-right-sidebar-in-shinydashboardplus-is-not-workin/73853950#73853950
    tabsetPanel(
        ##  Initial Tab: Current Status Map
        tabPanel(
            ##  No Sidebar for this panel  ##
            ##  Tab title text
            title = 'Current Network Status', 
            fluid = TRUE,
            ##  Current and Recent Discharge Status Map
            mainPanel(
                leafletOutput("currentmap")
                ##  mainPanel End ---
            )
            ##  tabPanel End ---
        ),
        ##  Discharge Events Timeline and Frequency Tabs
        tabPanel(
            ##  Tab title text
            title = "Discharge Events Summary",
            fluid = TRUE,
            ##  SIDEBAR LAYOUT:
            sidebarLayout(
                ##  Date Range Selectors
                sidebarPanel(dateRangeInput(inputId = "daterange",
                                            label = "Dates of Interest",
                                            ##  Default to 30 days prior
                                            start = {Sys.Date()-30},
                                            ##  Default to the current date
                                            end = {Sys.Date()},
                                            min = "2022-04-01",
                                            max = Sys.Date(),
                                            autoclose = TRUE),
                             ##  br() element to introduce extra vertical spacing ----
                             br(),
                             ##  River Catchment Selector
                             selectInput(inputId = "catchment",
                                         label = "River Mgmt. Catchment:",
                                         choices = as.list(c("All",
                                                             unique(riv_cat$MNCAT_NAME))),
                                         ##  Default value
                                         selected = "All"
                                         ##  TODO: Can allow for multiple selections, but will need to
                                         ##        make proper handling for creating the API query;
                                         ##        simple first
                             ),
                             ##  TODO: Make this a drill down
                             ##  Sensor Selector
                             selectInput(inputId = "sensor",
                                         label = "Sensor:",
                                         choices = as.list(c("All",
                                                             levels(tw_df_current$NAME))),
                                         ##  Default value
                                         selected = "All"
                                         ##  TODO: Can allow for multiple selections, but will need to
                                         ##        make proper handling for creating the API query;
                                         ##        simple first
                             )
                             ##  sidebarPanel End  ---
                ),
                
                ##  Graphical objects  ---
                mainPanel(
                    ##  TODO:  Populate this with other graphics
                    ##  Graphical objects  ---
                    plotlyOutput("timeline"),
                    plotlyOutput("donut"),
                    plotlyOutput("event_vio")
                    ##  mainPanel End  ---
                )             
                ##  sidebarLayout End  ---
            ),
            ##  TabPanel End ---
        ),
        
        ##  Discharge (Raw-ish) Table Tab
        tabPanel(
            ## Tab Title Text
            title = "Tables",
            fluid = TRUE,
            ##  SIDEBAR LAYOUT:
            sidebarLayout(
                ##  Date Range Selectors
                sidebarPanel(dateRangeInput(inputId = "daterange",
                                            label = "Dates of Interest",
                                            ##  Default to 30 days prior
                                            start = {Sys.Date()-30},
                                            ##  Default to the current date
                                            end = NULL,
                                            min = "2022-04-01",
                                            max = Sys.Date(),
                                            autoclose = TRUE),
                             ##  br() element to introduce extra vertical spacing ----
                             br(),
                             ##  River Catchment Selector
                             selectInput(inputId = "catchment",
                                         label = "River Mgmt. Catchment:",
                                         choices = as.list(c("All",
                                                             levels(riv_cat$MNCAT_NAME))),
                                         ##  Default value
                                         selected = "All"
                                         ##  TODO: Can allow for multiple selections, but will need to
                                         ##        make proper handling for creating the API query;
                                         ##        simple first
                             ),
                             ##  TODO: Make this a drill down
                             ##  Sensor Selector
                             selectInput(inputId = "sensor",
                                         label = "Sensor:",
                                         choices = as.list(c("All",
                                                             levels(tw_df_current$NAME))),
                                         ##  Default value
                                         selected = "All"
                                         ##  TODO: Can allow for multiple selections, but will need to
                                         ##        make proper handling for creating the API query;
                                         ##        simple first
                             )
                             ##  sidebarPanel End  ---
                ),
                
                ##  
                ##  TODO:  POPULATE THIS WITH OTHER TABLE VIEWS
                mainPanel(
                    ##  TODO:  POPULATE THIS WITH OTHER TABLE VIEWS
                    DTOutput("dischargeDT_view")
                    ##  tabPanel End ---
                    ##  mainPanel End  ---
                )
                ##  sidebarLayout End  ---
            )
            ##  tabPanel End ---
        )
        ##  tabsetPanel End ---
    )
    ##  UI End ---
)




##  SERVER  --------------------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ## TODO: LONGTERM - Examine how I can make the below things reactive.
    ## TODO: Calculate summary stats on the entire service area and by drainage basin/watercourse
    ## TODO: Determine the length (in hours) of the last discharge event and rejoin to the main d
    ## TODO: Determine the average time between discharges for each sensor (will 
    ##       need to clean those lasting less than 30min and assume those are sensor errors)
    ## TODO: Determine the number of discharge hours per 1000 operating hours
    ## TODO: Figure out how to transform data into useful format for time plotting
    
    
    ##  USER REACTIVE INPUTS  --------------------------------------------------
    ##  TODO: Finish these statements and carry out a reactive update of data 
    ##  grabbing from the API.
    ##  Observe for changes in the input date range and grab them for formatting
    ##  in the API query and also store the river catchment for the local 
    ##  filtering of data:
    startdate <- reactive({paste0(as.character(input$daterange[1]),
                                  "T00:00:00")})
    enddate <- reactive({paste0(as.character(input$daterange[2]),
                                "T00:00:00")})
    
    curr_catchment <- reactive(input$catchment)
    
    
    
    
    ##  REACTIVE DATA QUERY  ---------------------------------------------------
    ##  Selection of River Catchment for dynamic plotting of historical data:
    ##  Data retrieval:
    discharge_data_sf <- reactive({
        ##  If there is no NA in the end date:
        if(!stringr::str_detect(enddate(),"NA.*")){
            ##    Set query parameters to retrieve the observations up to 48 
            ##    hours from the present time:
            params <- paste("limit=1000&",
                            sprintf(tw_date_query_temp_start, 
                                    "1", "1", "1",
                                    startdate()),
                            sprintf(tw_date_query_temp_end,
                                    "2", "2", "2",
                                    enddate()),
                            sep="&")
            ##  Make Initial API request:
            res <- httr::GET(url = url_alert,
                             httr::add_headers(client_id = client_id,
                                               client_secret = client_secret),
                             query = params) 
            ##  Potential Error Handling 
            if (httr::status_code(res) != 200){
                showModal(modalDialog(paste("Request failed with status code: ", 
                                            httr::status_code(res)),
                                      "API Request Error",
                                      footer = tagList(actionButton("ok","OK"))))
                observeEvent(input$ok, {
                    removeModal()
                })
            }else{
                ##  Retrieve the content
                foo_dis_dat <- res %>%
                    ##  Retrieve content:
                    httr::content() %>%
                    purrr::pluck("items") %>% 
                    dplyr::bind_rows()
                ##  Make 2nd or more request if the original request is equal to
                ##  the limit, i.e. check if there are more records to retrieve
                if (nrow(foo_dis_dat) == 1000) {
                    ##  Set a flag that there is possibly more records
                    maybe_more <- TRUE
                    ##  Set out API query offset parameter
                    off_set <- nrow(foo_dis_dat)
                    
                    while (maybe_more) {
                        ##    Set query parameters to retrieve the observations up to 48 
                        ##    hours from the present time:
                        params <- paste("limit=1000&",
                                        sprintf("offset=%s&", off_set),
                                        sprintf(tw_date_query_temp_start, 
                                                "1", "1", "1",
                                                startdate()),
                                        sprintf(tw_date_query_temp_end,
                                                "2", "2", "2",
                                                enddate()),
                                        sep="&")
                        ##  Make additional API request:
                        foo_res <- httr::GET(url = url_alert,
                                             httr::add_headers(client_id = client_id,
                                                               client_secret = client_secret),
                                             query = params)
                        ##  Check if the response is valid and nonempty
                        ##  Potential Error Handling 
                        if (httr::status_code(foo_res) != 200) {
                            maybe_more <- FALSE
                            showModal(modalDialog(paste("Additional requests failed with status code: ", 
                                                        httr::status_code(foo_res)),
                                                  "API Request Error",
                                                  footer = tagList(actionButton("ok","OK"))))
                            observeEvent(input$ok, {
                                removeModal()
                            })
                        }else{
                            ##  If valid check for non empty response:
                            if (!is.null(httr::content(foo_res) %>% 
                                         purrr::pluck("items"))) {
                                ##  Process the additional data:
                                foo_dis_dat <- foo_dis_dat %>%
                                    rbind({httr::content(foo_res) %>% 
                                            purrr::pluck("items") %>% 
                                            dplyr::bind_rows()})
                                ##  Repeat to see if there are more by changing 
                                ##  the offset:
                                off_set <- nrow(foo_dis_dat)
                            }else{maybe_more <- FALSE}
                        }
                    }
                }
                
                dis_dat_sf <- foo_dis_dat %>%
                    ##  Convert the column types:
                    mutate(LocationName = factor(LocationName,
                                                 levels = sort(unique(LocationName))),
                           ReceivingWaterCourse = factor(ReceivingWaterCourse),
                           AlertType = factor(AlertType),
                           DateTime = as.POSIXct(DateTime,
                                                 format="%Y-%m-%dT%H:%M:%S",
                                                 tz=Sys.timezone()))%>%
                    ##  Rename the columns:
                    rename(GRIDREF = LocationGridRef,
                           NAME = LocationName,
                           PERMITNUM = PermitNumber,
                           RECWATCOURS = ReceivingWaterCourse)%>%
                    ##  Convert to sf object:
                    st_as_sf(coords = c("X","Y"), crs = 27700) %>%
                    ##  Convert to WGS84:
                    st_transform(crs = 4326) %>%
                    ##  Retrieve the WGS84 lat lon as numeric columns:
                    mutate(LON = st_coordinates(.)[,1],
                           LAT = st_coordinates(.)[,2]) %>%
                    ##  Spatial join to the river catchments:
                    st_join(riv_cat)
                
                ##  If there is a subset by river catchments:
                # cat(curr_catchment())
                # print(head(dis_dat))
                # print(curr_catchment() != "All")
                if(curr_catchment() != "All"){
                    dis_dat_sf <- dis_dat_sf %>%
                        ##  Filter by river catchments:
                        dplyr::filter(MNCAT_NAME %in% curr_catchment())
                }
                ##  Return the data:
                return(dis_dat_sf)}
        }
    })
    
    
    
    
    ##  Update the potential sensors to select from based upon the selected 
    ##  catchment which was used to filter discharge_data_sf
    out_var_sensors <- reactive({c( "All", 
                                    {discharge_data_sf() %>% 
                                            mutate(across(where(is.factor),
                                                          as.character)) %>%
                                            pull(NAME) %>%
                                            unique()} )})
    ##    Update the selection list in the UI:
    observe({updateSelectInput(session = session,
                               "sensor",
                               choices = out_var_sensors())})
    
    
    
    
    ##  Presentation, non-spatial version of the discharge data:
    discharge_data_df <- reactive({
        ##  Only calc if the end date is not null
        if (!stringr::str_detect(enddate(),"NA.*")) {
            dis_dat_df <- discharge_data_sf() %>% 
                st_drop_geometry() %>%
                ##  Rename columns for presenting:
                dplyr::rename("Sensor Name" = NAME,
                              "Permit Number" = PERMITNUM,
                              "Grid Reference" = GRIDREF,
                              "Receiving Watercourse" = RECWATCOURS,
                              "Alert Type" = AlertType,
                              "Date and Time" = DateTime,
                              "Longitude" = LON,
                              "Latitude" = LAT,
                              "Mgmt. Catchment Name" = MNCAT_NAME)
        }else{
            dis_dat_df <- NULL
        }
        return(dis_dat_df)
    })
    
    
    
    
    ##  Time df of events for plotting across time:
    discharge_time_df <- reactive({
        ##  If there is no NA in the end date:
        if(!stringr::str_detect(enddate(),"NA.*")){
            ## Create the time data:
            dis_time <- discharge_data_sf() %>%
                ##  Drop geometry:
                st_drop_geometry() %>%
                ##  Subset columns:
                select(NAME, AlertType, DateTime) %>%
                group_by(NAME) %>%
                ##  Arrange in ascending order:
                arrange(DateTime) %>%
                ##  Lead the date and alert:
                mutate(TIMELEAD = lead(DateTime, n = 1),
                       ALERTLEAD = lead(AlertType, n = 1)) %>%
                ungroup() %>%
                ##  Classify the time period:
                mutate(PERIOD_TYPE = paste0(AlertType, 
                                            "-",
                                            ALERTLEAD)) %>%
                ## Remove any unresolved periods, i.e. ending with -NA:
                dplyr::filter(!(stringr::str_detect(PERIOD_TYPE, ".*-NA"))) %>%
                ##  Reclassify the period types for use as how we will 
                ##  display it in the pie/bar chart:
                mutate(PERIOD_CLASS = case_when(PERIOD_TYPE == "Start-Stop" ~ "Discharge",
                                                PERIOD_TYPE == "Offline start-Offline stop" ~ "Offline",
                                                PERIOD_TYPE == "Offline start-Offline start" ~ "Offline",
                                                PERIOD_TYPE == "Offline stop-Offline start" ~ "Regular Operation",
                                                PERIOD_TYPE == "Stop-Offline start" ~ "Regular Operation",
                                                PERIOD_TYPE == "Offline start-Start" ~ "Offline",
                                                ## Left Censor
                                                PERIOD_TYPE == "Offline start-Stop" ~ "Discharge",
                                                ##  Right Censor
                                                PERIOD_TYPE == "Start-Offline start" ~ "Discharge",
                                                ## Unknown offline start
                                                PERIOD_TYPE == "Stop-Offline stop" ~ "SemiRegular Operation", 
                                                TRUE ~ "Regular Operation")) %>%
                ##  Remove the regular periods between stops and starts:
                dplyr::filter(PERIOD_CLASS %in% c("Discharge",
                                                  "Offline",
                                                  "SemiRegular Operation")) %>%
                ##  Rename columns to better reflect what we are representing 
                ##  now:
                rename(PERIOD_START = DateTime,
                       PERIOD_END = TIMELEAD) %>%
                ##  Drop unnecessary columns:
                dplyr::select(NAME, 
                              PERIOD_START,
                              PERIOD_END,
                              PERIOD_TYPE,
                              PERIOD_CLASS) %>%
                mutate(PERIOD_TYPE = factor(PERIOD_TYPE, levels = c("Start-Stop",
                                                                    "Offline start-Offline stop",
                                                                    "Offline start-Offline start",
                                                                    "Offline start-Start",
                                                                    "Offline start-Stop",
                                                                    "Start-Offline start",
                                                                    "Stop-Offline stop")),
                       PERIOD_CLASS = factor(PERIOD_CLASS, levels = c("Discharge",
                                                                      "Offline",
                                                                      "SemiRegular Operation")))
        }
        dis_time
    })
    
    
    
    
    ##  Discharge duration DF:
    discharge_duration_df <- reactive({
        if(!stringr::str_detect(enddate(),"NA.*")){
            dis_dur_df <- discharge_time_df() %>%
                ##  Calculate the duration of each event in hours:
                mutate(DURATION = as.numeric(PERIOD_END - PERIOD_START)/60/60)
        }
        dis_dur_df
    })
    
    
    
    
    ##  Event duration data classified by type:
    sensor_prop_status_df <- reactive({
        if(!stringr::str_detect(enddate(),"NA.*")){
            sensor_prop_df <- discharge_data_sf() %>%
                ##  Drop geometry:
                st_drop_geometry() %>%
                ##  Subset columns:
                select(NAME, AlertType, DateTime) %>%
                group_by(NAME) %>%
                ##  Arrange in ascending order:
                arrange(DateTime) %>%
                ##  Lead the date and alert:
                mutate(TIMELEAD = lead(DateTime, n = 1),
                       ALERTLEAD = lead(AlertType, n = 1)) %>%
                ungroup() %>%
                ##  Classify the time period:
                mutate(PERIOD_TYPE = paste0(AlertType, 
                                            "-",
                                            ALERTLEAD)) %>%
                ## Remove any unresolved periods, i.e. ending with -NA:
                dplyr::filter(!(stringr::str_detect(PERIOD_TYPE, ".*-NA")))
            
            
            ##  Summarise time by period type:
            sensor_prop_summ_df <- sensor_prop_df %>% 
                group_by(NAME) %>%
                ##  Calculate the time of each period in hours:
                mutate(DURATION = as.numeric(TIMELEAD - DateTime)/60/60) %>%
                ungroup() %>%
                group_by(NAME, PERIOD_TYPE) %>%
                summarise(PERIODTIME = sum(DURATION, na.rm = TRUE))
            
            ##  Calculate the total operating hours to normalise our period 
            ##  types:
            tot_operating_hours <- sensor_prop_df %>%
                dplyr::select(NAME, DateTime) %>%
                group_by(NAME) %>%
                ##  Find min and max hours of the sensor data:
                dplyr::summarise(MINTIME = min(DateTime, na.rm = TRUE),
                                 MAXTIME = max(DateTime, na.rm = TRUE)) %>%
                ##  Calculate the total operating time in hours:
                mutate(TOTOPTIME = as.numeric(MAXTIME-MINTIME)/60/60) %>%
                ungroup() %>%
                ##  Drop all but necessary columns:
                dplyr::select(NAME,TOTOPTIME)
                
            ##  Rejoin the tot operating time to the summarised period data and 
            ##  back infer normal operating time:
            sensor_prop_summ_df <- sensor_prop_summ_df %>%
                ##  Remove normal operation periods (we will back infer this 
                ##  later)
                dplyr::filter(!(PERIOD_TYPE %in% 
                                    c("Offline stop-Start",
                                      "Stop-Start"))) %>%
                left_join(tot_operating_hours, by = "NAME") %>%
                ##  If the total operating hours for a given sensor is less than
                ##  168 hours (7 days), we'll assign it a quality flag:
                mutate(QA_FLAG = case_when(TOTOPTIME < 168 ~ "Low Hours in Record",
                                           PERIOD_TYPE == "Offline start-Stop" ~ "Partial Discharge Record - Left Censor",
                                           PERIOD_TYPE == "Start-Offline Start" ~ "Partial Discharge Record - Right Censor",
                                           PERIOD_TYPE == "Stop-Offline stop" ~ "Partial Discharge Record - Unknown Offline Start",
                                           TRUE ~ "None"),
                       ##  Reclassify the period types for use as how we will 
                       ##  display it in the pie/bar chart:
                       PERIOD_CLASS = case_when(PERIOD_TYPE == "Start-Stop" ~ "Discharge",
                                                PERIOD_TYPE == "Offline start-Offline stop" ~ "Offline",
                                                PERIOD_TYPE == "Offline start-Offline start" ~ "Offline",
                                                PERIOD_TYPE == "Offline stop-Offline start" ~ "Regular Operation",
                                                PERIOD_TYPE == "Stop-Offline start" ~ "Regular Operation",
                                                PERIOD_TYPE == "Offline start-Start" ~ "Offline",
                                                ## Left Censor
                                                PERIOD_TYPE == "Offline start-Stop" ~ "Discharge",
                                                ##  Right Censor
                                                PERIOD_TYPE == "Start-Offline start" ~ "Discharge",
                                                ## Unknown offline start
                                                PERIOD_TYPE == "Stop-Offline stop" ~ "SemiRegular Operation", 
                                                TRUE ~ "Regular Operation")) %>%
                ##  Create weights based upon the QA flag for the operating 
                ##  time:
                mutate(WEIGHT = case_when(QA_FLAG == "Partial Discharge Record - Left Censor" ~ 0.5,
                                          QA_FLAG == "Partial Discharge Record - Right Censor" ~ 0.5,
                                          QA_FLAG == "Partial Discharge Record - Unknown Offline Start" ~ 0.5,
                                              ##  Note this 0.25 means that 0.25
                                              ##  of this period will be 
                                              ##  counted as non-regular 
                                              ##  operation:
                                              QA_FLAG == "SemiRegular Operation" ~ 0.25,
                                              TRUE ~ 1))
            ##  Return it   
            sensor_prop_summ_df
        }
    })
    
    
    ##  Sensor event total duration by type (useful for pie/bar charts):
    sensor_hour_df <- reactive({
        ##  TODO: Put in a reactive condition for some sensor having been 
        ##        selected in order to show a drill down pie chart
        ##      
        # if(!stringr::str_detect(enddate(),"NA.*") & ){
        #     
        # }
        ##  Default pie chart of all selected sensors combined:
        if(!stringr::str_detect(enddate(),"NA.*")){
        ##  Calculate sum of non regular hours by sensor:
            nonreg_hours <- sensor_prop_status_df() %>%
                ##  Filtering out stations with low records:
                dplyr::filter(QA_FLAG != "Low Hours in Record") %>%
                ##  Calculate weighted time duration:
                mutate(WEIGHTTIME = WEIGHT * PERIODTIME) %>%
                ##  Thin down our dataframe prior to pivoting:
                dplyr::select(NAME,
                              PERIOD_CLASS, 
                              WEIGHTTIME)
            
            ##  Infer the ESTIMATED regular hours from the nonreg hours and known regular hours
            est_reg_hours <- nonreg_hours %>%
                group_by(NAME) %>%
                summarise(WEIGHTTIME = sum(WEIGHTTIME, na.rm = TRUE)) %>%
                ##  Join the total hours back to this:
                left_join(tot_operating_hours, by = "NAME") %>%
                ##  calculate estimated regular hours
                mutate(TIME = TOTOPTIME - WEIGHTTIME,
                       PERIOD_CLASS = "Est. Regular Operation") %>%
                ##  Drop columns and rename to facilitate tidy rejoining:
                dplyr::select(NAME, PERIOD_CLASS, TIME) %>%
                rename(WEIGHTTIME = TIME)
            
            nonreg_hours <- nonreg_hours %>%
                ##  Add the est regular hours to the main data
                rbind(est_reg_hours) %>%
                ##  Rename one last time:
                rename(OPTYPE = PERIOD_CLASS,
                       TIME = WEIGHTTIME) %>%
                ##  Make sure OPTYPE is a factor for proper level color coding:
                mutate(OPTYPE = factor(OPTYPE, 
                                       levels = c("Offline",
                                                  "Discharge",
                                                  "Regular Operation",
                                                  "SemiRegular Operation",
                                                  "Est. Regular Operation")),
                       ##  Hard classify the coloring bc plot_ly be that way:
                       HEX = case_when(OPTYPE == "Offline" ~ "#707070",
                                       OPTYPE == "Discharge" ~ {rgb(t(col2rgb("tomato3"))/255)},
                                       OPTYPE == "Regular Operation" ~ "#58b6d2",
                                       OPTYPE == "SemiRegular Operation" ~ "#004b6c",
                                       OPTYPE == "Est. Regular Operation" ~ "#00b5ee"))
        }
        nonreg_hours
    })
    
    
    
    
    
    ##  CURRENT STATUS MAP WIDGET (NON-REACTIVE)  ----
    output$currentmap <- renderLeaflet({
        ##  Create a map that plots the most recent record for each sensor and
        ##  assigns color coding for the status:
        leaflet() %>%
            ##  Add basemaps tiles
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            ##  Add the river catchments:
            addPolygons(data = riv_cat,
                        stroke = TRUE,
                        opacity = 0.6,
                        ##  Stroke width in pixels
                        weight = 1,
                        fill = FALSE,
                        color = rgb(t(col2rgb("grey20"))/255),
                        dashArray = "6",
                        label = ~sprintf("%s",
                                         MNCAT_NAME)) %>%
            addLabelOnlyMarkers(data = riv_cat_df,
                                lng = ~LON,
                                lat = ~LAT,
                                label = ~MNCAT_NAME,
                                labelOptions = labelOptions(noHide = TRUE,
                                                            direction = 'top',
                                                            textOnly = TRUE,
                                                            opacity = 0.6,
                                                            textsize = "8pt")) %>%
            ##  Add the sensor location with current status and popup info:
            addCircleMarkers(data = tw_df_current,
                             fillColor = ~COLOR,
                             radius = 5,
                             stroke = FALSE,
                             fillOpacity = 0.8,
                             popup = ~sprintf(popup_template,
                                              NAME,
                                              PERMITNUM,
                                              RECWATCOURS,
                                              ALERTSTAT,
                                              DISRECENT,
                                              LASTCHANG,
                                              LASTLEN)) %>%
            addLegend(position = "bottomright",
                      colors = c(rgb(t(col2rgb("tomato3"))/255),
                                 "#eeb744",
                                 "#00b5ee",
                                 rgb(t(col2rgb("grey30"))/255)),
                      labels = c("Discharging",
                                 "Discharge in Last 48 hours",
                                 "Not Discharging",
                                 "Offline"),
                      title = "Current Status",
                      opacity = 1.0) %>%
            ##  Set the initial view:
            setView(lng = mean(tw_df_current$LON, na.rm = TRUE),
                    lat = mean(tw_df_current$LAT, na.rm = TRUE),
                    zoom = 7)
    })
    
    
    
    
    ##  DATA QUERY TABLE DISPLAY  ----
    ##  Raw-ish discharge data
    output$dischargeDT_view <- renderDT(discharge_data_df(),
                                        rownames = FALSE)
    
    
    
    
    ##  EVENT TIME PLOT (DUMBBELL)
    output$timeline <- renderPlotly({
        plot_ly(discharge_time_df(),
                color = ~PERIOD_CLASS,
                colors = c("tomato3","#707070","#eeb744")) %>%
            add_segments(x = ~PERIOD_START,
                         xend = ~PERIOD_END,
                         y = ~NAME,
                         yend = ~NAME) %>%
            layout(title = list(text = "<b>Sensor Event Timeline</b>"),
                   yaxis = list(title = "Sensor Name",
                                autorange = "reversed"),
                   xaxis = list(title = "Time"),
                   legend = list(title = list(text = "<b>Non-Regular<br>Event Type</b>")),
                   margin = list(l = 65))
    })
    
    
    ##  OPERATIONAL HOURS PIE(DONUT) CHART
    output$donut <- renderPlotly({plot_ly(sensor_hour_df(),
                                          labels = ~OPTYPE,
                                          values = ~TIME,
                                          marker = list(colors = ~HEX,
                                                        line = list(color = '#FFFFFF',
                                                                    width = 1)),
                                          insidetextorientation='radial',
                                          rotation = 90) %>%
            add_pie(hole = 0.6) %>%
            layout(title = "Operational Hours by Sensor Status",  
                   showlegend = TRUE,
                   xaxis = list(showgrid = FALSE, 
                                zeroline = FALSE, 
                                showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, 
                                zeroline = FALSE, 
                                showticklabels = FALSE),
                   margin = list(l = 50, r = 50,
                                 b = 50, t = 1,
                                 pad = 20))
    })
        
    
    ##  VIOLINPLOT (RIDGEPLOT) OF EVENT DURATIONS
    output$event_vio <- renderPlotly({
        foo_vio_1 <- plot_ly(type = "violin",
                             x = filter(sensor_hour_df(),
                                        OPTYPE == "Offline") %>% pull(TIME),
                             color = I("#707070"),
                             side = "positive",
                             box = list(visible = TRUE),
                             meanline = list(visible = TRUE))%>%
            layout(xaxis = list(title = "Event Duration (Hours)"),
                   yaxis = list(title = "Frequency"),
                   domain = list(x = c(0,1), y = c(0.8,1.0)))
        foo_vio_2 <- plot_ly(type = "violin",
                             x = filter(sensor_hour_df(),
                                        OPTYPE == "Discharge") %>% pull(TIME),
                             color = I({rgb(t(col2rgb("tomato3"))/255)}),
                             side = "positive",
                             box = list(visible = TRUE),
                             meanline = list(visible = TRUE))%>%
            layout(xaxis = list(title = "Event Duration (Hours)"),
                   yaxis = list(title = "Frequency"),
                   domain = list(x = c(0,1), y = c(0.6,0.8)))
        foo_vio_3 <- plot_ly(type = "violin",
                             x = filter(sensor_hour_df(),
                                        OPTYPE == "Regular Operation") %>% pull(TIME),
                             color = I("#58b6d2"),
                             side = "positive",
                             box = list(visible = TRUE),
                             meanline = list(visible = TRUE))%>%
            layout(xaxis = list(title = "Event Duration (Hours)"),
                   yaxis = list(title = "Frequency"),
                   domain = list(x = c(0,1), y = c(0.4,0.6)))
        foo_vio_4 <- plot_ly(type = "violin",
                             x = filter(sensor_hour_df(),
                                        OPTYPE == "SemiRegular Operation") %>% pull(TIME),
                             color = I("#004b6c"),
                             side = "positive",
                             box = list(visible = TRUE),
                             meanline = list(visible = TRUE)) %>%
            layout(xaxis = list(title = "Event Duration (Hours)"),
                   yaxis = list(title = "Frequency"),
                   domain = list(x = c(0,1), y = c(0.2,0.4)))
        foo_vio_5 <- plot_ly(type = "violin",
                             x = filter(sensor_hour_df(),
                                        OPTYPE == "Est. Regular Operation") %>% pull(TIME),
                             color = I("#00b5ee"),
                             side = "positive",
                             box = list(visible = TRUE),
                             meanline = list(visible = TRUE)) %>%
            layout(xaxis = list(title = "Event Duration (Hours)"),
                   yaxis = list(title = "Frequency"),
                   domain = list(x = c(0,1), y = c(0.0,0.2)))
        
        ##  Create a 1x5 (rc) series of subplots
        subplot(list(foo_vio_1, foo_vio_2,
                     foo_vio_3, foo_vio_4,
                     foo_vio_5),
                nrows = 5, 
                titleX = TRUE,
                titleY = TRUE,
                margin = 0.1,
                heights = c(0.2, 
                            0.2,
                            0.2,
                            0.2,
                            0.2)) %>%
            ##  Update subplot titles locations and such
            layout(annotations = list( 
                list(x = 0.2,  
                     y = 1.0,  
                     text = "Offline",  
                     xref = "paper",  
                     yref = "paper",  
                     xanchor = "center",  
                     yanchor = "bottom",  
                     showarrow = FALSE),  
                list(x = 0.2,  
                     y = 1.0,  
                     text = "Discharge",  
                     xref = "paper",  
                     yref = "paper",  
                     xanchor = "center",  
                     yanchor = "bottom",  
                     showarrow = FALSE),  
                list(x = 0.2,  
                     y = 1.0,  
                     text = "Regular Operations",  
                     xref = "paper",  
                     yref = "paper",  
                     xanchor = "center",  
                     yanchor = "bottom",  
                     showarrow = FALSE),  
                list(x = 0.2,  
                     y = 1.0,  
                     text = "SemiRegular Operation",  
                     xref = "paper",  
                     yref = "paper",  
                     xanchor = "center",  
                     yanchor = "bottom",  
                     showarrow = FALSE),  
                list(x = 0.2,  
                     y = 1.0,  
                     text = "Est. Regular Operation",  
                     xref = "paper",  
                     yref = "paper",  
                     xanchor = "center",  
                     yanchor = "bottom",  
                     showarrow = FALSE)
                ))
    })
    
    
    
    ##  TEST SELECTION RESULTS
    output$test <- renderText({
        sprintf("StartDate: %s\nEndDate: %s\nCatchment: %s",
                startdate(),
                enddate(),
                curr_catchment())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
