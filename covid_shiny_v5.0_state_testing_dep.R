### Load necessary packages ###
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(jsonlite)
### Load theme ###
#Staturdays Colors
staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)
staturdays_colors <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (staturdays_col_list)
  staturdays_col_list[cols]
}
staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15)
)

# User two action buttons and reactiveValues to let users select data source between cases and deaths

### Load in Data ###

# Johns Hopkins Case and Death Timeseries by County -----------------------

data_source <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv" 

data_csv <- read.csv(data_source, header = TRUE, sep = ",") # Read in csv file from the web
data_csv <- data_csv %>% pivot_longer(cols = -c(1:11), names_to = "Date") # Move Dates to one column
data_csv <- data_csv %>%  mutate(Date = str_remove(data_csv$Date, "X")) # Remove the X from the date
data_csv <- data_csv %>% mutate(date = mdy(data_csv$Date)) # Convert to actual date values
data_csv <- data_csv %>% select(-Date) # Get rid of unnecessary Date column
covid_data_cases <- data_csv
covid_data_cases <- covid_data_cases %>% 
  mutate(New_Cases = 
           case_when(
             lag(Combined_Key, n = 1L) == Combined_Key ~ value - lag(value, 1L),
             TRUE ~ as.integer(0)
             )
         )
covid_data_cases <- covid_data_cases %>% rename(Cumulative_Cases = value)

data_source_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv" 

data_csv_deaths <- read.csv(data_source_deaths, header = TRUE, sep = ",") # Read in csv file from the web
data_csv_deaths <- data_csv_deaths %>% pivot_longer(cols = -c(1:12), names_to = "Date") # Move Dates to one column
data_csv_deaths <- data_csv_deaths %>%  mutate(Date = str_remove(data_csv_deaths$Date, "X")) # Remove the X from the date
data_csv_deaths <- data_csv_deaths %>% mutate(date = mdy(data_csv_deaths$Date)) # Convert to actual date values
data_csv_deaths <- data_csv_deaths %>% select(-Date) # Get rid of unnecessary Date column
covid_data_deaths <- data_csv_deaths %>% 
  mutate(New_Deaths = 
           case_when(
             lag(Combined_Key, n = 1L) == Combined_Key ~ value - lag(value, 1L),
             TRUE ~ as.integer(0)
             )
         )
covid_data_deaths <- covid_data_deaths %>% rename(Cumulative_Deaths = value)

covid_data <- left_join(covid_data_cases, covid_data_deaths, by = c("Province_State", "Combined_Key", "date"))


# State and US Summaries --------------------------------------------------

covid_data_state <- covid_data %>% 
  group_by(Province_State, date) %>% 
  summarise(New_Cases = sum(New_Cases), New_Deaths = sum(New_Deaths), Cumulative_Cases = sum(Cumulative_Cases), Cumulative_Deaths = sum(Cumulative_Deaths), Population = sum(Population))

covid_data_us <- covid_data_state %>% 
  group_by(date) %>% 
  summarise(New_Cases = sum(New_Cases), New_Deaths = sum(New_Deaths), Cumulative_Cases = sum(Cumulative_Cases), Cumulative_Deaths = sum(Cumulative_Deaths), Population = sum(Population))


# Global Data -------------------------------------------------------------

#Global Confirmed Cases
data_source_global <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" 

data_csv <- read.csv(data_source_global, header = TRUE, sep = ",") # Read in csv file from the web
data_csv <- data_csv %>% pivot_longer(cols = -c(1:4), names_to = "Date") # Move Dates to one column
data_csv <- data_csv %>%  mutate(Date = str_remove(data_csv$Date, "X")) # Remove the X from the date
data_csv <- data_csv %>% mutate(date = mdy(data_csv$Date)) # Convert to actual date values
data_csv <- data_csv %>% select(-Date) # Get rid of unnecessary Date column
covid_data_cases <- data_csv
covid_data_cases <- covid_data_cases %>% 
  group_by(Country.Region, date) %>% 
  summarise(Cumulative_Cases = as.integer(sum(value)))
covid_data_cases <- covid_data_cases %>% 
  mutate(New_Cases = 
           case_when(
             lag(Country.Region, n = 1L) == Country.Region ~ Cumulative_Cases - lag(Cumulative_Cases, 1L),
             TRUE ~ as.integer(0)
           )
  )

data_source_global_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" 

data_csv <- read.csv(data_source_global_deaths, header = TRUE, sep = ",") # Read in csv file from the web
data_csv <- data_csv %>% pivot_longer(cols = -c(1:4), names_to = "Date") # Move Dates to one column
data_csv <- data_csv %>%  mutate(Date = str_remove(data_csv$Date, "X")) # Remove the X from the date
data_csv <- data_csv %>% mutate(date = mdy(data_csv$Date)) # Convert to actual date values
data_csv <- data_csv %>% select(-Date) # Get rid of unnecessary Date column
covid_data_deaths <- data_csv
covid_data_deaths <- covid_data_deaths %>% 
  group_by(Country.Region, date) %>% 
  summarise(Cumulative_Deaths = as.integer(sum(value)))
covid_data_deaths <- covid_data_deaths %>% 
  mutate(New_Deaths = 
           case_when(
             lag(Country.Region, n = 1L) == Country.Region ~ Cumulative_Deaths - lag(Cumulative_Deaths, 1L),
             TRUE ~ as.integer(0)
           )
  )

covid_data_global <- left_join(covid_data_cases, covid_data_deaths)

data_source_global_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv" 

data_csv <- read.csv(data_source_global_recovered, header = TRUE, sep = ",") # Read in csv file from the web
data_csv <- data_csv %>% pivot_longer(cols = -c(1:4), names_to = "Date") # Move Dates to one column
data_csv <- data_csv %>%  mutate(Date = str_remove(data_csv$Date, "X")) # Remove the X from the date
data_csv <- data_csv %>% mutate(date = mdy(data_csv$Date)) # Convert to actual date values
data_csv <- data_csv %>% select(-Date) # Get rid of unnecessary Date column
covid_data_recovered <- data_csv
covid_data_recovered <- covid_data_recovered %>% 
  group_by(Country.Region, date) %>% 
  summarise(Cumulative_Recovered = as.integer(sum(value)))
covid_data_recovered <- covid_data_recovered %>% 
  mutate(New_Recovered = 
           case_when(
             lag(Country.Region, n = 1L) == Country.Region ~ Cumulative_Recovered - lag(Cumulative_Recovered, 1L),
             TRUE ~ as.integer(0)
           )
  )

covid_data_global <- left_join(covid_data_global, covid_data_recovered)

covid_data_global_longer <- covid_data_global %>% 
  pivot_longer(cols = c(Cumulative_Deaths, Cumulative_Recovered, New_Deaths, New_Recovered), names_to = "Case_Outcome", values_to = "Count")

# Calculate Recovery Rate
covid_data_global_recoveries <- covid_data_global %>% 
  group_by(Country.Region) %>% 
  summarise(Total_Deaths = max(Cumulative_Deaths), Total_Recovered = max(Cumulative_Recovered), Recovery_Rate = Total_Recovered/(Total_Deaths+Total_Recovered))

# Testing Data Source ---------------------------------------------------------

covid_state_daily_source <- "https://covidtracking.com/api/v1/states/daily.json"
covid_state_daily <- jsonlite::fromJSON(covid_state_daily_source)
#update date
covid_state_daily <- covid_state_daily %>% mutate(date = ymd(date))
#update state names
state_names <- as.data.frame(cbind(state.abb,state.name))

# #Fix non-US states AS, GU, etc.
# covid_state_daily %>% 
#   filter(is.na(state.name) == T) %>% 
#   count(state)

state_names <- state_names %>% rbind(tribble(
  ~ state.abb, ~ state.name,
  "AS", "American Samoa",
  "DC", "District of Columbia",
  "GU", "Guam",
  "MP", "Northern Mariana Islands",
  "PR", "Puerto Rico",
  "VI", "Virgin Islands"
)
)
covid_state_daily <- left_join(covid_state_daily, state_names, by = c("state" = "state.abb"))

# Hospital Data (Beds and ICUs in use by Date and State)
covid_state_daily_longer <- covid_state_daily %>% 
  pivot_longer(cols = c(hospitalizedCurrently, inIcuCurrently, onVentilatorCurrently), 
               names_to = ("current_hospitalization_severity"))

# Join in population data to show % of pop tested
state_populations <- select(covid_data_state, c(Province_State, Population)) %>% 
  distinct()
covid_state_daily_pop <- left_join(covid_state_daily, state_populations, by = c("state.name" = "Province_State"))

# Total Tests Done by Each State
temp_rank <- covid_state_daily_pop %>% 
  filter(date == max(date)) %>% 
  group_by(state.name) %>% 
  summarise(percent_tests_pos = positive / posNeg, 
            percent_pop_tested = posNeg / Population, 
            percent_pop_positive = positive / Population, 
            positive, 
            negative, 
            total_tests = posNeg) %>% 
  arrange(desc(percent_pop_positive))

covid_state_daily_rank <- temp_rank %>% mutate(percent_tests_pos_rank = row_number(desc(percent_tests_pos)), 
                                               total_positive_rank = row_number(desc(positive)), 
                                               total_testing_rank = row_number(desc(total_tests))) %>% 
  arrange(percent_tests_pos_rank)

# Start Shiny App ---------------------------------------------------------


# UI ----------------------------------------------------------------------

ui <- navbarPage(title = "COVID-19 Case Tracker",
                 tabPanel("County",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput(inputId = "casetype", label = "Select Cases to Display", choices = c("New Cases", "Total Cases")), 
                                selectizeInput(inputId = "county", label = "Select a county", 
                                               choices = unique(covid_data$Combined_Key), 
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 3, placeholder = "Select up to 3 counties")
                                ),
                                tags$h6("Optionally, you can type the state name to filter the list down to just the counties in that state.", style = "margin-bottom:30px; color:#545454"),
                                dateRangeInput(inputId = "daterange", label = "Select Date Range", start = today()-60, end = max(covid_data$date), min = "2020-01-15"),
                                plotOutput(outputId = "top10_counties")
                              ),
                              mainPanel(
                                plotOutput(outputId = "cases"),
                                plotOutput(outputId = "deaths"),
                                tags$p("A shiny app by ", 
                                       tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                       " - ", 
                                       tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank")),
                                tags$p("Data - ", 
                                       tags$a("Johns Hopkins CSSE" , href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", target="_blank"))
                              )
                            )
                          )
                          ),
                 navbarMenu("State",
                   tabPanel("Cases and Deaths",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput(inputId = "casetype_state", label = "Select Cases to Display", choices = c("New Cases", "Total Cases")), 
                                selectizeInput(inputId = "state", label = "Select a state", 
                                               choices = unique(covid_data$Province_State), 
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 3, placeholder = "Select up to 3 states")
                                ),
                                dateRangeInput(inputId = "daterange_state", label = "Select Date Range", start = today()-60, end = max(covid_data$date), min = "2020-01-15"),
                                plotOutput(outputId = "top10_states")
                              ),
                              mainPanel(
                                plotOutput(outputId = "cases_state"),
                                plotOutput(outputId = "deaths_state"),
                                tags$p("A shiny app by ", 
                                       tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                       " - ", 
                                       tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank")),
                                tags$p("Data - ", 
                                       tags$a("Johns Hopkins CSSE" , href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", target="_blank"))
                              )
                            )
                          )
                 ),
                 tabPanel("Testing and Hospitalizations",
                          fluidPage(
                                dateRangeInput(inputId = "daterange_testing", label = "Select Date Range", start = today()-60, end = max(covid_state_daily$date), min = "2020-01-15"),
                                selectizeInput(inputId = "testing_state", label = "Select a state", 
                                               choices = unique(covid_state_daily$state.name), 
                                               selected = NULL,
                                               multiple = FALSE,
                                               options = list(maxItems = 1, placeholder = "Select a state")
                                ),
                                plotOutput(outputId = "hospitalizations_state"),
                                tags$p("Data - ", 
                                       tags$a("COVID Tracking Project" , href="https://covidtracking.com/api", target="_blank")),
                                plotOutput(outputId = "testing_state"),
                                tags$p("Data - ", 
                                       tags$a("COVID Tracking Project" , href="https://covidtracking.com/api", target="_blank")),
                                selectizeInput(inputId = "pos_test_input_state", label = "Select a state", 
                                               choices = unique(covid_state_daily$state.name), 
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 3, placeholder = "Select up to 3 states")
                                ),
                                plotOutput(outputId = "pos_tests_state"),
                                tags$p("Data - ", 
                                       tags$a("COVID Tracking Project" , href="https://covidtracking.com/api", target="_blank")),
                                tags$hr(),
                                tags$h2("Detailed Testing Data by State"),
                                dataTableOutput(outputId = "total_testing_dt_state"),
                                tags$p("Data - ", 
                                       tags$a("COVID Tracking Project" , href="https://covidtracking.com/api", target="_blank")),
                                tags$p("A shiny app by ", 
                                       tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                       " - ", 
                                       tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank"))
                              )
                            )
                 ),
                 tabPanel("US",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput(inputId = "casetype_US", label = "Select Cases to Display", choices = c("New Cases", "Total Cases")), 
                                dateRangeInput(inputId = "daterange_US", label = "Select Date Range", start = today()-60, end = max(covid_data$date), min = "2020-01-15")
                              ),
                              mainPanel(
                                plotOutput(outputId = "cases_US"),
                                plotOutput(outputId = "deaths_US"),
                                tags$p("A shiny app by ", 
                                       tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                       " - ", 
                                       tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank")),
                                tags$p("Data - ", 
                                       tags$a("Johns Hopkins CSSE" , href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", target="_blank"))
                              )
                            )
                          )
                 ),
                 tabPanel("Global",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                selectizeInput(inputId = "casetype_global", label = "Select Cases to Display", choices = c("New Cases", "Total Cases")), 
                                selectizeInput(inputId = "country", label = "Select a country", 
                                               choices = unique(covid_data_global$Country.Region), 
                                               selected = NULL,
                                               multiple = TRUE,
                                               options = list(maxItems = 3, placeholder = "Select up to 3 countries")
                                ),
                                dateRangeInput(inputId = "daterange_global", label = "Select Date Range", start = today()-60, end = max(covid_data_global$date), min = "2020-01-15")
                              ),
                              mainPanel(
                                plotOutput(outputId = "cases_global"),
                                plotOutput(outputId = "deaths_global"),
                                tags$hr(),
                                tags$h2("Recovery Rate by Country"),
                                dataTableOutput(outputId = "recovery_rate_global"),
                                tags$p("A shiny app by ", 
                                       tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                       " - ", 
                                       tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank")),
                                tags$p("Data - ", 
                                       tags$a("Johns Hopkins CSSE" , href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", target="_blank"))
                              )
                            )
                          )
                 )
                 )


# Server ------------------------------------------------------------------

server <- function(input, output) {

# County Output -----------------------------------------------------------

  output$top10_counties <- renderPlot(
    {
    covid_data %>% 
      filter(date == max(covid_data$date)) %>% 
      top_n(10, New_Cases) %>%
      ggplot() +
      geom_col(aes(x = Combined_Key, y = New_Cases), fill = staturdays_colors("orange")) +
      labs(title = "Counties with Most\nNew Cases Today",
           subtitle = paste0("Data as of ", format.Date(max(covid_data$date), "%B %d, %Y")),
           x = "County",
           y = "Number of Cases",
           caption = "@kylebeni012 | @staturdays") +
      staturdays_theme +
      theme(plot.title = element_text(color = staturdays_colors("dark_blue"), size = 15, face = "bold"),
            plot.subtitle = element_text(size = 10)) +
      scale_y_continuous(labels = comma) +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_discrete(labels = function(x) str_remove(x, ", US"))
    }
  )
  
  output$cases <- renderPlot(
    {
    covid_data %>%
      filter(Combined_Key %in% c(input$county), date >= input$daterange[1] & date <= input$daterange[2]) %>%
      ggplot() + {
      if(input$casetype == "New Cases")
        geom_line(aes(y = New_Cases, x = date, colour = Combined_Key), na.rm = T)} + {
          if(input$casetype == "Total Cases")
            geom_line(aes(y = Cumulative_Cases, x = date, colour = Combined_Key), na.rm = T)} +
      labs(title = input$casetype,
           subtitle = paste0("Data as of ", format.Date(max(covid_data$date), "%B %d, %Y")),
           x = "Date",
           y = "Number of Cases",
           color = "County",
           caption = "@kylebeni012 | @staturdays") +
      staturdays_theme +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
      expand_limits(y = 0) +
      scale_y_continuous(labels = comma)
  }
  )
  
  output$deaths <- renderPlot(
    {
      covid_data %>%
        filter(Combined_Key %in% c(input$county), date >= input$daterange[1] & date <= input$daterange[2]) %>%
        ggplot() + {
          if(input$casetype == "New Cases")
            geom_line(aes(y = New_Deaths, x = date, colour = Combined_Key), na.rm = T)} + {
              if(input$casetype == "Total Cases")
                geom_line(aes(y = Cumulative_Deaths, x = date, colour = Combined_Key), na.rm = T)} +
        labs(title = paste0(str_remove(input$casetype, " Cases"), " Deaths"),
             subtitle = paste0("Data as of ", format.Date(max(covid_data$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Deaths",
             color = "County",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )

# State Cases Output ------------------------------------------------------

  output$top10_states <- renderPlot(
    {
      covid_data_state %>% 
        ungroup() %>% 
        filter(date == max(date)) %>% 
        slice_max(n = 10, order_by = New_Cases) %>%
        ggplot() +
        geom_col(aes(x = Province_State, y = New_Cases), fill = staturdays_colors("orange")) +
        labs(title = "States with Most\nNew Cases Today",
             subtitle = paste0("Data as of ", format.Date(max(covid_data_state$date), "%B %d, %Y")),
             x = "State",
             y = "Number of Cases",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(plot.title = element_text(color = staturdays_colors("dark_blue"), size = 15, face = "bold"),
              plot.subtitle = element_text(size = 10)) +
        scale_y_continuous(labels = comma) +
        theme(axis.text.x = element_text(angle = 90))
      }
  )
  
  output$cases_state <- renderPlot(
    {
      covid_data_state %>%
        filter(Province_State %in% c(input$state), date >= input$daterange_state[1] & date <= input$daterange_state[2]) %>%
        ggplot() + {
          if(input$casetype_state == "New Cases")
            geom_line(aes(y = New_Cases, x = date, colour = Province_State), na.rm = T)} + {
              if(input$casetype_state == "Total Cases")
                geom_line(aes(y = Cumulative_Cases, x = date, colour = Province_State), na.rm = T)} +
        labs(title = input$casetype_state,
             subtitle = paste0("Data as of ", format.Date(max(covid_data_state$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Cases",
             color = "State",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )
  
  output$deaths_state <- renderPlot(
    {
      covid_data_state %>%
        filter(Province_State %in% c(input$state), date >= input$daterange_state[1] & date <= input$daterange_state[2]) %>%
        ggplot() + {
          if(input$casetype_state == "New Cases")
            geom_line(aes(y = New_Deaths, x = date, colour = Province_State), na.rm = T)} + {
              if(input$casetype_state == "Total Cases")
                geom_line(aes(y = Cumulative_Deaths, x = date, colour = Province_State), na.rm = T)} +
        labs(title = paste0(str_remove(input$casetype_state, " Cases"), " Deaths"),
             subtitle = paste0("Data as of ", format.Date(max(covid_data_state$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Deaths",
             color = "State",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )

# State Testing Output ----------------------------------------------------

  output$hospitalizations_state <- renderPlot(
    {
      covid_state_daily_longer %>%
        filter(state.name %in% c(input$testing_state), date >= input$daterange_testing[1] & date <= input$daterange_testing[2]) %>% 
        ggplot() +
        geom_line(aes(y = value, x = date, color = current_hospitalization_severity), na.rm = T) +
        labs(title = "Current Hospitalization Data",
             subtitle = paste0("Data as of ", format.Date(max(covid_state_daily_longer$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Patients",
             color = "Severity Level",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma) +
        scale_color_viridis_d(labels = c("Hospitalized", "In ICU", "On Ventilator"))
    }
    )
  
  output$testing_state <- renderPlot(
    {
     if(input$testing_state == ""){
       return(covid_state_daily %>% 
                ggplot() +
                labs(title = "Current Testing Data",
                     subtitle = paste0("Data as of ", format.Date(max(covid_state_daily$date), "%B %d, %Y")),
                     x = "Date",
                     y = "Number of Tests",
                     color = "Test Outcome",
                     caption = "@kylebeni012 | @staturdays") +
                staturdays_theme +
                theme(legend.position = "bottom") +
                guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
                expand_limits(y = 0) +
                scale_y_continuous(labels = comma) +
                scale_color_viridis_d(labels = c("Negative", "Positive"), option = "C", begin = .3, end = .7))
     } else {
      covid_state_daily %>%
        filter(state.name %in% c(input$testing_state), date >= input$daterange_testing[1] & date <= input$daterange_testing[2]) %>% 
        ggplot() +
        geom_line(aes(y = positiveIncrease, x = date, color = "Positive"), na.rm = T) +
        geom_line(aes(y = negativeIncrease, x = date, color = "Negative"), na.rm = T) +
        labs(title = "Current Testing Data",
             subtitle = paste0("Data as of ", format.Date(max(covid_state_daily$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Tests",
             color = "Test Outcome",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma) +
        scale_color_viridis_d(labels = c("Negative", "Positive"), option = "C", begin = .3, end = .7) #First label is getting applied via alphabetical order of colors above
     }
    }
    )
  
  output$pos_tests_state <- renderPlot(
    {
      covid_state_daily %>% 
        group_by(state.name, date) %>% 
        summarise(percent_positive_test = positiveIncrease/(sum(positiveIncrease, negativeIncrease))) %>% 
        filter(state.name %in% c(input$pos_test_input_state), date >= input$daterange_testing[1] & date <= input$daterange_testing[2]) %>% 
        ggplot() +
        geom_smooth(aes(y = percent_positive_test, x = date, colour = state.name), na.rm = T) +
        labs(title = "Positive Test Percentage",
             subtitle = paste0("Data as of ", format.Date(max(covid_state_daily$date), "%B %d, %Y")),
             x = "Date",
             y = "Percent of Tests Positive",
             color = "State",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        theme(legend.position = "bottom") +
        expand_limits(y = 0) +
        scale_y_continuous(labels = percent) +
        scale_color_viridis_d()
    }
    )
  
  output$total_testing_dt_state <- renderDataTable(
    {
      datatable(covid_state_daily_rank, colnames = c("State", "Percent of Tests Positive", "Percent of Population Tested", "Percent of Population Positive", "Positive Results", "Negative Results", "Total Tests", "Ranking - Percent of Tests Positive", "Ranking - Total Positives", "Ranking - Total Tests"), 
                caption = paste0("Data as of ", format.Date(max(covid_state_daily_pop$date), "%B %d, %Y"))) %>% 
        formatPercentage(2:4, digits = 2) %>% 
        DT::formatRound(5:7, digits = 0)
    }
    )
  
# US Output ---------------------------------------------------------------

  output$cases_US <- renderPlot(
    {
      covid_data_us %>%
        filter(date >= input$daterange_US[1] & date <= input$daterange_US[2]) %>%
        ggplot() + {
          if(input$casetype_US == "New Cases")
            geom_line(aes(y = New_Cases, x = date), na.rm = T)} + {
              if(input$casetype_US == "Total Cases")
                geom_line(aes(y = Cumulative_Cases, x = date), na.rm = T)} +
        labs(title = input$casetype_US,
             subtitle = paste0("Data as of ", format.Date(max(covid_data_us$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Cases",
             color = "State",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )
  
  output$deaths_US <- renderPlot(
    {
      covid_data_us %>%
        filter(date >= input$daterange_US[1] & date <= input$daterange_US[2]) %>%
        ggplot() + {
          if(input$casetype_US == "New Cases")
            geom_line(aes(y = New_Deaths, x = date), na.rm = T)} + {
              if(input$casetype_US == "Total Cases")
                geom_line(aes(y = Cumulative_Deaths, x = date), na.rm = T)} +
        labs(title = paste0(str_remove(input$casetype_US, " Cases"), " Deaths"),
             subtitle = paste0("Data as of ", format.Date(max(covid_data_us$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Deaths",
             color = "State",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )

# Global Output -----------------------------------------------------------

  output$cases_global <- renderPlot(
    {
      covid_data_global %>%
        filter(Country.Region %in% c(input$country), date >= input$daterange_global[1] & date <= input$daterange_global[2]) %>%
        ggplot() + {
          if(input$casetype_global == "New Cases")
            geom_line(aes(y = New_Cases, x = date, colour = Country.Region), na.rm = T)} + {
              if(input$casetype_global == "Total Cases")
                geom_line(aes(y = Cumulative_Cases, x = date, colour = Country.Region), na.rm = T)} +
        labs(title = input$casetype_global,
             subtitle = paste0("Data as of ", format.Date(max(covid_data_global$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Cases",
             color = "Country",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma)
    }
  )
  
  output$deaths_global <- renderPlot(
    {
      global_recover_1 <- if (identical(input$casetype_global, "New Cases")) {
        filter(covid_data_global_longer,Case_Outcome %in% c("New_Deaths", "New_Recovered"))
      } else if (identical(input$casetype_global, "Total Cases")) {
        filter(covid_data_global_longer,Case_Outcome %in% c("Cumulative_Deaths", "Cumulative_Recovered"))
      }
      
      global_recover_2 <- global_recover_1 %>% 
        filter(Country.Region %in% c(input$country), date >= input$daterange_global[1] & date <= input$daterange_global[2])
      
      global_recover_2 %>% 
        ggplot() + 
        geom_line(aes(y = Count, x = date, colour = Country.Region, linetype = Case_Outcome), na.rm = T) +
        labs(title = paste0(str_remove(input$casetype_global, " Cases"), " Outcomes"),
             subtitle = paste0("Data as of ", format.Date(max(covid_data_global$date), "%B %d, %Y")),
             x = "Date",
             y = "Number of Outcomes",
             color = "Country",
             linetype = "Case Outcome",
             caption = "@kylebeni012 | @staturdays") +
        staturdays_theme +
        theme(legend.position = "bottom") +
        guides(color = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE)) +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma) +
        {
          if(input$casetype_global == "New Cases")
            scale_linetype_manual(labels = c("New Deaths", "New Recoveries"), values = c("solid", "dashed"))
        } +
        {
          if(input$casetype_global == "Total Cases")
            scale_linetype_manual(labels = c("Total Deaths", "Total Recoveries"), values = c("solid", "dashed"))
        }
      }
  )
  
  output$recovery_rate_global <- renderDataTable(
    datatable(covid_data_global_recoveries, colnames = c("Country", "Total Deaths", "Total Recoveries", "Recovery Rate"), 
              caption = "Total Recoveries Divided By Recoveries Plus Deaths") %>% 
      formatPercentage(4, digits = 2) %>% 
      DT::formatRound(2:3, digits = 0)
  )
}

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
