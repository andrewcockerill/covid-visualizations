#########################################################
# Packages
#########################################################

library(dplyr)
library(plotly)
library(reshape2)
library(htmltools)

#########################################################
# Helper functions for data loading and transforms
#########################################################


get_raw_data <- function(){
  # Fetch and store raw data files from GitHub as a list
  
  # Data directory
  repository <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data"
  
  # File names
  file_names <- c("csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                  "csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                  "UID_ISO_FIPS_LookUp_Table.csv",
                  "csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
                  "csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  
  file_names <- mapply(file.path, repository, file_names)
  
  # File descriptions
  file_desc <- c("global_confirmed",
                 "global_deaths",
                 "population",
                 "us_confirmed",
                 "us_deaths")
  
  # Output list
  out_list <- lapply(file_names, read.csv)
  names(out_list) <- file_desc
  
  return(out_list)
  
}

format_us_data <- function(raw_list, input_name){
  # Setup dataframes
  df <- raw_list[[input_name]]
  feature <- gsub(".*_", "", input_name)
  pop_df <- raw_list[["population"]]
  
  # Population is split into county level data
  # At this time will limit scope to entire states,
  # so these will be grouped and summed at the state level
  pop_df <- pop_df %>%
    filter(Country_Region=="US") %>%
    select(region=Province_State, population=Population) %>%
    group_by(region) %>%
    summarize(population=sum(population, na.rm=TRUE)) %>%
    filter(population>0)

  # Confirmed/death data is split differently - any country that
  # can be divided into subregions does not include a separate row
  # for the whole country - must sum counts
    
  out_df <- df %>%
    select(-c("UID","iso2","iso3","code3","FIPS","Admin2",
              "Country_Region","Lat","Long_","Combined_Key")) %>%
    melt(id.vars="Province_State") %>%
    mutate(variable=as.Date(variable, format="X%m.%d.%y")) %>%
    select(region=Province_State, dt=variable, value=value) %>%
    group_by(region, dt) %>%
    summarize(value=sum(value)) %>%
    ungroup() %>%
    arrange(region, dt) %>%
    group_by(region) %>%
    mutate(value_rate_ma = (value - lag(value,n=7,default=NA))/7) %>%
    inner_join(pop_df, by=c("region")) %>%
    mutate(value_prop = value/population*100000, value_rate_ma_prop = value_rate_ma/population*100000) %>%
    ungroup() %>%
    group_by(region) %>%
    mutate(value_rate_ma_prop_accel = (value_rate_ma_prop - lag(value_rate_ma_prop, n=7, default=NA)) / 7)
  
  # Variable names
  names(out_df) <- gsub("value", feature, names(out_df))
  
  # Output
  return(out_df)
  
}

format_global_data <- function(raw_list, input_name){
  # Function to transpose and format global tracking data
  # and join to population measures
  
  # Setup dataframes
  df <- raw_list[[input_name]]
  feature <- gsub(".*_", "", input_name)
  pop_df <- raw_list[["population"]]
  
  # Population data includes a single row for the entire country,
  # then additional rows if that country is further divided into
  # provinces/states/territories etc.
  pop_df <- pop_df %>%
    filter(Province_State=="") %>%
    select(region=Country_Region, population=Population)

  
  # Confirmed/death data is split differently - any country that
  # can be divided into subregions does not include a separate row
  # for the whole country - must sum counts
  
  # Manipulations
  out_df <- df %>%
    #filter(Province.State=="") %>%
    select(-c("Province.State","Lat","Long")) %>%
    melt(id.vars="Country.Region") %>%
    mutate(variable=as.Date(variable, format="X%m.%d.%y")) %>%
    select(region=Country.Region, dt=variable, value=value) %>%
    group_by(region, dt) %>%
    summarize(value=sum(value)) %>%
    ungroup() %>%
    arrange(region, dt) %>%
    group_by(region) %>%
    mutate(value_rate_ma = (value - lag(value,n=7,default=NA))/7) %>%
    inner_join(pop_df, by=c("region")) %>%
    mutate(value_prop = value/population*100000, value_rate_ma_prop = value_rate_ma/population*100000) %>%
    ungroup() %>%
    group_by(region) %>%
    mutate(value_rate_ma_prop_accel = (value_rate_ma_prop - lag(value_rate_ma_prop, n=7, default=NA)) / 7)
  
  # Variable names
  names(out_df) <- gsub("value", feature, names(out_df))

  # Output
  return(out_df)
 
}

init_datasets <- function(){
  # Function to load raw data and run all transforms, storing output
  # as a list of dataframes
  
  # Get raw data
  raw_data <- get_raw_data()
  
  # Run transforms
  
  # Global data
  global_dataset_names <- names(raw_data)[1:2]
  global_datasets <- lapply(global_dataset_names, function(x){
    return(format_global_data(raw_data, x))
  })
  names(global_datasets) <- global_dataset_names
  
  # US data
  us_dataset_names <- names(raw_data[4:5])
  us_datasets <- lapply(us_dataset_names, function(x){
    return(format_us_data(raw_data,x))
  })
  
  names(us_datasets) <- us_dataset_names
  
  # Output
  return(c(global_datasets, us_datasets))
}

plot_global_data <- function(df_list, metrics, selected_metric, regions, normalize){
  # Function to produce a line graph of a global tracking metric
  # (confirmed or deaths) with options to filter by region
  # and normalize by population
  
  # Context metric
  metric_ref <- selected_metric
  metric_col_name_1 <- gsub("global_|us_","", metric_ref)
  metric_col_name_2 <- paste(metric_col_name_1,"rate_ma",sep="_")
  xtitle <- paste("Total", metric_col_name_1)
  ytitle <- paste("Daily Rate of New", metric_col_name_1, "- 7 day rolling avg.")
  plot_title <- paste("Rate of New Covid-19", metric_col_name_1, "vs. Total", metric_col_name_1)


  # Normalize population
  if(normalize==TRUE){
    metric_col_name_1 <- paste(metric_col_name_1,"prop",sep="_")
    metric_col_name_2 <- paste(metric_col_name_2,"prop",sep="_")
    xtitle <- paste(xtitle, "(per 100k)")
    ytitle <- paste(ytitle, "(per 100k)")
    plot_title <- paste(plot_title, "(per 100k)")
  }

  
  df <- df_list[[metric_ref]] %>%
    filter(region %in% regions)
  
  df <- df[c("region", "dt", metric_col_name_1, metric_col_name_2)]
  names(df) <- c("region", "dt", "value", "value_rate")
  
  # Plot
  fig <- plot_ly(data=df, x=~value, y=~value_rate, type="scatter", mode="lines+markers", color=~region,
          text=~dt, hovertemplate = paste('<b>Rate</b>: %{y}',
                                                        '<br><b>Value</b>: %{x}<br>',
                                                        '<br><b>Date</b>: %{text}<br>')) %>%
    layout(title=plot_title, xaxis=list(title=xtitle), yaxis=list(title=ytitle))
  
  return(fig)

}


#########################################################
# Application
#########################################################

# Pull data and format
datasets <- init_datasets()

# Global (world) region options
global_regions <- (datasets[[1]] %>% select(region) %>% unique())$region
global_metrics <- names(datasets)[1:2]
names(global_metrics) <- c("Confirmed Cases", "Deaths")
global_sortings <- c("A-Z", "Case Rate (per 100k)","Case Acceleration (per 100k)")

# US options
us_regions <- (datasets[[3]] %>% select(region) %>% unique())$region
us_metrics <- names(datasets)[3:4]
names(us_metrics) <- c("Confirmed Cases", "Deaths")
us_sortings <- c("A-Z", "Case Rate (per 100k)","Case Acceleration (per 100k)")

# UI
ui <- fluidPage(
  titlePanel("Covid-19 World/US Visualizations"),
  
  h3("test"),
  
  tabsetPanel(
    
    # Global tab
    tabPanel(
      title="Global",
      sidebarLayout(
        sidebarPanel(
          tags$head(tags$style("#global_plot{height:60vh !important;}")),
          width = 2,
          selectInput("global_metric", "Metric", choices = global_metrics),
          selectInput("global_sorting", "Sort Regions By", choices = global_sortings),
          selectizeInput("global_region", "Region", choices = global_regions, multiple = TRUE),
          checkboxInput("global_normalize", "Normalize Per 100k", FALSE),
          actionButton("global_run", "Run")
        ),
        
        mainPanel( HTML("<br><br>"),plotlyOutput("global_plot")
        )
        
      )
    ),
    
    # USA drilldown tab
    tabPanel(
      title="USA",
      sidebarLayout(
        sidebarPanel(
          tags$head(tags$style("#us_plot{height:60vh !important;}")),
          width = 2,
          selectInput("us_metric", "Metric", choices = us_metrics),
          selectInput("us_sorting", "Sort Regions By", choices = us_sortings),
          selectizeInput("us_region", "Region", choices = us_regions, multiple = TRUE),
          checkboxInput("us_normalize", "Normalize Per 100k", FALSE),
          actionButton("us_run", "Run")
        ),
        
        mainPanel( HTML("<br><br>"),plotlyOutput("us_plot")
        )
        
      )
    )
    
  )
  
)

# Server
server <- function(input, output, session){
  
  # Global plot settings
  observe({
    global_sorting_selected <- input$global_sorting
    
    # User can sort the dropdown list by recent trends in metrics
    if(global_sorting_selected=="Case Acceleration (per 100k)"){
      
      regions_sorted <- datasets[[1]] %>% filter(dt==max(datasets[[1]]$dt)) %>% arrange(-confirmed_rate_ma_prop_accel) %>% select(region)
      regions_sorted <- regions_sorted$region
      
      updateSelectizeInput(session, "global_region", choices = regions_sorted)
    } else if(global_sorting_selected=="Case Rate (per 100k)") {
      
      regions_sorted <- datasets[[1]] %>% filter(dt==max(datasets[[1]]$dt)) %>% arrange(-confirmed_rate_ma_prop) %>% select(region)
      regions_sorted <- regions_sorted$region
      
      updateSelectizeInput(session, "global_region", choices = regions_sorted)
    } else {
      updateSelectizeInput(session, "global_region", choices = global_regions)
    }
    
  })
  
  # Global Plotly output
  global_regions_selected <- eventReactive(input$global_run, {input$global_region})
  global_metric_selected <- eventReactive(input$global_run, {input$global_metric})
  global_normalize_selected <- eventReactive(input$global_run, {input$global_normalize})

  output$global_plot <- renderPlotly({
    p <- plot_global_data(datasets, metrics = global_metrics, selected_metric = global_metric_selected() ,
                     regions=global_regions_selected(), normalize=global_normalize_selected())
    
  })
  
  # US plot settings
  observe({
    us_sorting_selected <- input$us_sorting
    
    # User can sort the dropdown list by recent trends in metrics
    if(us_sorting_selected=="Case Acceleration (per 100k)"){
      
      regions_sorted_us <- datasets[[3]] %>% filter(dt==max(datasets[[3]]$dt)) %>% arrange(-confirmed_rate_ma_prop_accel) %>% select(region)
      regions_sorted_us <- regions_sorted_us$region
      
      updateSelectizeInput(session, "us_region", choices = regions_sorted_us)
    } else if(us_sorting_selected=="Case Rate (per 100k)") {
      
      regions_sorted_us <- datasets[[3]] %>% filter(dt==max(datasets[[3]]$dt)) %>% arrange(-confirmed_rate_ma_prop) %>% select(region)
      regions_sorted_us <- regions_sorted_us$region
      
      updateSelectizeInput(session, "us_region", choices = regions_sorted_us)
    } else {
      updateSelectizeInput(session, "us_region", choices = us_regions)
    }
    
  })
  
  # US Plotly output
  us_regions_selected <- eventReactive(input$us_run, {input$us_region})
  us_metric_selected <- eventReactive(input$us_run, {input$us_metric})
  us_normalize_selected <- eventReactive(input$us_run, {input$us_normalize})

  
  output$us_plot <- renderPlotly({
    p <- plot_global_data(datasets, metrics = us_metrics, selected_metric = us_metric_selected() ,
                          regions=us_regions_selected(), normalize=us_normalize_selected())
    
  })
  
  
}

shinyApp(ui, server)
