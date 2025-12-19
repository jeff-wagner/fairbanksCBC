# ---------------------------------------------------------------------------
# Fairbanks Christmas Bird Count Web Viewer
# Author: Jeff Wagner
# Last Updated: 2025-12-10
# Usage: Must be executed in R 4.0.0+.
# Description: This script builds a shiny web application to visualize the Arctic Audubon Society's Christmas Bird Count (CBC) data from 1964-present.
# ---------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(markdown)
library(bslib)
library(plotly)
library(dplyr)
library(tidyr)

options(scipen = 4)

# Load the data
cbc_data <- read.csv("./data/cbc_data_1964-2024_day.csv", check.names = FALSE) # bird counts
cbc_effort <- read.csv("./data/cbc_data_1964-2024_effort.csv", check.names = FALSE) # search effort 

# Add row for lumped redpolls
redpolls <- cbc_data %>%
  filter(Species %in% c("Redpoll sp.", "Common Redpoll (Now Redpoll)", "Hoary Redpoll")) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~ "Redpoll")
  )) %>%
  slice_tail(n=1)

cbc_data <- rbind(redpolls, cbc_data) %>% 
  mutate(Species = case_match(Species, "Common Redpoll (Now Redpoll)" ~ "Common Redpoll", .default = Species))

# Reshape data to long format
cbc_long <- cbc_data %>%
  pivot_longer(
    cols = -Species,
    names_to = "Year",
    values_to = "Count"
  ) %>%
  mutate(Year = as.integer(Year))

effort_long <- cbc_effort %>%
  arrange(effort_type) %>%
  pivot_longer(
    cols = -effort_type,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))

# Add in column for count/effort (total hours)
effort_hours <- effort_long %>% filter(effort_type == "total_hours")
cbc_long$total_hours <- NA

for(i in unique(cbc_long$Species)){
  cbc_long[cbc_long$Species == i, "total_hours"] <- effort_hours$Value
}

cbc_long$Count_total_hours <- ifelse(is.na(cbc_long$Count/cbc_long$total_hours), 0, cbc_long$Count/cbc_long$total_hours)

# Redpoll species for joint plotting
redpoll_species <- c("Common Redpoll", "Hoary Redpoll", "Redpoll sp.", "Redpoll")

# Get unique species for dropdown, replacing individual redpolls with just "Redpoll" and making sure Total is the first value
dropdown_species <- sort(c("Redpoll", setdiff(unique(cbc_long$Species), redpoll_species)))
total_index <- which(dropdown_species == "Total (all species)")
dropdown_species <- dropdown_species[c(total_index,1:total_index-1,total_index+1:length(dropdown_species))]
dropdown_datatype <- c("Raw Count", "Count/Survey Hours")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Fairbanks CBC Data Portal",
                  titleWidth = 280),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      br(),
      tags$a(href='https://arcticaudubon.org',
             tags$img(src='https://images.squarespace-cdn.com/content/v1/5a8fbea11aef1d984ca0fd28/ced25897-faeb-491b-a089-d75ad52cea2b/Circle+logo+bold.png?format=1500w', 
                      width = "290", )),
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Species Observations", icon = icon("crow"), tabName = "obs"),
      menuItem("Search Effort", icon = icon("shoe-prints"), tabName = "effort"),
      menuItem("Release Notes", tabName = "releases", icon = icon("code-branch")),
      HTML(paste0(
        "<br><br><br>",
        "<table style='margin-left:auto; margin-right:auto;'>",
        "<tr>",
        "<td style='padding: 10px;'><a href='https://www.facebook.com/arcticaudubon' target='_blank'><i class='fab fa-facebook-square fa-2x'></i></a></td>",
        "<td style='padding: 10px;'><a href='https://www.instagram.com/arcticaudubon' target='_blank'><i class='fab fa-instagram fa-2x'></i></a></td>",
        "</tr>",
        "</table>",
        "<br>")),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'> Jeff Wagner - <script>document.write(yyyy);</script></small></p>")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$head(includeHTML("www/google-analytics.html")),
      ),
    tabItems(
      tabItem(tabName = "home",
              includeMarkdown("www/home.Rmd")
      ),
      tabItem(tabName = "obs",
              # ggplot2 species charts section
              includeMarkdown("www/obs.Rmd"),

              fluidRow(column(3,
                              selectInput(inputId = "species", 
                                          label = "Select Species",
                                          choices = dropdown_species,
                                          selected = NULL,
                                          multiple = FALSE,
                                          selectize = TRUE,
                                          # options = list(
                                          #  onFocus = I("function () {this.clear( false );}")
                                          #  )
                                         ),
                              ),
                       column(6, br(), p("(* To search: hit backspace to clear current selection, then type species name)",
                                         style = "font-size: 14px;")
                              ),
                       column(3,
                              selectInput(inputId = "data_type",
                                          label = "Select Data Type",
                                          choices = dropdown_datatype,
                                          selected = NULL,
                                          multiple = FALSE,
                                          selectize = TRUE))
                       ),
              column(12, plotlyOutput("cbcPlot") %>% shinycssloaders::withSpinner(color = "green"),
                     uiOutput("redPointNote"), uiOutput("redpollNote"), uiOutput("obseffortNote"))
              ),
              
      tabItem(tabName = "effort",
              includeMarkdown("www/effort.Rmd"),
              fluidRow(column(3,
                              selectInput("effort_type", "Select Observer Effort Type:",
                                          choices = unique(effort_long$effort_type),
                                          selected = NULL,
                                          multiple = FALSE,
                                          selectize = TRUE))),
              column(12, plotlyOutput("effortPlot") %>% shinycssloaders::withSpinner(color = "green"),
                     uiOutput("effortNote"))
      ),
      tabItem(tabName = "releases",
              includeMarkdown("www/releases.Rmd")
        )
    )
  )
)


server <- function(input, output, session) {
  output$menu <- renderMenu({
    sidebarMenu()
  })
  output$cbcPlot <- renderPlotly({
    # Determine which dataset to plot
    if (input$data_type == "Raw Count") {
      # Determine which species to plot
      if (input$species == "Redpoll") {
        species_data <- cbc_long %>% filter(Species %in% redpoll_species)
        
        # Convert -1 counts to 0 but mark them
        species_data <- species_data %>%
          mutate(Count_Plot = ifelse(Count == -1, 0, Count),
                 Count_Week = ifelse(Count == -1, "Count Week", "Count Day"))
        
        # Create interactive plot
          # for y-axis scale
          y_max <- max(species_data$Count_Plot, na.rm = TRUE)
          
          if (y_max == 0) {
            y_min <- -0.05  # or some small fixed value
            y_max <- 0.5   # small positive value to create visible range
          } else {
            y_min <- -0.05 * y_max
            y_max <- max(species_data$Count_Plot, na.rm = TRUE)*1.1
          }
        
        plot_ly() %>%
          # Add trace for lumped "Redpoll" totals in its own legend group
          add_trace(data = species_data %>% filter(Species == "Redpoll"), 
                    x = ~Year, y = ~Count_Plot, 
                    type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8), 
                    name = "Redpoll (lumped total)", 
                    legendgroup = "total_group",
                    #legendgrouptitle = list(text = "Lumped total:"),
                    visible = TRUE) %>%
          # Add traces for other reported species names with their own legend group
          add_trace(data = species_data %>% filter(Species != "Redpoll"), 
                    x = ~Year, y = ~Count_Plot, 
                    type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8),
                    name = ~Species,
                    #legendgroup = "reported_species",
                    #legendgrouptitle = list(text = "Reported as:"),
                    visible = "legendonly") %>%
          layout(title = list(text = paste("Annual", input$species, "Raw Observations"),
                              font = list(family = "Arial Black"),
                              xanchor = "center", yanchor = "top",
                              pad = list(t = 1)),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Count", range = c(y_min, y_max)),
                 showlegend = TRUE
                 #legend = list(title = list(text = "Reported as:"))
          )
        
      } else {
        species_data <- cbc_long %>% filter(Species == input$species)
        
        # Convert -1 counts to 0 but mark them
        species_data <- species_data %>%
          mutate(Count_Plot = ifelse(Count == -1, 0, Count),
                 Count_Week = ifelse(Count == -1, "Count Week", "Count Day"))
        
        # Create interactive plot
          # for y-axis scale
          y_max <- max(species_data$Count_Plot, na.rm = TRUE)
          
          if (y_max == 0) {
            y_min <- -0.05  # or some small fixed value
            y_max <- 0.5   # small positive value to create visible range
          } else {
            y_min <- -0.05 * y_max
            y_max <- max(species_data$Count_Plot, na.rm = TRUE)*1.1
          }
        
        plot_ly(species_data, x = ~Year, y = ~Count_Plot, type = 'scatter', mode = 'lines+markers',
                marker = list(size = 8, color = ~ifelse(Count_Week == "Count Week", "red", "#1f77b4")),
                line = list(color = 'grey')) %>%
          layout(title = list(text = paste("Annual", input$species, "Raw Observations"),
                              font = list(family = "Arial Black"),
                              xanchor = "center", yanchor = "top",
                              pad = list(t = 1)),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Count", range = c(y_min, y_max)),
                 showlegend = FALSE)
      }
    } else {
      # Determine which species to plot
      if (input$species == "Redpoll") {
        species_data <- cbc_long %>% filter(Species %in% redpoll_species)
        
        # Convert -1 counts to 0 but mark them
        species_data <- species_data %>%
          mutate(Count_Plot = ifelse(Count == -1, 0, Count_total_hours),
                 Count_Week = ifelse(Count == -1, "Count Week", "Count Day"))
        
        # Create interactive plot
          # for y-axis scale
          y_max <- max(species_data$Count_Plot, na.rm = TRUE)
          
          if (y_max == 0) {
            y_min <- -0.05  # or some small fixed value
            y_max <- 0.5   # small positive value to create visible range
          } else {
            y_min <- -0.05 * y_max
            y_max <- max(species_data$Count_Plot, na.rm = TRUE)*1.1
          }
        
        plot_ly() %>%
          # Add trace for lumped "Redpoll" totals in its own legend group
          add_trace(data = species_data %>% filter(Species == "Redpoll"), 
                    x = ~Year, y = ~Count_Plot, 
                    type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8), 
                    name = "Redpoll (lumped total)", 
                    legendgroup = "total_group",
                    #legendgrouptitle = list(text = "Lumped total:"),
                    visible = TRUE) %>%
          # Add traces for other reported species names with their own legend group
          add_trace(data = species_data %>% filter(Species != "Redpoll"), 
                    x = ~Year, y = ~Count_Plot, 
                    type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8),
                    name = ~Species,
                    #legendgroup = "reported_species",
                    #legendgrouptitle = list(text = "Reported as:"),
                    visible = "legendonly") %>%
          layout(title = list(text = paste("Annual", input$species, "Observations per Survey Hour"),
                              font = list(family = "Arial Black"),
                              xanchor = "center", yanchor = "top",
                              pad = list(t = 1)),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Count/Survey Hour", range = c(y_min, y_max)),
                 showlegend = TRUE
                 #legend = list(title = list(text = "Reported as:"))
          )
        
      } else {
        species_data <- cbc_long %>% filter(Species == input$species)
        
        # Convert -1 counts to 0 but mark them
        species_data <- species_data %>%
          mutate(Count_Plot = ifelse(Count == -1, 0, Count_total_hours),
                 Count_Week = case_when(
                   Count == -1 ~ "Count Week",
                   is.na(total_hours) ~ "Missing Survey Data",
                   .default = "Count Day"))
        
        # Create interactive plot
          # for y-axis scale
          y_max <- max(species_data$Count_Plot, na.rm = TRUE)
          
          if (y_max == 0) {
            y_min <- -0.05  # or some small fixed value
            y_max <- 0.5   # small positive value to create visible range
          } else {
            y_min <- -0.05 * y_max
            y_max <- max(species_data$Count_Plot, na.rm = TRUE)*1.1
          }
        
          plot_ly(type = 'scatter', mode = 'markers') %>% 
            add_trace(data = filter(species_data, Count_Week != "Missing Survey Data"),
                      x = ~Year,
                      y = ~Count_Plot,
                      marker = list(size = 8,
                                    symbol = ~case_when(
                                      Count_Week == "Count Week" ~ "circle",
                                      Count_Week == "Count Day" ~ "circle",
                                      Count_Week == "Missing Survey Data" ~ "None"),
                                    color = ~case_when(
                                      Count_Week == "Count Week" ~ "red",
                                      Count_Week == "Count Day" ~ "#1f77b4",
                                      Count_Week == "Missing Survey Data" ~ "black"))) %>%
            add_trace(data = filter(species_data, Count_Week == "Missing Survey Data"),
                      x = ~Year,
                      y = ~Count_Plot,
                      marker = list(size = 8, color = "black", symbol = "circle-open")) %>%
            add_lines(data = filter(species_data, Count_Week != "Missing Survey Data"),
                      x = ~Year,
                      y = ~Count_Plot,
                      line = list(color = 'grey')) %>% 
            layout(title = list(text = paste("Annual", input$Species, "Observations per Survey Hour"),
                                font = list(family = "Arial Black"),
                                xanchor = "center", yanchor = "top",
                                pad = list(t = 1)),
                   xaxis = list(title = "Year"),
                   yaxis = list(title = "Count/Survey Hour", range = c(y_min, y_max)),
                   showlegend = FALSE)
      }
    }

    
  })
  
  output$effortPlot <- renderPlotly({
    
    # Determine which effort metric to plot
    effort_data <- effort_long %>% filter(effort_type == input$effort_type)
    
    # for y-axis scale
    y_max <- max(effort_data$Value, na.rm = TRUE)
    
    if (y_max == 0) {
      y_min <- -0.05  # or some small fixed value
      y_max <- 0.5   # small positive value to create visible range
    } else {
      y_min <- -0.05 * y_max
      y_max <- max(effort_data$Value, na.rm = TRUE)*1.1
    }
    
    # Create interactive plot
    suppressWarnings({
      plot_ly(type = 'scatter', mode = 'markers') %>% 
        add_trace(data = effort_data,
                  x = ~Year,
                  y = ~Value,
                  marker = list(size = 8,
                                color = "#1f77b4")) %>%
        add_trace(data = filter(effort_data, is.na(Value)),
                  x = ~Year,
                  y = 0,
                  marker = list(size = 8, color = "black", symbol = "circle-open")) %>%
        add_lines(data = effort_data,
                  x = ~Year,
                  y = ~Value,
                  line = list(color = 'grey'),
                  connectgaps = TRUE) %>% 
        layout(title = list(text = paste("Annual Search Effort:", input$effort_type),
                            font = list(family = "Arial Black"),
                            xanchor = "center", yanchor = "top",
                            pad = list(t = 1)),
               xaxis = list(title = "Year", range = c(min(effort_data$Year)-2, max(effort_data$Year)+2)),
               yaxis = list(title = paste(input$effort_type), range = c(y_min, y_max)),
               showlegend = FALSE)
    })
    
  })
  
  output$redPointNote <- renderUI({
    species_data <- cbc_long %>%
      filter(Species == input$species, Count == -1)
    
    if (nrow(species_data) > 0) {
      tags$p("Red points denote years where this species was observed during count week, but not on the official count day.")
    } else {
      NULL
    }
  })
  
  output$redpollNote <- renderUI({
    if (input$species == "Redpoll") {
      tags$p("In July 2024, the American Ornithological Society announced the lumping of the Hoary Redpoll into the Common Redpoll. Thus, prior CBC records have been lumped into a combined total. Click on the defunct species names to toggle prior reporting breakdowns.")
    } else {
      NULL
    }
  })
  
  output$obseffortNote <- renderUI({
    if (input$data_type == "Count/Survey Hours") {
      tags$p("Unfilled points denote years search effort data is missing.")
    } else {
      NULL
    }
  })
  
  output$effortNote <- renderUI({
      tags$p("Unfilled points denote years search effort data is missing.")
  })
  
  observeEvent(input$link_to_releases, {
    updateTabItems(session, inputId = "tabs", "releases")
  })
  
  observeEvent(input$link_to_obs, {
    updateTabItems(session, inputId = "tabs", "obs")
  })
}

shinyApp(ui, server, enableBookmarking = "url")
