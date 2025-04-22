#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(plotly)
library(tidyverse)
library(janitor)

data <- read_delim("Anemia-prevalence-WDI-2000-2019.csv", delim = ",", na = c("", "NA", "..")) %>% 
  clean_names() %>% 
  pivot_longer(cols = starts_with("x2")) %>% 
  mutate(year = as.numeric(str_extract(name, "[:digit:]{4}"))) %>% 
  filter(!is.na(value))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Anaemia Prevalence in Pregnant Women and Women of Reproductive Age"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "trace_group",
            "Population:",
            c("Pregnant women" = "SH.PRG.ANEM", "Women of reproductive age (15-49)" = "SH.ANM.ALLW.ZS"),
            selected = "SH.PRG.ANEM",
            multiple = FALSE
          ),
          selectizeInput(
            "traces",
            "Countries and regions",
            choices = unique(data$country_name),
            selected = c(
              "World",
              "European Union",
              "Low income"),
            multiple = TRUE,
            options = list(maxItems = 6)
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(plotlyOutput("distPlot", height = "700px")),
          br(),
          br(),
          br(),
          tabsetPanel(
            tabPanel(
              "Data source", 
              br(),
              br(),
              br(),
              div("World Development Indicators (WDI), The World Bank (2025)."),
              br(),
              div("Retrieved from: https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators"),
              br(),
              div("Retrieved on: 19.04.2025"),
              align="center"),
            tabPanel(
              "R Code",
              fluidRow(
                       br(),
                       br(),
                       br(), 
                       uiOutput("github_link", align = "center"),
                       br(),
                       br(),
                       img(src = "github_mark.png", height = 90, width = 90), 
                       align = "center",
                       br(),
                       br(),
                       br()
                     )
              ),
            tabPanel(
              "Contact",
              br(),
              br(),
              br(),
              div("Reach out", a("Rumpf_F@ukw.de", href = "mailto:Rumpf_F@ukw.de")),
              br(),
              br(),
              br(),
              align="center")
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$github_link <- renderUI({
    tagList("R Code is available on", a("GitHub", href="https://github.com/upfl0/anaemia-prevalence-wdi"))
  })

    output$distPlot <- renderPlotly({
      
      arial <- list(
        family = "Arial",
        size = 15)
      
      data %>% 
        filter(series_code %in% input$trace_group) %>% 
        filter(country_name %in% input$traces) %>% 
        mutate(country_name = fct_reorder2(country_name, year, value)) %>% 
        plot_ly(
        name = ~country_name,
        type = "scatter",
        mode = 'lines',
        x = ~year,
        y = ~value,
        color = ~country_name,
        colors = "Set1",
        marker = list(
          pattern = list(
            bgcolor = ~country_name,
            fgopacity = 1
          )
        )
      ) %>% 
        #add_text(textfont = arial, textposition = "top right") %>% 
        layout(
          font = arial,
          hovermode="x unified",
          legend = list(
            x = 0.6, 
            y = 0.03, 
            bgcolor = 'rgba(0,0,0,0)'),
          xaxis = list(
            title = "Year",
            zeroline = F
          ),
          yaxis = list(
            title = "Prevalence (%)", 
            rangemode = "tozero",
            zeroline = F,
            tickformat = ",",
            hoverformat = '.1f')
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
