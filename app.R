#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(fmsb)

player_stats <- read_csv("DCGFFL_Player_Stats.csv")

max_min_scale <- function(x, filtered=F, fil_max, fil_min){
  if (filtered) {
    return((x-fil_min)/(fil_max-fil_min))
  }
  return((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DCGFFL Stat Visualizer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            pickerInput(
              "player",
              "Player:",
              player_stats$playerName,
              options = pickerOptions(liveSearch=T)
            ),
            sliderInput(
              "prSnaps",
              "Min Pass Rush Snaps:",
              min=0, max=max(player_stats$prSnaps), value = 5
            ),
            sliderInput(
              "ppSnaps",
              "Min Pass Protection Snaps:",
              min=0, max=max(player_stats$ppSnaps), value = 5
            ),
            sliderInput(
              "targets",
              "Min Targets:",
              min=0, max=max(player_stats$recTarget), value = 5
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("statPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  pr_data <- reactive({filter(player_stats, prSnaps >= input$prSnaps)})
  rec_data <- reactive({filter(player_stats, recTarget >= input$targets)})
  pp_data <- reactive({filter(player_stats, ppSnaps >= input$ppSnaps)})
  
  norm_stats <- reactive({
      mutate(player_stats,
             explosiveness = max_min_scale(explosiveness,T,
                                           max(rec_data()$explosiveness, na.rm=T),
                                           min(rec_data()$explosiveness, na.rm=T)),
              possession = max_min_scale(possession,T,
                                         max(rec_data()$possession, na.rm=T),
                                         min(rec_data()$possession, na.rm=T)),
              defTackle = max_min_scale(defTackle/games),
              ppPRA = 1-max_min_scale(ppPRA,T,max(pp_data()$ppPRA, na.rm=T),
                                      min(pp_data()$ppPRA, na.rm=T)),
              prBPR = max_min_scale(prBPR,T,max(pr_data()$prBPR, na.rm=T),
                                    min(pr_data()$prBPR, na.rm=T)),
              prUPR = max_min_scale(prUPR,T,max(pr_data()$prUPR, na.rm=T),
                                    min(pr_data()$prUPR, na.rm=T)))})
  
  
  player_data <- reactive({
    filter(norm_stats(), playerName == input$player) |>
    select(all_of(c("explosiveness","possession","defTackle","ppPRA","prBPR",
                    "prUPR"))) |>
    mutate(across(everything(), ~replace(., is.na(.), 0)))})
  
  output$statPlot <- renderPlotly({
      plot_ly(
         type = 'scatterpolar',
         mode = "closest",
         fill = "toself"
       ) |>
      add_trace(
        r = as.matrix(player_data()),
        theta = c("Exp.","Poss.","Tackle","PRA","BPR","UPR"),
        showlegend = T,
        mode = "markers",
        name = input$player
      ) |>
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        )
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
