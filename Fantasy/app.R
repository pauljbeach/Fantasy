
library(shiny)
library(librarian)
library(DT)
shelf(tidyverse, janitor, ffanalytics, fflr, ffscrapr, ggrepel, ggbeeswarm, zoo)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fantasy App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("nteams",
                        "Number of Teams:",
                        min = 1,
                        max = 16,
                        value = 14),
            numericInput("draftpos",
                         "Draft Position",
                         value = 2),
            numericInput("risk",
                         "Risk Tolerance",
                         value = 4),
            numericInput("injury",
                         "Injury Tolerance",
                         value = 10),
            numericInput("indrisk",
                         "Max Indivial Risk",
                         value = .5),
            checkboxInput("forecast",
                          "Forecast Ahead",
                          value = F),
            selectizeInput("playerlist",
                           "Players Taken",
                           optimizeData$player,
                           multiple = T),
            selectizeInput("benchlist",
                           "Bench Players Taken",
                           optimizeData$player,
                           multiple = T)
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                    tabPanel("Best Team",DTOutput("df"), DTOutput("sums")),
                    tabPanel("Available", DTOutput("plist")),
                    tabPanel("Bench", DTOutput("bench"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  bestteam <- reactive({
    optimizeTeam(maxRisk = input$risk,
                 forecast = input$forecast,
                 maxInjury = input$injury,
                 playerlist = input$playerlist,
                 nteams = input$nteams,
                 maxIndRisk = input$indrisk,
                 draftpos = input$draftpos,
                 current_pick = input$draftpos,
                 draftlist = dlist()
    )
  }) 
    output$df <- renderDT({
        # generate bins based on input$bins from ui.R
        bestteam()$playerInfo %>%
        datatable(options = list(dom = "t")) %>%
        formatRound(c(3:5), digits = 1)
    })
    draft_order(2,14)
    output$sums <- renderDT({
      tibble(points = bestteam()$points, risk = bestteam()$risk, pgm = bestteam()$pgm) %>%
        datatable(options = list(dom = "t"))
    })
    output$bench <- renderDT({
      # generate bins based on input$bins from ui.R
      optimizeBench(maxRisk = input$risk,
                   forecast = input$forecast,
                   playerlist = input$benchlist,
                   nteams = input$nteams,
                   draftpos = input$draftpos,
                   current_pick = input$draftpos,
                   draftlist = union(dlist(), input$playerlist)
      )$playerInfo %>%
        datatable() %>%
        formatRound(c(3:5), digits = 1)
    })
    
    output$plist <- renderDT({
      # generate bins based on input$bins from ui.R
      optimizeData %>% 
        select(player,pos,rank,espn_rank, ecr_rank,points,points_vor, uncertainty, pgm) %>%
        arrange(espn_rank) %>%
        DT::datatable(options = list(scrollY = 300,srcollCollape = T, pageLength = 100)) %>% 
        formatRound(c(6:9),digits = 1)
      })
    
    dlist <- reactive({
      od <- optimizeData %>% arrange(espn_rank) 
      od[c(input$plist_rows_selected),]$player
    }) 
}
optimizeTeam()
# Run the application 
shinyApp(ui = ui, server = server)
