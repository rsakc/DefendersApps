#Loading Packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(dplyr)
library(gdata)
library(stats)
library(viridis)


#Loading in Defenders Data
data.all <-read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 

#Filter by Level
data.all <- data.all %>% filter(Level > 0)

#Convering Columns to Factors
data.all$TurretType <- as.factor(data.all$TurretType)
data.all$Level <- as.factor(data.all$Level)
data.all$Upgrade <- as.factor(data.all$Upgrade)
data.all$Medicine <- as.factor(data.all$Medicine)
data.all$Virus <- as.factor(data.all$Virus)
data.all$Wave <- as.factor(data.all$Wave)
data.all$Location <- as.factor(data.all$Location)

#Creating Additional Columns
data.all <- mutate(data.all, PercDestroyed = Destroyed/Shot)
data.all <- mutate(data.all, Missed = Shot - Destroyed)
data.all <- mutate(data.all, PerMissed = Missed/Shot)
data.all <- mutate(data.all, PerDefective = Destroyed/Shot)

#Renaming Wave to Round
data.all <- data.all %>% rename(Round = Wave)



#To use for inputs
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))


#UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  #App Title
  titlePanel("Defenders Data Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  c("all", all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput(inputId = "playerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput("levels", "Level",
                  choices = c(1, 2, 3, 4, 5),
                  multiple = TRUE,
                  selected = "1"),
      
      selectInput(inputId = "chart",
                  label = "Chart Type",
                  #columns of the dataset
                  choices = c("Counts","Percent"),
                  selected = "Counts",
                  multiple = FALSE),
      
      selectInput(inputId = "xvar",
                  label = "X Axis:",
                  #columns of the dataset
                  choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "Medicine",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by",
                  choices = c("None", "Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "None",
                  multiple = FALSE),
      
      checkboxInput('ctest',"Chi-Sq Test", FALSE),
      
      downloadButton('downloadData', label = "Defenders Data")
      
    ),
    
    #Outputs
    mainPanel(
      plotOutput(outputId = "Plot"),
      uiOutput("header"),
      verbatimTextOutput("table")
    
   
     
    )
  )
)


#Server
server <- function(input, output,session) {
  
  #Reactive Data
  plotDataR <- reactive({
    if("all" %in% input$groupID){
      if("all" %in% input$playerID){
        data <- data.all %>% filter(Level %in% input$levels)
      } else {
        data <- data.all %>% filter(Level %in% input$levels, PlayerID %in% input$playerID)
      }
      
    } else{
      if("all" %in% input$playerID){
        data <- data.all %>% filter(Level %in% input$levels, GroupID %in% input$groupID)
      } else {
        data <- data.all %>% filter(Level %in% input$levels, PlayerID %in% input$playerID, GroupID %in% input$groupID)
      }
    }
    
    return(data)
  })
  
  
  
  # Dynamic PlayerID Input
  observe({
    
    # req() requires a selection from GroupID before any output
    # or reactivity occurs (keeps the app from crashing)
    req(input$groupID)   
    
    if ("all" %in% input$groupID) {gamedata <- data.all}
    else{gamedata <- filter(data.all, GroupID %in% input$groupID)}
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  })
  
  
  # Dynamic Level Input 
  observe({
    req(input$groupID)   
    
    if ("all" %in% input$groupID) {gamedata <- data.all}
    else{gamedata <- filter(data.all, GroupID %in% input$groupID)}
    updateSelectInput(session, 
                      "levels",
                      choices = sort(unique(gamedata$Level)),
                      selected = sort(unique(gamedata$Level))[c(1)])
  })
  
  
  #Visualization Output
  output$Plot <- renderPlot({
    
    #Requiring groupID input 
    req(input$groupID)
    
    #Using Reactive Data
    plotData <- plotDataR()
    
   
    if(input$facet != "None"){
    vec <- c(input$xvar, input$facet)
    visual_data <- plotData %>%
      group_by_at(vec) %>%
      summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
      gather("Destroyed", "Missed", key = "Indicator", value = "ShotDestroyed")
    
    } else {
      
      visual_data <- plotData %>%
        group_by_at(input$xvar) %>%
        summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
        gather("Destroyed", "Missed", key = "Indicator", value = "ShotDestroyed")
    }
    
    
    
    if(input$chart == "Counts" & input$facet == "None"){

      myplot <- ggplot(data = visual_data, aes_string(x = input$xvar, y = "ShotDestroyed")) +
        geom_bar(stat = "identity", aes(fill = Indicator)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Counts")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        scale_fill_brewer(palette = "Dark2")
      
    }
    
     if(input$chart == "Percent" & input$facet == "None"){

      total <- visual_data %>%
        group_by_at(input$xvar) %>%
        summarize(Total = sum(ShotDestroyed))

      visual_data_p <- inner_join(visual_data, total) %>%
        mutate(Percent = ShotDestroyed/Total)


      myplot <- ggplot(data = visual_data_p, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Percent")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        scale_fill_brewer(palette = "Dark2")
     
    }
    
    if(input$facet != "None" & input$chart == "Counts"){

      myplot <- ggplot(data = visual_data, aes_string(x = input$xvar, y = "ShotDestroyed")) +
        geom_bar(stat = "identity", aes(fill = Indicator)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Counts")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) + 
       facet_wrap(as.formula(paste("~", input$facet))) +
       theme(strip.text = element_text(size = 16)) +
        scale_fill_brewer(palette = "Dark2")
   
      }
    
    if(input$facet != "None" & input$chart == "Percent"){
      
      vec <- c(input$xvar, input$facet)
      
      total_f <- visual_data %>%
        group_by_at(vec) %>%
        summarize(Total = sum(ShotDestroyed))
      
      visual_data_f <- inner_join(visual_data, total_f) %>%
        mutate(Percent = ShotDestroyed/Total)
      
      myplot <- ggplot(data = visual_data_f, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Percent")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        facet_wrap(as.formula(paste("~", input$facet))) +
        theme(strip.text = element_text(size = 16)) +
        scale_fill_brewer(palette = "Dark2")
        
  
    }

    output$table = renderPrint({
      
      plotData <- plotDataR()
      
 
       if(input$ctest == "TRUE"){
         
         if(input$facet == "None"){
           
          output$header <- renderUI({
            h4("Observed Table:")
          }) 

         test_data <- plotData %>%
           group_by_at(input$xvar) %>%
           summarize(Destroyed = sum(Destroyed), Missed = sum(Missed))
          
         chisq.test(test_data[,2:3]) 
        
       } else {
        
         "Facet must be set to None to run the chi-squared test"
         
      }
      
    }
    })
    
    
    observeEvent(input$ctest, {
      
      if(input$ctest == "FALSE"){
        output$header <- renderUI({
         ""}) 
        
      }
    })

   
   return(myplot)
    

  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
      
    })
  
  
}


#Creating Shiny App
shinyApp(ui = ui, server = server)
