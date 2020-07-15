#Last Updated on July 15 2020

#Loading Packages
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gdata)
library(stats)
library(curl)
 
#Importing Data
n <- sample(c(0,1), size = 1)

if(n == 0){
  data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 

} else{
  data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 
}


#Filter by Level
data.all <- data.all %>% filter(Level > 0)

#To Lower
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)

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
                  choices =  c(all_groups),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "sta310"),
      
      selectInput(inputId = "playerID",
                  label = "Player ID:",
                  choices =  c("all", all_players),
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "all"),
      
      selectInput("levels", "Level:",
                  choices = c(1, 2, 3, 4, 5),
                  multiple = TRUE,
                  selected = "1"),
      
      selectInput(inputId = "chart",
                  label = "Chart Type:",
                  #columns of the dataset
                  choices = c("Counts","Percent"),
                  selected = "Counts",
                  multiple = FALSE),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  #columns of the dataset
                  choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "Medicine",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by:",
                  choices = c("None", "Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "tests",
                  label = "Statistic Tests:",
                  choices = c("None", "Chi-Sq Test", "Two Proportion Z-Test"),
                  selected = "None",
                  multiple = FALSE),
  
      checkboxInput("summary", "Show Summary Statistics (For X Variable)", FALSE),
      
      downloadButton('downloadData', label = "Defenders Data"),
      
      a(h5("Instructor Details"),
        href="https://stat2labs.sites.grinnell.edu/defenders.html", 
        align="left", target = "_blank")
      
      
    ),
    
    #Outputs
    mainPanel(
      plotOutput(outputId = "Plot"),
      uiOutput("header"),
      verbatimTextOutput("chi"),
      verbatimTextOutput("prop"),
      tableOutput("summarytable")
    
   
     
    )
  )
)


#Server
server <- function(input, output,session) {
  
  #Reactive Data
  plotDataR <- reactive({
   
      if("all" %in% input$playerID){
        data <- data.all %>% filter(Level %in% input$levels, GroupID %in% input$groupID)
        
      } else {
        data <- data.all %>% filter(Level %in% input$levels, PlayerID %in% input$playerID, GroupID %in% input$groupID)
      }
    
    return(data)
  })
  
  
  
  # Dynamic PlayerID Input
  observe({
    
    # req() requires a selection from GroupID before any output
    # or reactivity occurs (keeps the app from crashing)
    req(input$groupID)   
    
    gamedata <- filter(data.all, GroupID %in% input$groupID)
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  })
  
  
  # Dynamic Level Input 
  observe({
    req(input$groupID)   
    
   
    gamedata <- filter(data.all, GroupID %in% input$groupID)
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
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Counts")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_blank(), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        scale_fill_manual(values = c("Destroyed" = "steelblue2", "Missed" = "snow3")) +
        guides(fill = guide_legend(reverse = TRUE))
 
      
    }
    
     if(input$chart == "Percent" & input$facet == "None"){

      total <- visual_data %>%
        group_by_at(input$xvar) %>%
        summarize(Total = sum(ShotDestroyed))

      visual_data_p <- inner_join(visual_data, total) %>%
        mutate(Percent = ShotDestroyed/Total)


      myplot <- ggplot(data = visual_data_p, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Percent")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_blank(), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        scale_fill_manual(values = c("Destroyed" = "steelblue2", "Missed" = "snow3")) +
        guides(fill = guide_legend(reverse = TRUE))
     
    }
    
    if(input$facet != "None" & input$chart == "Counts"){

      myplot <- ggplot(data = visual_data, aes_string(x = input$xvar, y = "ShotDestroyed")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Counts")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_blank(), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) + 
       facet_wrap(as.formula(paste("~", input$facet))) +
       theme(strip.text = element_text(size = 16)) +
        scale_fill_manual(values = c("Destroyed" = "steelblue2", "Missed" = "snow3")) +
        guides(fill = guide_legend(reverse = TRUE))
   
      }
    
    if(input$facet != "None" & input$chart == "Percent"){
      
      vec <- c(input$xvar, input$facet)
      
      total_f <- visual_data %>%
        group_by_at(vec) %>%
        summarize(Total = sum(ShotDestroyed))
      
      visual_data_f <- inner_join(visual_data, total_f) %>%
        mutate(Percent = ShotDestroyed/Total)
      
      myplot <- ggplot(data = visual_data_f, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Shot vs Destroyed by",input$xvar, "in Percent")) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 18, angle = 40, hjust = 1), 
              axis.title = element_text(size = 20), 
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              legend.title = element_blank(), 
              legend.text = element_text(size = 16), 
              axis.text.y = element_text(size = 14)) +
        facet_wrap(as.formula(paste("~", input$facet))) +
        theme(strip.text = element_text(size = 16)) +
        scale_fill_manual(values = c("Destroyed" = "steelblue2", "Missed" = "snow3")) +
        guides(fill = guide_legend(reverse = TRUE))
        
  
    }

    output$chi = renderPrint({
      
      plotData <- plotDataR()
      
 
       if(input$tests == "Chi-Sq Test"){
         
         if(input$facet == "None"){
           
          output$header <- renderUI({
            h4("Observed Table:")
          }) 

         test_data <- plotData %>%
           group_by_at(input$xvar) %>%
           summarize(Destroyed = sum(Destroyed), Missed = sum(Missed))
          
         chisq.test(test_data[,2:3], correct = FALSE) 
        
       } else {
        
         "Facet must be set to None to run the chi-squared test"
         
      }
      
    }
    })
    
    #Removing Header if chi-squared/proportions test checkbox is deselected
    observeEvent(input$tests, {
      
      if(input$tests == "None"){
        output$header <- renderUI({
         ""}) 

      }
    })
    
    
    #Two proportion Z test
    output$prop <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #Setting Up
      XVariable <- plotData %>% pull(input$xvar)
      XVariable <- drop.levels(as.factor(XVariable))
      xlevel <- nlevels(XVariable)
      
      if(input$tests == "Two Proportion Z-Test"){
        
        if(input$facet == "None"){
          
          if(xlevel == 2){
          
          output$header <- renderUI({
            h4("Observed Table:")
          }) 
          
          test_data <- plotData %>%
            group_by_at(input$xvar) %>%
            summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
            mutate(Total = Destroyed + Missed)
          
          Destroyed <- test_data$Destroyed
          Total <- test_data$Total
          
          prop.test(Destroyed, Total, correct = FALSE)
          
          } else{
            
            "X Variable must have exactly two levels to run the two proportion z-test."
          }
        
        } else {
          
          "Facet must be set to None to run the two proportion z-test"
          
        }
      }
      
    })
    
    
    
    
    
    #Summary table
    output$summarytable <- renderTable({
      
      if(input$summary == "TRUE"){
      
      if(input$facet != "None"){
        vec <- c(input$xvar, input$facet)
        visual_data <- plotData %>%
          group_by_at(vec) %>%
          summarize(Destroyed = as.integer(sum(Destroyed)), Missed = as.integer(sum(Missed)))
        
        
      } else {
        visual_data <- plotData %>%
          group_by_at(input$xvar) %>%
          summarize(Destroyed = as.integer(sum(Destroyed)), Missed = as.integer(sum(Missed)))
      }
      
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
