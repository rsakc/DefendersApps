#Last Updated on July 21 2020

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
  
#Importing Data (Sampling to ensure that new data loads into the app quickly)
n <- sample(c(0,1), size = 1)

if(n == 0){
  data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 

} else{
  data.all <- readr::read_csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 
}

#Filter by Level
data.all <- data.all %>% filter(Level > 0)

#Converting PlayerID and GroupID to lower case
data.all$PlayerID <- tolower(data.all$PlayerID)
data.all$GroupID <- tolower(data.all$GroupID)

#Convering Certain Columns to Factors
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

#Vectors to use when creating inputs in UI
all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))
all_levels <- sort(unique(data.all$Level))



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
                  choices = all_levels,
                  multiple = TRUE,
                  selected = all_levels[1]),
      
      selectInput(inputId = "chart",
                  label = "Chart Type:",
                  choices = c("Counts","Percent"),
                  selected = "Counts",
                  multiple = FALSE),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "Medicine",
                  multiple = FALSE),
      
      selectInput(inputId = "facet",
                  label = "Facet by:",
                  choices = c("None", "Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                  selected = "None",
                  multiple = FALSE),
      
      selectInput(inputId = "tests",
                  label = "Statistical Tests:",
                  choices = c("None", "Chi-Sq Test", "Two Proportion Z-Test",
                              "Randomization Test"),
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
      
      #Visual Output
      plotOutput(outputId = "Plot"),
      #Dynamic Header
      uiOutput("header"),
      #Chi-Squared Test
      verbatimTextOutput("chi"),
      #Two Proportion Z Test
      verbatimTextOutput("prop"),
      #Randomization Test
      verbatimTextOutput("rt"),
      #Summary Table
      tableOutput("summarytable"))
                
    ))
  

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
  
  
  
  #Dynamic PlayerID Input
  observe({
    gamedata <- filter(data.all, GroupID %in% input$groupID)
    
    updateSelectInput(session, 
                      "playerID",
                      choices = c("all", sort(unique(gamedata$PlayerID))),
                      selected = "all")
  })
  
  
  #Dynamic Level Input 
  observe({
    
    #If all is selected within the PlayerID input
    if("all" %in% input$playerID){
      gamedata <- filter(data.all, GroupID %in% input$groupID)
    
    #If all is not selected within the PlayerID input
    } else{
      gamedata <- filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID)
    }
 
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
    
    #We need data to run
    if(nrow(plotData) > 0){
    
   
    #If facet option is not None
    #Creating data to use for vizualization
    if(input$facet != "None"){
    vec <- c(input$xvar, input$facet)
    visual_data <- plotData %>%
      group_by_at(vec) %>%
      summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
      gather("Destroyed", "Missed", key = "Indicator", value = "ShotDestroyed")
    
    #If facet option is None
    #Creating data to use for vizualization
    } else {
      visual_data <- plotData %>%
        group_by_at(input$xvar) %>%
        summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
        gather("Destroyed", "Missed", key = "Indicator", value = "ShotDestroyed")
    }
    
    
    #Visual for Counts and No Facet
    if(input$chart == "Counts" & input$facet == "None"){

      #Creating visualization
      myplot <- ggplot(data = visual_data, aes_string(x = input$xvar, y = "ShotDestroyed")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Missed and Destroyed by",input$xvar, "in Counts")) +
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
    
    #Visual for Percent and No Facet
     if(input$chart == "Percent" & input$facet == "None"){

      #Creating additional columns to use for vizualization
      total <- visual_data %>%
        group_by_at(input$xvar) %>%
        summarize(Total = sum(ShotDestroyed))

      visual_data_p <- inner_join(visual_data, total) %>%
        mutate(Percent = ShotDestroyed/Total)

      #Creating visualization
      myplot <- ggplot(data = visual_data_p, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Missed and Destroyed by",input$xvar, "in Percent")) +
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
    
    #Visual for Counts and Facet option is selected
    if(input$facet != "None" & input$chart == "Counts"){

      #Creating visualization
      myplot <- ggplot(data = visual_data, aes_string(x = input$xvar, y = "ShotDestroyed")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        labs(x = input$xvar, y = "Counts", title = paste("Plot of Missed and Destroyed by",input$xvar, "and Faceted by", input$facet,"in Counts")) +
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
    
    #Visual for Percent and Facet option is selected
    if(input$facet != "None" & input$chart == "Percent"){
      
      #Creating additional columns to use for vizualization
      vec <- c(input$xvar, input$facet)
      
      total_f <- visual_data %>%
        group_by_at(vec) %>%
        summarize(Total = sum(ShotDestroyed))
      
      visual_data_f <- inner_join(visual_data, total_f) %>%
        mutate(Percent = ShotDestroyed/Total)
      
      #Creating visualization
      myplot <- ggplot(data = visual_data_f, aes_string(x = input$xvar, y = "Percent")) +
        geom_bar(stat = "identity", aes(fill = Indicator), position = position_stack(reverse = TRUE)) +
        scale_y_continuous(labels = scales::percent) +
        labs(x = input$xvar, y = "Percent", title = paste("Plot of Missed and Destroyed by",input$xvar, "and Faceted by", input$facet, "in Percent")) +
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

      
    ##Chi Squared Test
    output$chi = renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data to run
      if(nrow(plotData) > 0){
      
       #Test option is chi-squared test
       if(input$tests == "Chi-Sq Test"){
         
         #If Facet option is none
         if(input$facet == "None"){
           
          #Creating header 
          output$header <- renderUI({
            h4("Observed Table:")
          }) 

         #Creating data to use for the test
         test_data <- plotData %>%
           group_by_at(input$xvar) %>%
           summarize(Destroyed = sum(Destroyed), Missed = sum(Missed))
          
         #Running chi-squared test
         chisq.test(test_data[,2:3], correct = FALSE) 
        
       } else {
         "Facet must be set to None to run the chi-squared test"
       }
       }
      }
    })
    
    
    #Removing Header when appropriate
    observeEvent(c(input$tests, input$groupID, input$playerID, input$levels), {
      
      #Reactive Data
      plotData <- plotDataR()
      
      #When test option is none
      if(input$tests == "None"){
        output$header <- renderUI({
         ""}) 
      }
      
      #When there is no data
      if(nrow(plotData) == 0){
        output$header <- renderUI({
          ""}) 
      }
    })
    
    
    ##Two proportion Z test
    output$prop <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data to run
      if(nrow(plotData) > 0){
      
      #Setting Up
      XVariable <- plotData %>% pull(input$xvar)
      XVariable <- drop.levels(as.factor(XVariable))
      xlevel <- nlevels(XVariable)
      
      #Test option is two proportion z-test
      if(input$tests == "Two Proportion Z-Test"){
        
        #Facet option is none
        if(input$facet == "None"){
          
          #There are exactly 2 levels for the x variable
          if(xlevel == 2){
          
          #Creating header
          output$header <- renderUI({
            h4("Observed Table:")
          }) 
          
          #Creating data to use for the test
          test_data <- plotData %>%
            group_by_at(input$xvar) %>%
            summarize(Destroyed = sum(Destroyed), Missed = sum(Missed)) %>%
            mutate(Total = Destroyed + Missed)
          
          Destroyed <- test_data$Destroyed
          Total <- test_data$Total
          
          #Running the two proportion z test
          prop.test(Destroyed, Total, correct = FALSE)
          
          } else{
            "X Variable must have exactly two levels to run the two proportion z-test."
          }
        
        } else {
          "Facet must be set to None to run the two proportion z-test"
          
        }
      }
     }
  })
    
    
    ##Randomization Test
    output$rt <- renderPrint({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data to run
      if(nrow(plotData) > 0){
        
        #Setting Up
        XVariable <- plotData %>% pull(input$xvar)
        XVariable <- drop.levels(as.factor(XVariable))
        xlevel <- nlevels(XVariable)
        
        #Test option is the randomization test
        if(input$tests == "Randomization Test"){
          
          #Facet option is none
          if(input$facet == "None"){
            
            #There is more than 1 level for the x variable
            if(xlevel > 1){
            
              #Creating header
              output$header <- renderUI({
                h4("Observed Table:")
              }) 
              
              #Test data to use for the randomization test
              test_data <- plotData %>%
                group_by_at(input$xvar) %>%
                summarize(Destroyed = as.integer(sum(Destroyed)), Missed = as.integer(sum(Missed))) %>%
                mutate(Total = Destroyed + Missed) %>%
                mutate(ProportionD = Destroyed/Total)
              
              #Vector for total counts of destroyed and missed
              D <- rep("D", times = sum(test_data$Destroyed))
              M <- rep("M", times = sum(test_data$Missed))
              
              #All counts vector
              DM <- c(D, M)
            
              #Running the randomization test
              
              ##Setting up
              
              #Test statistic
              maxdiff <- max(test_data$ProportionD) - min(test_data$ProportionD)
              #Number of replications
              R <- 10000
              #Vector to store results
              results <- numeric()
              
              #Repeating the process R times
              for(i in 1:R){
                
                #Permuting the individual observations of Destroyed and Misses
                samp <- sample(DM, size = length(DM), replace = FALSE)
                
                #Vector to store proportions temporarily
                proportions <- numeric()
                
                #Going through each row in the data to use for the randomization test
                for(j in 1:nrow(test_data)){
                  
                  #For first group
                  if(j == 1){
                    
                    #Getting observations for first group
                    observations <- samp[1 : test_data$Total[j]]
                    
                    #Storing the percent destroyed for first group
                    proportions[j] <- sum(observations == "D") / length(observations)
                    
                    #Sum to use for splitting data for remaining groups
                    sum <- test_data$Total[j]
                    
                  #For remaining groups
                  } else{
                
                  #Starting index
                  first <- (sum + 1)
                  
                  #Ending index
                  second <- (first - 1 + test_data$Total[j])
                  
                  #Getting observations for the particular group
                  observations <- samp[first:second]
                  
                  #Storing the percent destroyed for the particular group
                  proportions[j] <- sum(observations == "D") / length(observations)
              
                  #Updating sum to use for splitting data for remaining groups
                  sum <- sum + test_data$Total[j]
                  }
                }
            
                #Calculating max difference in proportions for the replication and storing result
                results[i] <- max(proportions) - min(proportions)
              }
              
              #P Value
              pvalue <- (1 + sum(results >= abs(maxdiff)) + sum(results <= -abs(maxdiff))) / (R + 1)
              
              #Returning P Value
              return(paste("P Value:", round(pvalue, 4)))
              
            } else{
              "At least 2 levels are needed to run the randomization test."
            }    
              
          } else{
            "Facet must be set to None to run the randomization test."
          }
     
        }
      }
    })
    
    
    #Summary table
    output$summarytable <- renderTable({
      
      #Reactive Data
      plotData <- plotDataR()
      
      #We need data to run
      if(nrow(plotData) > 0){
      
      #Checkbox for summary table is selected
      if(input$summary == "TRUE"){
      
      #If facet option is selected
      if(input$facet != "None"){
        vec <- c(input$xvar, input$facet)
        visual_data <- plotData %>%
          group_by_at(vec) %>%
          summarize(Destroyed = as.integer(sum(Destroyed)), Missed = as.integer(sum(Missed)))
        
      #If facet option is None 
      } else {
        visual_data <- plotData %>%
          group_by_at(input$xvar) %>%
          summarize(Destroyed = as.integer(sum(Destroyed)), Missed = as.integer(sum(Missed)))
      }
    }
   }
  })

   #Returning visualization
   return(myplot)
    }
  })
   
  #Download data 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(plotDataR(), con)
      
    })
    
#Closes server
}

#Creating Shiny App
shinyApp(ui = ui, server = server)