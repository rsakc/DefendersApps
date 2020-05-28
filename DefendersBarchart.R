library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)

###Defenders DATA####
###
data.all <-read.csv("https://www.stat2games.sites.grinnell.edu/data/defenders/getdata.php") 
#data.all <- separate(data = data.all, col = TurretType, into = c("Turret", "Upgrade"), sep = "LV")
data.all <- filter(data.all, Level > 0)
data.all$Level <- as.factor(data.all$Level)
data.all$Round <- as.factor(data.all$Wave)
data.all$Location <- as.factor(data.all$Location)
data.all$GroupID <- as.character(data.all$GroupID)
data.all$PlayerID <- as.character(data.all$PlayerID)
data.all["None"] = NA

#data.all <- filter(data.all, Medicine == "red" | Medicine == "blue")
#data.all <- filter(data.all, Shot > 0)
data.all <- mutate(data.all, PercDestroyed = Destroyed/Shot)
data.all <- mutate(data.all, Missed = Shot - Destroyed)
###
data.all <- mutate(data.all, PerMissed = (Shot - Destroyed)/Shot)
data.all <- mutate(data.all, PerDefective = Destroyed/Shot)
data.all <- gather(data.all,"Result","Value", PerDefective,PerMissed)
data.all$Result[data.all$Result == "PerDefective"] <- "Destroyed" 
data.all$Result[data.all$Result == "PerMissed"] <- "Missed" 

summed1 = sum(data.all$Shot)
summed2 = sum(data.all$Missed)
data.all$new = c(summed1, summed2)
require(dplyr)
require(gdata)
###


all_groups <- sort(unique(data.all$GroupID))
all_players <- sort(unique(data.all$PlayerID))


ui <- fluidPage(
    # App title ----
    titlePanel("Defenders Data Visualizations"),
    
    sidebarLayout(
        sidebarPanel(
         ###   
            
        ###    
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
                        choices = c(
                                    "Counts",
                                    "Percent"),
                        selected = "Dotplot",
                        multiple = FALSE),
            
            selectInput(inputId = "xvar",
                        label = "X Axis:",
                        #columns of the dataset
                        choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                        selected = "Medicine",
                        multiple = FALSE),
            
        
           
            
            selectInput(inputId = "color",
                        label = "Facet by",
                        choices = c("Level", "Round", "Location", "TurretType", "Upgrade", "Medicine", "Virus"),
                        selected = "Virus",
                        multiple = FALSE),
            
            checkboxInput('ctest',"Chi-Sq Test",FALSE),
            
            
            downloadButton('downloadData', label = "Defenders Data")
            
        ),
        
        mainPanel(
            
            plotOutput(outputId = "Plot"),
            verbatimTextOutput("test"),
            
            p(h4("Observed Table (Must Select Chi-sq test):")),
            verbatimTextOutput("table")
        )
    )
)

server <- function(input, output,session) {
    
    # Updates PlayerID based upon GroupID
    observe({
        # req() requires a selection from GroupID before any output
        # or reactivity occurs (keeps the app from crashing)
        req(input$groupID)   
        
        if ("all" %in% input$groupID) {gamedata <- data.all}
        else{gamedata <- filter(data.all, GroupID == input$groupID)}
        
        updateSelectInput(session, 
                          "playerID",
                          choices = c("all", sort(unique(gamedata$PlayerID))),
                          selected = "all")
    })
    
    # Updates Levels based upon GroupID
    observe({
        req(input$groupID)   
        
        if ("all" %in% input$groupID) {gamedata <- data.all}
        else{gamedata <- filter(data.all, GroupID == input$groupID)}
        updateSelectInput(session, 
                          "levels",
                          choices = sort(unique(gamedata$Level)),
                          selected = sort(unique(gamedata$Level))[c(1)])
    })
    
    
    output$Plot <- renderPlot({
        req(input$groupID)
        data.all <- data.all[data.all$Level %in% input$levels, ]
        
        if ("all" %in% input$groupID) {plotData <- data.all}
        else{
            if("all" %in% input$playerID) {plotData <- data.all[data.all$GroupID %in% input$groupID, ]}
            else{
                plotData <- data.all[data.all$GroupID %in% input$groupID, ]
                plotData = plotData[plotData$PlayerID %in% input$playerID, ]
            }
        }
        
        

        
        
        if(input$chart == "Percent"){
            DefData1 = plotData
            DefData2 <- mutate(DefData1, PerDefective = Destroyed/Shot)
            DefData2 <- mutate(DefData2, PerMissed = (Shot - Destroyed)/Shot)
            
            Def.Sum2 <- gather(DefData2,"Result","Value", PerDefective,PerMissed)
            Def.Sum2$Result[Def.Sum2$Result == "PerDefective"] <- "Destroyed" 
            Def.Sum2$Result[Def.Sum2$Result == "PerMissed"] <- "Missed" 
            
                myplot = ggplot() + geom_bar(aes(y = Value, x = eval(parse(text = input$xvar)), fill = forcats::fct_rev(Result)), data = Def.Sum2, position = "fill", stat = "identity") + 
                    ggtitle("Chart of Percent Destroyed") + ylab("Percent") + scale_fill_discrete(name = "Result") + 
                    scale_y_continuous(labels = scales::percent) + facet_wrap(c(input$color)) + xlab(input$xvar)
            
            
        } 
        
        
        if(input$chart == "Counts"){
            DefData1 = plotData
            DefData2 <- mutate(DefData1, PerDefective = Destroyed/Shot)
            DefData2 <- mutate(DefData2, PerMissed = (Shot - Destroyed)/Shot)
            DefData2 <- mutate(DefData2, Missed = (Shot - Destroyed))
            
            Def.Sum2 <- gather(DefData2,"Result","Value", Destroyed,Missed)
            myplot = ggplot(Def.Sum2, aes(y = Value, x = eval(parse(text = input$xvar)), fill = forcats::fct_rev(Result))) + 
                geom_bar(stat = "summary", fun.y = "sum") + 
                facet_wrap(c("Medicine")) + scale_fill_discrete(name = "Result") + 
                ylab("Counts") + ggtitle("Chart of Counts Destroyed") + facet_wrap(c(input$color)) + xlab(input$xvar)
        } 
        
        
        output$table = renderPrint( {
            ##  YVariable = plotData %>% pull(input$yvar)
            XVariable = plotData %>% pull(input$xvar)
            ColorVariable = plotData %>% pull(input$color)
            ## FacetVariable = plotData %>% pull(input$facets)
            if(input$ctest == "TRUE"){
                X_Variable = drop.levels(XVariable)
                Color_Variable = drop.levels(ColorVariable)
                table(X_Variable, Color_Variable)
                DefData1 = plotData
                test2 <- mutate(DefData1, PerDefective = Destroyed/Shot)
                test2 <- mutate(DefData2, PerMissed = (Shot - Destroyed)/Shot)
                test2 <- mutate(DefData2, Missed = (Shot - Destroyed))
                test3 = test2[,c(input$xvar,"Missed", "Destroyed")]
                string = input$xvar
                Data = aggregate(cbind(test3$Missed, test3$Destroyed), by=list(Category=test3[[string]]), FUN = sum )
                chisq.test(Data[,2:3])
            }
            
        })
        
        myplot
        
    })
    
   
   
     plotData <- reactive({
        if ("all" %in% input$groupID) {filter(data.all, Level %in% input$levels)}
        else{
            if("all" %in% input$playerID)
            {filter(data.all, GroupID %in% input$groupID, Level %in% input$levels)}
            else{filter(data.all, GroupID %in% input$groupID, PlayerID %in% input$playerID, Level %in% input$levels)}
        }
    })    
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('Data-', Sys.Date(), '.csv', sep="")
        },
        content = function(con) {
            write.csv(plotData(), con)
            
        })
    
}

shinyApp(ui = ui, server = server)

