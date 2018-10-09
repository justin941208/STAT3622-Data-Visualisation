library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(dplyr)
library(streamgraph)
library(viridis)

hk_drug_map <- read.csv("hk_drug_map.csv")

tt1 = read.csv("Types2.csv")
all <- read.csv("All.csv")
gender <- read.csv("Gender.csv")
age <- read.csv("Age.csv")
first <- read.csv("First.csv")
edu <- read.csv("Edu.csv")
occup <- read.csv("Occup.csv")
multi <- read.csv("multidrug.csv")

tt2 = melt(tt1, id.vars = "Drugs")
allmelt <- melt(all, id.vars = "Year")
gendermelt <- melt(gender, id.vars = "Year")
agemelt <- melt(age, id.vars = "Year")
firstmelt <- melt(first, id.vars = "Year")
edumelt <- melt(edu, id.vars = "Year")
occupmelt <- melt(occup, id.vars = "Year")
multimelt <- melt(multi, id.vars = "Year")

colnames(tt2) <- c("Drug_Types", "Year", "Users")
colnames(allmelt) <- c("Total", "Year", "Abusers")
colnames(gendermelt) <- c("Gender", "Year", "Abusers")
colnames(agemelt) <- c("Age", "Year", "Abusers")
colnames(firstmelt) <- c("First", "Year", "Abusers")
colnames(edumelt) <- c("Edu", "Year", "Abusers")
colnames(occupmelt) <- c("Occup", "Year", "Abusers")
colnames(multimelt) <- c("multi", "Year", "Abusers")

tt2$Year <- gsub("X", "", tt2$Year)
allmelt$Year <- gsub("X", "", allmelt$Year)
gendermelt$Year <- gsub("X", "", gendermelt$Year)
agemelt$Year <- gsub("X", "", agemelt$Year)
firstmelt$Year <- gsub("X", "", firstmelt$Year)
edumelt$Year <- gsub("X", "", edumelt$Year)
occupmelt$Year <- gsub("X", "", occupmelt$Year)
multimelt$Year <- gsub("X", "", multimelt$Year)

ui <- shinyUI(navbarPage(theme = "sandstone",
                         "Visualizing Drug Abuse in Hong Kong",
                         tabPanel("Time Series Streamgraphs",
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("This panel generates a streamgraph based on your choice of a sorting method."),
                                      p(strong("Horizontal Axis:"), "Year"),
                                      p(strong("Vertical Axis:"), "Number of Drug Abusers"),
                                      br(),
                                      selectInput("type1", "Select Sorting Method:", 
                                                  c("All Drug Abusers", 
                                                    "Common Drug Types",
                                                    "Whether Taking More Than 1 Drug",
                                                    "Gender",
                                                    "Age", 
                                                    "Age of First Abuse", 
                                                    "Educational Attainment", 
                                                    "Activity Status"),
                                                  selected="All Drug Abusers"),
                                      br(),
                                      uiOutput("ui_3"),
                                      br(),
                                      p(strong("Note:")),
                                      p("(1) For the streamgraph of 'Common Drug Types', the height of the stream does not necessarily represent
                                        the total number of drug abusers, since some abusers take multiple drugs at the same time."),
                                      p("(2) For the streamgraph of 'Activity Status', the drug abusers were only divided into two groups before 1995, namely 
                                        'employed' and 'unemployed'. Since then, more subgroups have been added.")
                                    ),
                                    mainPanel(
                                      streamgraphOutput("stream", width = "100%", height = "600px")
                                    )
                                  )
                         ),
                         tabPanel("Geographical Map",
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("This panel generates a map showing the distribution of drug abusers based on your selections."),
                                      br(),
                                      selectInput("type2", "Select Sorting Method:", 
                                                  c("All Drug Abusers", 
                                                    "Gender",
                                                    "Age", 
                                                    "Most Common Drug Type"),
                                                  selected="All Drug Abusers"),
                                      uiOutput("ui_2"),
                                      br(),
                                      p(strong("Note:")),
                                      p("(1) The map only takes into account those who take residence in Hong Kong, as there might be reported drug abusers
                                        taking residence outside Hong Kong."),
                                      p("(2) 'Show by District Density' will divide the number of drug abusers in each district by the district population."),
                                      p("(3) 'Male to Female Log Ratio' is obtained by dividing the number of male drug abusers by that of the female ones, 
                                        then taking the natural logarithm (similar for the two age groups). The purpose of taking logarithm is to create a 
                                        fair scale for comparing both groups.")
                                    ),
                                    mainPanel(
                                      plotlyOutput("drugMap", width = "880px", height = "660px"),
                                      uiOutput("ui_1")
                                    )
                                  )
                         )
))

server <- shinyServer(function(input, output) {
  output$ui_1 <- renderUI({
    switch(input$type2,
           "All Drug Abusers" = sliderInput("yr1", "Year:", value=1996, min=1996, max=2015, sep="", width="800px", animate=animationOptions(interval=2500, loop=FALSE)),
           "Gender" = sliderInput("yr2", "Year:", value=1996, min=1996, max=2015, sep="", width="800px", animate=animationOptions(interval=2500, loop=FALSE)),
           "Age" = sliderInput("yr3", "Year:", value=1996, min=1996, max=2015, sep="", width="800px", animate=animationOptions(interval=2500, loop=FALSE)),
           "Most Common Drug Type" = sliderInput("yr4", "Year:", value=2006, min=2006, max=2014, sep="", width="800px", animate=animationOptions(interval=2500, loop=FALSE))
    )
  })
  
  output$ui_2 <- renderUI({
    switch(input$type2,
           "All Drug Abusers" = checkboxInput("density", "Show Density Instead", value = FALSE),
           "Gender" = radioButtons("gender", "Gender Options:", choices = c("Male", "Female", "Male to Female Log Ratio"), selected = "Male"),
           "Age" = radioButtons("age", "Age Options:", choices = c("Below 21", "21 or Above", "Below 21 / 21 or Above Log Ratio"), selected = "Below 21"),
           "Most Common Drug Type" = NULL
    )
  })
  
  output$ui_3 <- renderUI({
    switch(input$type1,
           "All Drug Abusers" = NULL,
           "Gender" = radioButtons("chart1", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Age" = radioButtons("chart2", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Age of First Abuse" = radioButtons("chart3", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Educational Attainment" = radioButtons("chart4", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Activity Status" = radioButtons("chart5", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Common Drug Types" = radioButtons("chart6", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle"),
           "Whether Taking More Than 1 Drug" = radioButtons("chart7", "Streamgraph Options:", c("Wiggle", "Area", "Percentage"), selected = "Wiggle")
    )
  })

  output$stream <- renderStreamgraph ({
    if (is.null(input$type1)) {return()}
      
    else if (input$type1 == "All Drug Abusers") {
        {streamgraph(allmelt, "Total", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual("black")
        }
    }
    
    else if (input$type1 == "Gender") {
      if (is.null(input$chart1)) {return()}
      
      else if (input$chart1 == "Wiggle")
      {streamgraph(gendermelt, "Gender", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("hotpink", "deepskyblue"))
      }
      
      else if (input$chart1 == "Area")
      {streamgraph(gendermelt, "Gender", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("hotpink", "deepskyblue"))
      }
      
      else if (input$chart1 == "Percentage")
      {streamgraph(gendermelt, "Gender", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("hotpink", "deepskyblue"))
      }
    }
    
    else if (input$type1 == "Age") {
      if (is.null(input$chart2)) {return()}
      
      else if (input$chart2 == "Wiggle")
      {streamgraph(agemelt, "Age", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(6))
      }
      
      else if (input$chart2 == "Area")
      {streamgraph(agemelt, "Age", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(6))
      }
      
      else if (input$chart2 == "Percentage")
      {streamgraph(agemelt, "Age", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(6))
      }
    }
    
    else if (input$type1 == "Age of First Abuse") {
      if (is.null(input$chart3)) {return()}
      
      else if (input$chart3 == "Wiggle")
      {streamgraph(firstmelt, "First", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(7))
      }
      
      else if (input$chart3 == "Area")
      {streamgraph(firstmelt, "First", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(7))
      }
      
      else if (input$chart3 == "Percentage")
      {streamgraph(firstmelt, "First", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(viridis(7))
      }
    }
    
    else if (input$type1 == "Educational Attainment") {
      if (is.null(input$chart4)) {return()}
      
      else if (input$chart4 == "Wiggle")
      {streamgraph(edumelt, "Edu", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(plasma(5))
      }
      
      else if (input$chart4 == "Area")
      {streamgraph(edumelt, "Edu", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(plasma(5))
      }
      
      else if (input$chart4 == "Percentage")
      {streamgraph(edumelt, "Edu", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(plasma(5))
      }
    }
    
    else if (input$type1 == "Activity Status") {
      if (is.null(input$chart5)) {return()}
      
      else if (input$chart5 == "Wiggle")
      {streamgraph(occupmelt, "Occup", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_tableau("tableau20")
      }
      
      else if (input$chart5 == "Area")
      {streamgraph(occupmelt, "Occup", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_tableau("tableau20")
      }
      
      else if (input$chart5 == "Percentage")
      {streamgraph(occupmelt, "Occup", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_tableau("tableau20")
      }
    }
        
    else if (input$type1 == "Common Drug Types") {
      if (is.null(input$chart6)) {return()}
      
      else if (input$chart6 == "Wiggle")
      {streamgraph(tt2, "Drug_Types", "Users", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(magma(11))
      }
      
      else if (input$chart6 == "Area")
      {streamgraph(tt2, "Drug_Types", "Users", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(magma(11))
      }
      
      else if (input$chart6 == "Percentage")
      {streamgraph(tt2, "Drug_Types", "Users", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(magma(11))
      }
    }
    
    else if (input$type1 == "Whether Taking More Than 1 Drug") {
      if (is.null(input$chart7)) {return()}
      
      else if (input$chart7 == "Wiggle")
      {streamgraph(multimelt, "multi", "Abusers", "Year", interpolate="cardinal", offset="wiggle") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("royalblue", "darkorange"))
      }
      
      else if (input$chart7 == "Area")
      {streamgraph(multimelt, "multi", "Abusers", "Year", interpolate="cardinal", offset="zero") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("royalblue", "darkorange"))
      }
      
      else if (input$chart7 == "Percentage")
      {streamgraph(multimelt, "multi", "Abusers", "Year", interpolate="cardinal", offset="expand") %>%
          sg_axis_x(2) %>%
          sg_fill_manual(c("royalblue", "darkorange"))
      }
    }
  })
  
  output$drugMap <- renderPlotly ({
    if (is.null(input$type2)) {return()}
    
    #Total Number
    
    else if (input$type2 == "All Drug Abusers") {
      if (is.null(input$density)) {return()}
      
      else if (input$density == FALSE) 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, text=paste("District: ", District, "</br>Reported Drug Abusers: ", hk_drug_map[,input$yr1-1989]))) +
           geom_polygon(aes(fill=hk_drug_map[,input$yr1-1989])) + 
           scale_fill_gradient(name="", limits=c(0, 2300), low="springgreen", high="black") + 
           ggtitle("Total Number of Reported Drug Abusers by District") + 
           xlab("") + 
           ylab(""), tooltip='text')}
    
      else if (input$density == TRUE) 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr1-1969], text=paste("District: ", District, "</br>Percentage of Drug Abusers:", hk_drug_map[,input$yr1-1969]))) +
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(0, 0.65), low="springgreen", high="black") + 
          ggtitle("Percentage of Reported Drug Abusers in the Total Population by District (%)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    }

    # Gender
    
    else if (input$type2 == "Gender") {
      if (is.null(input$gender)) {return()}
      
      else if (input$gender == "Male") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr2-1949], text=paste("District: ", District, "</br>Percentage of Male Drug Abusers: ", hk_drug_map[,input$yr2-1949]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(70, 95), low="white", high="royalblue") + 
          ggtitle("Percentage of Males in All Reported Drug Abusers by District (%)") + 
          xlab("") + 
          ylab(""), tooltip='text')}
    
      else if (input$gender == "Female") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr2-1929], text=paste("District: ", District, "</br>Percentage drug abuser proportion: ", hk_drug_map[,input$yr2-1929]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(5, 30), low="white", high="darkorange") + 
          ggtitle("Percentage of Females in All Reported Drug Abusers by District (%)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    
      else if (input$gender == "Male to Female Log Ratio") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr2-1909], text=paste("District: ", District, "</br>M/F Log Ratio: ", hk_drug_map[,input$yr2-1909]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(0, 2.82), low="red", high="black") + 
          ggtitle("Log Ratio of Male to Female Drug Abusers by District</br>(High: More Males; Low: More Females)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    }
    
    #Age
    
    else if (input$type2 == "Age") {
      if (is.null(input$age)) {return()}
      
      else if (input$age == "Below 21") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr3-1889], text=paste("District: ", District, "</br>Below 21 Percentage: ", hk_drug_map[,input$yr3-1889]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(0, 50), low="white", high="royalblue") + 
          ggtitle("Percentage of Drug Abusers Aged Below 21 in All Reported Drug Abusers by District (%)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    
      else if (input$age == "21 or Above") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr3-1869], text=paste("District: ", District, "</br>21 or Above Percentage: ", hk_drug_map[,input$yr3-1869]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(50, 98), low="white", high="darkorange") + 
          ggtitle("Percentage of Drug Abusers Aged 21 or Above in All Reported Drug Abusers by District (%)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    
      else if (input$age == "Below 21 / 21 or Above Log Ratio") 
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr3-1849], text=paste("District: ", District, "</br>Below 21 to 21 or Above Log Ratio: ", hk_drug_map[,input$yr3-1849]))) + 
          geom_polygon() + 
          scale_fill_gradient(name="", limits=c(-3.7, 1), low="black", high="chartreuse") + 
          ggtitle("Log Ratio of Drug Abusers Aged Below 21 to 21 or Above by District</br>(High: More Younger Abusers; Low: More Older Abusers)") + 
          xlab("") + 
          ylab(""), tooltip="text")}
    }
    
    else if (input$type2 == "Most Common Drug Type")
        {ggplotly(ggplot(hk_drug_map, aes(long, lat, group=group, fill=hk_drug_map[,input$yr4-1839], text=paste("District: ", District, "</br>Most Common Drug: ", hk_drug_map[,input$yr4-1839]))) + 
          geom_polygon() + 
          scale_fill_manual(name="Drug", values=c("#F8766D", "#00BA38", "#619CFF")) + 
          ggtitle("Most Common Drug Types among Reported Drug Abusers by District") + 
          xlab("") + 
          ylab(""), tooltip="text")}
  })
})

shinyApp(ui = ui, server = server)