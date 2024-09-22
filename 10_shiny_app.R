pacman::p_load(
  xgboost, 
  SHAPforxgboost, 
  data.table, 
  caret, 
  vip, 
  sf, 
  tidyverse, 
  table1, 
  ggcorrplot, 
  gridExtra, 
  leaflet, 
  randomForest, 
  glmnet, 
  plotly, 
  shiny,
  shinythemes
)
# Load shinydata
# Request for .Rds files
shinydata <- readRDS("mapdata.Rds")
shinydata2 <- readRDS("summary_data_melted")

# Calculate averages for zinc
average_znwv4 <- mean(shinydata$Predictionsznwv4, na.rm = TRUE)
shinydata<- shinydata %>%
  mutate(color_znwv4 = ifelse(Predictionsznwv4 > average_znwv4, "red", "green"))


#My UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  
  tags$head(
    tags$style(HTML("
      .title-panel {
        text-align: center;
      }
    "))
  ),
  titlePanel(
    div(class = "title-panel", "MAP OF TANZANIA SHOWING PERCENTAGE RISK OF INADEQUATE MICRONUTRIENT INTAKE")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_selector", "Select Micronutrient:", 
                  choices = c("znadq", "Predictionsznwv4", "foladq", "Predictionsflwv4"), 
                  selected = "znadq"),
      plotOutput("histogram"),
      imageOutput("photo", height = "650px")  
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", width = "100%", height = "800px") 
        ),
        tabPanel("Household Analysis",
                 plotOutput("household_plot", width = "100%", height = "800px")
        ),
        tabPanel("National Average",
                 plotOutput("national_average_plot", width = "100%", height = "800px")
        ),
        tabPanel("Download Data",
                 downloadButton("download_data", "Download Data")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    fill_variable <- input$fill_selector
    
    pal <- colorNumeric(palette = "Reds", domain = shinydata[[fill_variable]])
    
    leaflet(shinydata) %>%
      addPolygons(
        fillColor = ~pal(shinydata[[fill_variable]]),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        label = ~ADM1_EN,  
        popup = ~paste("Region: ", ADM1_EN, "<br>",
                       fill_variable, ": ", shinydata[[fill_variable]])
      ) %>%
      addLegend("bottomright", pal = pal, values = ~shinydata[[fill_variable]],
                title = input$fill_selector,
                opacity = 1)
  })
  
  # Generate the ggplot plot
  output$household_plot <- renderPlot({
    ggplot(shinydata2, aes(x = region, y = count, fill = status)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c("Adequate" = "green", "Inadequate" = "red")) +
      labs(y = "Number of Households", x = "Regions", fill = "Status") +
      ggtitle("Predicted Folate Intake Status by Regions in Tanzania (wave 4)") +
      theme_minimal() +   
      theme(
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),   
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.background = element_rect(fill = "white")  
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  })
  
  output$national_average_plot <- renderPlot({
    ggplot(shinydata, aes(x = ADM1_EN, y = Predictionsznwv4, color = color_znwv4)) +
      geom_point(size = 3) +
      geom_hline(yintercept = average_znwv4, linetype = "dashed", color = "red") +
      labs(x = "Regions", y = "Percentage Inadequacy", title = "Tanzania WV4 Zinc Inadequacy / National Average") +
      scale_color_manual(values = c("red" = "red", "green" = "green"), guide = "none") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA)
      )
  })
  
  output$histogram <- renderPlot({
    fill_variable <- input$fill_selector
    hist(shinydata[[fill_variable]], 
         main = paste("Distribution of", fill_variable), 
         xlab = fill_variable,
         col = "lightblue",
         border = "black")
  })
  
  output$photo <- renderImage({
    list(
      src = "logo.png",  
      contentType = "image/png",
      width = 528,
      height = 348
    )
  }, deleteFile = FALSE)
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("micronutrient_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(shinydata, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)




####NA hover 
ui <- fluidPage(
  theme = shinytheme("cerulean"), 
  tags$head(
    tags$style(HTML("
      .title-panel {
        text-align: center;
      }
    "))
  ),
  titlePanel(
    div(class = "title-panel", "MAP OF TANZANIA SHOWING PERCENTAGE RISK OF INADEQUATE MICRONUTRIENT INTAKE")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("fill_selector", "Select Micronutrient:", 
                  choices = c("znadq", "Predictionsznwv4", "foladq", "Predictionsflwv4"), 
                  selected = "znadq"),
      plotOutput("histogram"),
      imageOutput("photo", height = "650px")  
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", width = "100%", height = "800px") 
        ),
        tabPanel("Household Analysis",
                 plotOutput("household_plot", width = "100%", height = "800px")
        ),
        tabPanel("National Average",
                 plotlyOutput("national_average_plot", width = "100%", height = "800px")  
        ),
        tabPanel("Download Data",
                 downloadButton("download_data", "Download Data")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    fill_variable <- input$fill_selector
    
    pal <- colorNumeric(palette = "Reds", domain = shinydata[[fill_variable]])
    
    leaflet(shinydata) %>%
      addPolygons(
        fillColor = ~pal(shinydata[[fill_variable]]),
        fillOpacity = 0.7,
        weight = 1,
        color = "white",
        label = ~ADM1_EN,  
        popup = ~paste("Region: ", ADM1_EN, "<br>",
                       fill_variable, ": ", shinydata[[fill_variable]])
      ) %>%
      addLegend("bottomright", pal = pal, values = ~shinydata[[fill_variable]],
                title = input$fill_selector,
                opacity = 1)
  })
  
  output$household_plot <- renderPlot({
    ggplot(shinydata2, aes(x = region, y = count, fill = status)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = c("Adequate" = "green", "Inadequate" = "red")) +
      labs(y = "Number of Households", x = "Regions", fill = "Status") +
      ggtitle("Predicted Folate Intake Status by Regions in Tanzania (wave 4)") +
      theme_minimal() +   
      theme(
        panel.grid.major = element_blank(),   
        panel.grid.minor = element_blank(),   
        plot.title = element_text(hjust = 0.5), 
        panel.border = element_rect(colour = "black", fill = NA),
        plot.background = element_rect(fill = "white")  
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  })
  
  output$national_average_plot <- renderPlotly({
    p <- ggplot(shinydata, aes(x = ADM1_EN, y = Predictionsznwv4, color = color_znwv4)) +
      geom_point(size = 3) +
      geom_hline(yintercept = average_znwv4, linetype = "dashed", color = "red") +
      labs(x = "Regions", y = "Percentage Inadequacy", title = "Tanzania WV4 Zinc Inadequacy / National Average") +
      scale_color_manual(values = c("red" = "red", "green" = "green"), guide = "none") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA)
      )
    
    ggplotly(p) %>%
      layout(hoverlabel = list(bgcolor = "white")) 
  })
  
  output$histogram <- renderPlot({
    fill_variable <- input$fill_selector
    hist(shinydata[[fill_variable]], 
         main = paste("Distribution of", fill_variable), 
         xlab = fill_variable,
         col = "lightblue",
         border = "black")
  })
  
  output$photo <- renderImage({
    list(
      src = "logo.png",  
      contentType = "image/png",
      width = 528,
      height = 348
    )
  }, deleteFile = FALSE)
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("micronutrient_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(shinydata, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)