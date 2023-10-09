
version <- "v1.0.1"

#use these packages
library(shiny)
library(tidyverse)
library(readxl)
library(gt)
library(DescTools)

cons_clust <- function(x) {
  
  #clustering algorithm with area under the curve (auc)
  #DescTools::AUC() uses trapezoidal rule for estimating *total* auc
  #Estimating total auc is non-interactive over the entire line
  piv_data_clust <- expr(!!x) |> 
    group_by(name) |> 
    mutate(id = consecutive_id(value)) |> 
    filter(id != 1) |>                               
    mutate(
      auc = AUC(time, value, method = "trapezoid"),
      categ = if_else(value > threshold, 0, 1),      
      clust_id = consecutive_id(categ)               
    ) |> 
    group_by(name, categ, clust_id) |> 
    mutate(
      n = n()                                        
    ) |> 
    group_by(name, clust_id) |> 
    filter(categ == 1 & n > 1)                       
  
  #summary stats
  piv_data_sum <- piv_data_clust |> 
    group_by(name, clust_id) |> 
    reframe(
      clust_time = n*5,                              
    ) |> 
    distinct(name, clust_id, clust_time) |> 
    ungroup() |> 
    group_by(name) |> 
    mutate(
      total_time = sum(clust_time),                   
      largest_bout = max(clust_time)
    )
  
  total_bouts <- piv_data_clust |> 
    distinct(name, clust_id) |> 
    group_by(name) |> 
    mutate(
      total_bouts = n()
    ) |> 
    distinct(name, total_bouts)
  
  piv_data_sums <- left_join(piv_data_sum, total_bouts, by = join_by("name"))
  
  #combine clustering and summary for table output
  piv_data_comp <<- left_join(piv_data_clust, piv_data_sums, by = join_by("name", "clust_id")) |> 
    select(!c(categ, id)) |> 
    arrange(name, time)
}

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel(str_glue("Actigraphy data clustering {version}")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        fileInput("datafile", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        downloadButton("downloadData", "Download.csv"),
        tags$hr(),
        checkboxGroupInput("inCheckGroup1", "Set a custom threshold?"),
        tags$hr(),
        conditionalPanel(
          "input.inCheckGroup1.includes('1')",
          sliderInput(
            "threshold1", "Threshsold for 1", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('2')",
          sliderInput(
            "threshold2", "Threshsold for 2", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('3')",
          sliderInput(
            "threshold3", "Threshsold for 3", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('4')",
          sliderInput(
            "threshold4", "Threshsold for 4", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('5')",
          sliderInput(
            "threshold5", "Threshsold for 5", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('6')",
          sliderInput(
            "threshold6", "Threshsold for 6", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('7')",
          sliderInput(
            "threshold7", "Threshsold for 7", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('8')",
          sliderInput(
            "threshold8", "Threshsold for 8", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('9')",
          sliderInput(
            "threshold9", "Threshsold for 9", min = 0.01, max = 1, value = 0.1
          )
        ),
        conditionalPanel(
          "input.inCheckGroup1.includes('10')",
          sliderInput(
            "threshold10", "Threshsold for 10", min = 0.01, max = 1, value = 0.1
          )
        )
      ),
      mainPanel(
        plotOutput("plot"),
        gt_output("contents")
      )
    )
)

# Define server logic 
server <- function(input, output, session) {

  
  reactives <- reactiveValues(
    
    mydata = NULL
    
  )
  
  observe({
    if (is.null(input$datafile)) {
          load("ex_data.rdata")

          data <- ex_data

          piv_data <- pivot_longer(data,
                                   cols = names(data[,-1]),
                                   names_to = "name",
                                   cols_vary = "fastest")

          names(piv_data) <- c("time", "name", "value")

          #making a key is important for the JavaScript interpretation of the conditional panel
          piv_data_key <- piv_data |>
            distinct(name) |>
            mutate(name_id = as.character(1:length(name)))

          piv_data <- left_join(piv_data, piv_data_key, by = join_by(name))

          reactives$mydata <- piv_data

          #Update select input
            updateCheckboxGroupInput(
              session,
              inputId = 'inCheckGroup1',
              choices  =  unique(piv_data$name_id)
              )
    }
  })

  
  observeEvent(input$datafile, {

    #Process and store loaded data in reactive
    data <- read.csv(file = input$datafile$datapath)



    piv_data <- pivot_longer(data,
                             cols = names(data[,-1]),
                             names_to = "name",
                             cols_vary = "fastest")

    names(piv_data) <- c("time", "name", "value")

    #making a key is important for the JavaScript interpretation of the conditional panel
    piv_data_key <- piv_data |>
      distinct(name) |>
      mutate(name_id = as.character(1:length(name)))

    piv_data <- left_join(piv_data, piv_data_key, by = join_by(name))

    reactives$mydata <- piv_data

    #Update select input
    updateCheckboxGroupInput(
      session,
      inputId = 'inCheckGroup1',
      choices  =  unique(reactives$mydata$name_id)
      )

  })


   output$contents <- render_gt({
     df <- reactives$mydata
     
     if (length(input$inCheckGroup1) == 0) {
       
       piv_data <- df |> 
         group_by(name) |> 
         mutate(
           max = max(value),
           threshold = max*0.1 
         )
       
       cons_clust(piv_data)
       
     } else {
       
       piv_data <- df |> 
         group_by(name) |> 
         mutate(
           max = max(value) 
         )
       
       threshold_key <- tibble(prop = c(input$threshold1, input$threshold2, input$threshold3,
                                             input$threshold4, input$threshold5, input$threshold6,
                                             input$threshold7, input$threshold8, input$threshold9,
                                             input$threshold10),
                               name_id = c("1", "2", "3",
                                           "4", "5", "6",
                                           "7", "8", "9",
                                           "10"))
       
       piv_data <- left_join(piv_data, threshold_key, by = join_by(name_id)) |> 
         mutate(threshold = max*prop) |> 
         select(!prop)
       cons_clust(piv_data)
     }
     
     ##the output of the function (arbitrary name)
     piv_data_comp |> 
       group_by(name) |> 
       slice(1) |> 
       select(!c(time, value, name_id, clust_id, n, clust_time)) |> 
       gt()
   })
   
   output$plot <- renderPlot({
     df <- reactives$mydata
     
     if (length(input$inCheckGroup1) == 0) {
       
       piv_data <- df |> 
         group_by(name) |> 
         mutate(
           max = max(value),
           threshold = max*0.1 
         )
       
       cons_clust(piv_data)
       
     } else {
       
       piv_data <- df |> 
         group_by(name) |> 
         mutate(
           max = max(value) 
         )
       
       threshold_key <- tibble(prop = c(input$threshold1, input$threshold2, input$threshold3,
                                        input$threshold4, input$threshold5, input$threshold6,
                                        input$threshold7, input$threshold8, input$threshold9,
                                        input$threshold10),
                               name_id = c("1", "2", "3",
                                           "4", "5", "6",
                                           "7", "8", "9",
                                           "10"))
       
       piv_data <- left_join(piv_data, threshold_key, by = join_by(name_id)) |> 
         mutate(threshold = max*prop) |> 
         select(!prop)
       
       cons_clust(piv_data)
     }
     
     
     ##the output of the function (arbitrary name), wrapped in a plot
     outputPlot <- ggplot(data = piv_data, aes(x = time, y = value)) +
       geom_hline(aes(yintercept = threshold), 
                  color = "red") +
       geom_line(aes(alpha = value <= threshold, 
                     group = name)) +
       geom_point(aes(alpha = value <= threshold)) +
       facet_wrap(facets = vars(name), 
                  ncol = 2) +
       geom_label(data = piv_data |> 
                    group_by(name) |> 
                    mutate(max_time = max(time)) |> 
                    distinct(name, name_id, max, max_time), 
                  aes(x = max_time*0.005, y = mean(max), label = name_id),
                  hjust = "inward") +
       labs(
         title = "Actigraphy data",
         x = "Time",
         y = "Activity"
       )
     outputPlot
   })
   
   # Downloadable csv of selected dataset (the output of the function) ----
   output$downloadData <- downloadHandler(
     filename = "inactivity_data.csv",
     
     content = function(file) {
       write.csv(piv_data_comp, file, row.names = FALSE)
     }
     
   )
}

# Run the application 
shinyApp(ui = ui, server = server)
