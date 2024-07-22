### Shiny package to get the Venn Diagram from a list of files (Differentially expressed genes between different conditions/samples)

### This shiny app uses VennDiagram to create Ven diagram for upto file gene list.
### For gene list more than 5, R-package named upSetR is used to create Upset plot instead of Venn Diagram used to deal with more than 5 sets.


library(shiny)
library(UpSetR)
library(dplyr)
library(tidyr)
library(VennDiagram)
library(gridExtra)

# Define UI
ui <- fluidPage(
  titlePanel("Gene Overlap Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose Gene Files", multiple = TRUE, accept = c(".txt")),
      actionButton("plot", "Generate Plot")
    ),
    
    mainPanel(
      plotOutput("genePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$plot, {
    req(input$files)
    
    # Read the uploaded files
    datasets <- lapply(input$files$datapath, function(file_path) {
      read.table(file_path, sep = '\t', header = TRUE)
    })
    
    # Ensure all datasets have a "Gene.ID" column
    datasets <- lapply(datasets, function(df) {
      if ("Gene.ID" %in% colnames(df)) {
        return(df$Gene.ID)
      } else {
        stop("Each file must contain a 'Gene.ID' column.")
      }
    })
    
    # Create names for the datasets
    names(datasets) <- paste0("Location", 1:length(datasets))
    
    # Determine the number of sets
    num_sets <- length(datasets)
    
    if (num_sets <= 5) {
      # Prepare Venn Diagram input
      venn_data <- datasets
      names(venn_data) <- names(datasets)
      
      # Render Venn Diagram using VennDiagram package
      output$genePlot <- renderPlot({
        fill_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")[1:num_sets]
        venn.plot <- venn.diagram(
          x = venn_data,
          category.names = names(datasets),
          filename = NULL,
          output = TRUE,
          col = "blue",
          fill = fill_colors,
          alpha = 0.5,
          cex = 1.5,
          cat.cex = 1.5,
          cat.dist = 0.05
        )
        grid.draw(venn.plot)
      })
      
    } else {
      # Combine datasets into a single data frame for UpSet Plot
      df_list <- lapply(names(datasets), function(name) {
        data.frame(Gene.ID = datasets[[name]], Location = name)
      })
      combined_df <- bind_rows(df_list)
      
      # Spread the data into a wide format
      df_wide <- combined_df %>%
        mutate(value = 1) %>%
        spread(Location, value, fill = 0)
      
      # Render UpSet Plot
      output$genePlot <- renderPlot({
        upset(df_wide, sets = names(datasets), main.bar.color = "skyblue", point.size = 3)
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
