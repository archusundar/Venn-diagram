library(shiny)
library(UpSetR)
library(dplyr)
library(tidyr)
library(VennDiagram)
library(grid)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #title-banner {
        background-color: #4CAF50;
        color: white;
        padding: 10px;
        text-align: center;
        font-size: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      #description {
        font-size: 16px;
        padding: 20px;
        background-color: #f2f2f2;
        border-radius: 8px;
        margin-bottom: 30px;
      }
    "))
  ),
  
  div(id = "title-banner", "GeneSetViz: Interactive Visualization of Differentially Expressed Genes"),
  
  # First Page Description
  div(id = "description", 
      h4("Welcome to GeneSetViz!"),
      p("GeneSetViz is a tool for visualizing and comparing differentially expressed genes across datasets."),
      p("Upload your gene files to generate interactive Venn Diagrams (Upto 5 sample files) or UpSet plots (more than 5 Sample files) to explore gene overlaps."),
      p("Click Generate plot to vizulaize the plots and download it.")
  ),
  
  # Sidebar and Main Panel
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose Gene Files", multiple = TRUE, accept = c(".txt")),
      actionButton("plot", "Generate Plot"),
      downloadButton("downloadPlot", "Download Plot (.jpg)")  # Button to download plot
    ),
    
    mainPanel(
      plotOutput("genePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Store the datasets and plot type
  datasets <- reactiveVal(NULL)
  plotType <- reactiveVal(NULL)
  
  observeEvent(input$plot, {
    req(input$files)
    
    # Read the uploaded files and extract the base names
    datasets_raw <- lapply(seq_along(input$files$datapath), function(i) {
      data <- read.table(input$files$datapath[i], sep = '\t', header = TRUE)
      if (!"Gene.ID" %in% colnames(data)) {
        stop("Each file must contain a 'Gene.ID' column.")
      }
      list(data = data$Gene.ID, name = tools::file_path_sans_ext(basename(input$files$name[i])))
    })
    
    # Create names for the datasets based on file names
    dataset_names <- sapply(datasets_raw, function(ds) ds$name)
    datasets_list <- lapply(datasets_raw, function(ds) ds$data)
    names(datasets_list) <- dataset_names
    datasets(datasets_list)  # Store datasets for reuse
    
    # Determine the number of sets
    num_sets <- length(datasets_list)
    
    if (num_sets <= 5) {
      # Prepare Venn Diagram input
      venn_data <- datasets_list
      names(venn_data) <- names(datasets_list)
      
      # Generate Venn Diagram
      fill_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")[1:num_sets]
      venn.plot <- venn.diagram(
        x = venn_data,
        category.names = names(datasets_list),
        filename = NULL,
        output = TRUE,
        col = "blue",
        fill = fill_colors,
        alpha = 0.5,
        cex = 1.5,
        cat.cex = 1.5,
        cat.dist = 0.05
      )
      
      # Save plot type
      plotType("venn")
      
      # Render the Venn Diagram
      output$genePlot <- renderPlot({
        grid.newpage()
        grid.draw(venn.plot)
      })
      
    } else {
      # Combine datasets into a single data frame for UpSet Plot
      df_list <- lapply(names(datasets_list), function(name) {
        data.frame(Gene.ID = datasets_list[[name]], Location = name)
      })
      combined_df <- bind_rows(df_list)
      
      # Spread the data into a wide format
      df_wide <- combined_df %>%
        mutate(value = 1) %>%
        spread(Location, value, fill = 0)
      
      # Save plot type and data
      plotType("upset")
      
      # Render the UpSet plot
      output$genePlot <- renderPlot({
        upset(df_wide, sets = names(datasets_list), main.bar.color = "skyblue", point.size = 3)
      })
    }
  })
  
  # Download the plot as a .jpg file
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("gene_overlap_plot", Sys.Date(), ".jpg", sep = "")
    },
    content = function(file) {
      # Open a JPEG device
      jpeg(file, width = 800, height = 600)
      
      if (plotType() == "venn") {
        # Regenerate and save the Venn Diagram
        venn_data <- datasets()
        fill_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")[1:length(venn_data)]
        venn.plot <- venn.diagram(
          x = venn_data,
          category.names = names(venn_data),
          filename = NULL,
          output = TRUE,
          col = "blue",
          fill = fill_colors,
          alpha = 0.5,
          cex = 1.5,
          cat.cex = 1.5,
          cat.dist = 0.05
        )
        grid.newpage()
        grid.draw(venn.plot)
      } else if (plotType() == "upset") {
        # Regenerate and save the UpSet plot
        df_list <- lapply(names(datasets()), function(name) {
          data.frame(Gene.ID = datasets()[[name]], Location = name)
        })
        combined_df <- bind_rows(df_list)
        df_wide <- combined_df %>%
          mutate(value = 1) %>%
          spread(Location, value, fill = 0)
        
        # Explicitly draw the UpSet plot
        upset(df_wide, sets = names(datasets()), main.bar.color = "skyblue", point.size = 3)
      }
      
      dev.off()  # Close the device
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
