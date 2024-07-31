# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(heatmaply)
library(grDevices)
library(tibble)
library(tidyr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinybusy)
library(fst)
library(RColorBrewer)
library(qs)

# Load the data and source the utils script
metabolites_frame <- qread(("data/metabolites.qs"))
source("utils/metabolites_utils.R")

# UI
ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  shiny::titlePanel("Metabolites Data Visualization"),
  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      shinyWidgets::pickerInput(
        inputId = "metabolite_name",
        label = "Select metabolites",
        choices = unique(metabolites_frame$metabolite_name),
        options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
        multiple = TRUE
      ),
      
      shiny::radioButtons(
        inputId = "visualization",
        label = "Select visualisation",
        choices = c("Heatmap only" = "heatmap", "Boxplots only" = "boxplots", "Both" = "both"),
        selected = "heatmap"
      ),
      
      shiny::conditionalPanel(
        condition = "input.visualization == 'heatmap' || input.visualization == 'both'",
        radioButtons(
          inputId = "rescaling",
          label = "Rescaling",
          choices = c("On" = "on", "Off" = "off"),
          selected = "off"
        ),
        
        shiny::radioButtons(
          inputId = "clustering",
          label = "Heatmap clustering",
          choices = c("On" = "both", "Off" = "none"),
          selected = "none"
        )
      ),  
      
      shiny::conditionalPanel(
        condition = "input.visualization == 'boxplots' || input.visualization == 'both'",
        shiny::radioButtons(
          inputId = "grouping_variable",
          label = "Grouping variable",
          choices = c("Geographic origin"),
          selected = "Geographic origin"
        )
      ),
      
      shiny::actionButton("apply", "Apply"),
      shiny::uiOutput("legend_table")
    ),
    
    shiny::mainPanel(
      shiny::uiOutput("visualization_output"),
      shiny::uiOutput("metabolite_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  df_subset_reactive <- shiny::reactiveVal(tibble::tibble())
  
  observeEvent(input$apply, {
    output$visualization_output <- renderUI({
      output_plots <- tagList()
      
      input_viz <- isolate(input$visualization)
      
      if (input_viz == "heatmap" || input_viz == "both") {
        output_plots <- tagAppendChild(
          output_plots, 
          plotlyOutput("heatmap", height = "800px")
        )
      }
      if (input_viz == "boxplots" || input_viz == "both") {
        output_plots <- tagAppendChild(
          output_plots, 
          plotlyOutput("boxplots", height = "600px")
        )
      }
      
      return(output_plots)
    })
  })
  
  observeEvent(input$apply, {
    subset_data <- transform_and_subset_data(
      raw_data = metabolites_frame,
      metabolite_group_filter = unique(metabolites_frame$metabolite_group),  # Use all groups
      metabolite_name_filter = input$metabolite_name,
      rescaling = input$rescaling
    )
    
    df_subset_reactive(subset_data)
  })
  
  output$heatmap <- renderPlotly({
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    heatmap_data <- df_subset %>%
      select(metabolite_name, sample_id, metabolite_abundancy) %>%
      spread(key = sample_id, value = metabolite_abundancy) %>%
      column_to_rownames("metabolite_name")
    
    row_side_color <- df_subset %>%
      select(metabolite_name, metabolite_group_color) %>%
      distinct() %>%
      column_to_rownames("metabolite_name")
    
    col_side_color <- df_subset %>%
      select(sample_id, geographic_location_color) %>%
      distinct() %>%
      column_to_rownames("sample_id")
    
    clustering <- isolate(input$clustering)
    
    heatmaply(heatmap_data, 
              RowSideColors = row_side_color, 
              ColSideColors = col_side_color, 
              dendrogram = clustering,
              color = colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(1000),
              custom_hovertext = matrix(
                paste("Metabolite:", rownames(heatmap_data), "<br>Sample:", colnames(heatmap_data)),
                nrow = nrow(heatmap_data),
                ncol = ncol(heatmap_data)
              )) %>%
      layout(
        xaxis = list(showticklabels = FALSE),
        showlegend = c(row_side_colors = FALSE) 
      )
  })
  
  output$boxplots <- renderPlotly({
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    boxplots <- df_subset %>%
      ggplot(aes(x = geographic_location, y = metabolite_abundancy, fill = metabolite_name)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
      facet_wrap(~ metabolite_name, scales = "free_y") +
      scale_fill_manual(values = setNames(unique(df_subset$metabolite_color), unique(df_subset$metabolite_name))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Geographic Location", y = "Metabolite Abundance")
    
    ggplotly(boxplots)
  })
  
output$legend_table <- renderUI({
  df_subset <- df_subset_reactive()
  
  if (nrow(df_subset) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset %>%
    select(geographic_location, sample_id, geographic_location_color) %>%
    distinct() %>%
    mutate(geographic_location = cell_spec(geographic_location, "html", 
                                       color = "white", 
                                       background = adjustcolor(geographic_location_color, alpha.f = 0.7))) %>%
    select(geographic_location, sample_id)
  
  table_html <- table_data %>%
    kable("html", escape = FALSE, col.names = c("Geographic location", "Sample id")) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(table_html)
})
  
  output$metabolite_table <- renderUI({
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    table_data <- df_subset %>%
      select(metabolite_name, metabolite_group, metabolite_color) %>%
      distinct() %>%
      mutate(metabolite_name = cell_spec(metabolite_name, "html", 
                                         color = "white", 
                                         background = adjustcolor(metabolite_color, alpha.f = 0.7))) %>%
      select(metabolite_name, metabolite_group)
    
    table_html <- table_data %>%
      kable("html", escape = FALSE, col.names = c("Metabolite", "Group")) %>%
      kable_styling("striped", full_width = FALSE)
    
    HTML(table_html)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)