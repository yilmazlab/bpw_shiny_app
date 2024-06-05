# Load necessary libraries
library(shiny)
library(shinyWidgets)
library(heatmaply)
library(grDevices)
library(tibble)
library(tidyr)
library(kableExtra)
library(dplyr)
library(GiNA)

source("utils/utils.R")

final_frame <- qs::qread("data/final_frame.qs")

ui <- fluidPage(
  shinybusy::add_busy_spinner(spin = "fading-circle"),
  shiny::titlePanel("Data Visualization"),
  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,  
      shinyWidgets::pickerInput(
        inputId = "pathway_label",
        label = "Select pathways",
        choices = unique(final_frame$pathway_label),
        options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
        multiple = TRUE
      ),
      
      shiny::uiOutput("pathway_species_ui"),
      
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
          choices = c("Host type", "Geographic origin", "Sampling location", "Developmental stage"),
          selected = "Host type"
        )
      ),
      
      shiny::actionButton("apply", "Apply"),
      shiny::uiOutput("legend_table")
      
    ),
    
    shiny::mainPanel(
      shiny::uiOutput("visualization_output"),  # Use uiOutput() to conditionally render the heatmap and/or boxplots
      shiny::tableOutput("table")  # Add tableOutput() here to always display the table at the bottom
    )
  )
)

server <- function(input, output) {
  
  df_subset_reactive <- shiny::reactiveVal(tibble::tibble())
  
  output$pathway_species_ui <- renderUI({
    req(input$pathway_label)
    df_subset <- final_frame[final_frame$pathway_label %in% input$pathway_label, ]
    species_choices <- unique(df_subset$pathway_species)
    species_choices <- c("Sample community", species_choices[species_choices != "Sample community"])
    pickerInput(
      inputId = "pathway_species",
      label = "Select species",
      choices = species_choices,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      selected = "Sample community",
      multiple = TRUE
    )
  })
  
  shiny::observeEvent(input$apply, {
    output$visualization_output <- renderUI({
      output_plots <- tagList()
      
      input_viz <- shiny::isolate(input$visualization)
      
      if (input_viz == "heatmap" || input_viz == "both") {
        output_plots <- tagAppendChild(output_plots, plotlyOutput("heatmap"))
      }
      if (input_viz == "boxplots" || input_viz == "both") {
        output_plots <- tagAppendChild(output_plots, plotlyOutput("boxplots"))
      }
      
      return(output_plots)
    })

  })
  
  shiny::observeEvent(input$apply, {
    
    subset_data <- transform_and_subset_data(
      raw_data = final_frame, 
      pathway_labels_filter = input$pathway_label, 
      pathway_species_filter = input$pathway_species, 
      rescaling = input$rescaling
    )
    
    df_subset_reactive(subset_data)
    
  })
  
  
  output$heatmap <- renderPlotly({
    
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    heatmap_data <- df_subset |>
      dplyr::select(selection_label, sample_id, abundance) |>
      tidyr::spread(key = sample_id, value = abundance) |>
      tibble::column_to_rownames("selection_label")
    
    unique_sample_ids <- unique(df_subset$sample_id)
    
    heatmap_data <- tibble::as_tibble(heatmap_data)

    heatmap_data <- heatmap_data |>dplyr::select(all_of(unique_sample_ids))
    
    heatmap_data <- as.matrix(heatmap_data)
    
    row_side_color <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      dplyr::distinct() |>
      tibble::column_to_rownames("selection_label")
    
    names(row_side_color)[names(row_side_color) == "pathway_color"] <- "Pathway label"
    
    col_side_color <- df_subset |>
      dplyr::select(sample_id, host_group_color, geographic_location_color, sampling_location_color, young_adult_color) |>
      dplyr::distinct() |>
      dplyr::arrange(match(sample_id, colnames(heatmap_data))) |> 
      tibble::column_to_rownames("sample_id") 
    
    # TODO: Use the rename function from dplyr
    names(col_side_color)[names(col_side_color) == "host_group_color"] <- "Host type"
    names(col_side_color)[names(col_side_color) == "geographic_location_color"] <- "Geographic origin"
    names(col_side_color)[names(col_side_color) == "sampling_location_color"] <- "Sampling location"
    names(col_side_color)[names(col_side_color) == "young_adult_color"] <- "Developmental stage"

    
    heatmaply(heatmap_data, 
              RowSideColors = row_side_color, 
              ColSideColors = col_side_color, 
              dendrogram = shiny::isolate(input$clustering),
              color = colorRampPalette(rev(brewer.pal(n = 11, name =
                                                        "RdBu")))(1000),
              custom_hovertext = matrix(
                paste("Pathway label:", rownames(heatmap_data), "<br>Sample:", colnames(heatmap_data)),
                nrow = nrow(heatmap_data),
                ncol = ncol(heatmap_data)
              )              ) |>
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
    
    # Create a named vector of colors
    color_vector <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      distinct() |>
      pull(pathway_color, name = selection_label)  # Use pull to create a named vector
    
    # Determine the grouping variable based on the selected radio button
    grouping_variable <- switch(shiny::isolate(input$grouping_variable),
                                "Host type" = df_subset$host_group,
                                "Geographic origin" = df_subset$geographic_location,
                                "Sampling location" = df_subset$sampling_location,
                                "Developmental stage" = df_subset$young_adult)
    
    # Create the boxplots
    boxplots <- df_subset |>
      ggplot(aes(x = grouping_variable, y = abundance, fill = selection_label)) +
      geom_boxplot(outliers = F) +
      geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +  # Add dotplot with transparency and color mapping
      facet_wrap(~ selection_label) +
      scale_fill_manual(values = color_vector, guide = FALSE) +  # Use the color vector here and remove the legend
      theme_classic() +
      labs(x = shiny::isolate(input$grouping_variable), y = "Relative abundance")  # Set the x and y axis labels
    
    # Remove the x-axis labels if there are more than 4 unique values in grouping_variable
    if (length(unique(grouping_variable)) > 4) {
      boxplots <- boxplots + scale_x_discrete(labels = NULL)
    }
    
    ggplotly(boxplots)
    
  })
  
  output$table <- renderUI({
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    table_data <- df_subset |>
      dplyr::select(selection_label, pathway_color, pathway_code, pathway_description, pathway_species, significantly_different) |>
      distinct() |>
      mutate(selection_label = cell_spec(selection_label, "html", color = "white", background = adjustcolor(pathway_color, alpha.f = 0.7))) |>
      dplyr::rename("Pathway label" = selection_label, 
                    "Pathway code" = pathway_code, 
                    "Pathway description" = pathway_description, 
                    "Species / community" = pathway_species, 
                    "Significant difference" = significantly_different)
    
    # Create the table
    table_html <- table_data |>
      select(-pathway_color)  |>
      kable("html", escape = FALSE) |>
      kable_styling("striped", full_width = FALSE)
    
    # Render the table as HTML
    HTML(table_html)
  })
  
  output$legend_table <- renderUI({
    
    df_subset <- df_subset_reactive()
    
    if (nrow(df_subset) == 0) {
      return(NULL)
    }
    
    # Use levels() function for factors
    # Modify host_group_df
    host_group_df <- data.frame(
      Label = levels(df_subset$host_group), 
      Description = "Host type of sample", 
      variable_index = "Host type", 
      Colors = "   ", 
      color_index = sapply(levels(df_subset$host_group), function(l) {
        # Find the first color where df_subset$host_group matches the level
        color <- df_subset$host_group_color[df_subset$host_group == l][1]
        # If no color is found, return NA
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )
    
    # Modify geographic_location_df
    geographic_location_df <- data.frame(
      Label = levels(df_subset$geographic_location), 
      Description = "Geographic origin of sample", 
      variable_index = "Geographic origin", 
      Colors = "   ", 
      color_index = sapply(levels(df_subset$geographic_location), function(l) {
        color <- df_subset$geographic_location_color[df_subset$geographic_location == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )
    
    # Modify sampling_location_df
    sampling_location_df <- data.frame(
      Label = levels(df_subset$sampling_location), 
      Description = "Type / location of sample", 
      variable_index = "Sampling location", 
      Colors = "   ", 
      color_index = sapply(levels(df_subset$sampling_location), function(l) {
        color <- df_subset$sampling_location_color[df_subset$sampling_location == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )
    
    # Modify young_adult_df
    young_adult_df <- data.frame(
      Label = levels(df_subset$young_adult), 
      Description = "Developmental status of host", 
      variable_index = "Developmental stage", 
      Colors = "   ", 
      color_index = sapply(levels(df_subset$young_adult), function(l) {
        color <- df_subset$young_adult_color[df_subset$young_adult == l][1]
        if (is.na(color)) {
          return(NA)
        } else {
          return(color)
        }
      })
    )
    
    # Combine data frames
    legend_data <- rbind(host_group_df, geographic_location_df, sampling_location_df, young_adult_df) |>
      dplyr::mutate(variable_index = ifelse(variable_index=="host_group", "Host type",
                                     ifelse(variable_index=="geographic_location", "Geographic origin",
                                            ifelse(variable_index=="sampling_location", "Sampling location",
                                                   ifelse(variable_index=="young_adult", "Developmental stage", variable_index))))) |>
      dplyr::mutate(variable_index = factor(variable_index, levels = c("Host type", "Geographic origin", "Sampling location", "Developmental stage")))
    
    
    # Create the legend table
    legend_html <- legend_data |>
      dplyr::arrange(variable_index) |>
      dplyr::mutate(Colors = cell_spec(Colors, "html", color = "white", background = adjustcolor(color_index, alpha.f = 0.7))) |>
      select(Colors) |>
      kable("html", escape = FALSE) |>
      pack_rows(index = table(arrange(legend_data |>
                                        dplyr::select(variable_index) |>
                                        dplyr::arrange(variable_index))), 
                background = "grey80") |>
      kable_styling("striped", full_width = FALSE)
    
    # Render the legend table as HTML
    HTML(legend_html)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)