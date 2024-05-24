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
library(shinylive)
library(httpuv)


# Define UI
ui <- fluidPage(
  titlePanel("Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Adjust this value to make the sidebar more narrow or wide
      pickerInput(
        inputId = "pathway_label",
        label = "Select pathways",
        choices = unique(final_frame$pathway_label),
        options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
        multiple = TRUE
      ),
      
      uiOutput("pathway_species_ui"),
      
      radioButtons(
        inputId = "visualization",
        label = "Select visualisation",
        choices = c("Heatmap only" = "heatmap", "Boxplots only" = "boxplots", "Both" = "both"),
        selected = "heatmap"
      ),
      
      conditionalPanel(
        condition = "input.visualization == 'heatmap' || input.visualization == 'both'",
        radioButtons(
          inputId = "rescaling",
          label = "Rescaling",
          choices = c("On" = "on", "Off" = "off"),
          selected = "off"
        ),
        
        radioButtons(
          inputId = "clustering",
          label = "Heatmap clustering",
          choices = c("On" = "both", "Off" = "none"),
          selected = "none"
        )
      ),  # Close the first conditionalPanel here
      
      conditionalPanel(
        condition = "input.visualization == 'boxplots' || input.visualization == 'both'",
        radioButtons(
          inputId = "grouping_variable",
          label = "Grouping variable",
          choices = c("Host type", "Geographic origin", "Sampling location", "Developmental stage"),
          selected = "Host type"
        )
      ),
      
      uiOutput("legend_table")
    ),
    
    mainPanel(
      uiOutput("visualization_output"),  # Use uiOutput() to conditionally render the heatmap and/or boxplots
      tableOutput("table")  # Add tableOutput() here to always display the table at the bottom
    )
  )
)

# Define server logic
server <- function(input, output) {
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
  
  output$visualization_output <- renderUI({
    output_plots <- tagList()
    
    if (input$visualization == "heatmap" || input$visualization == "both") {
      output_plots <- tagAppendChild(output_plots, plotlyOutput("heatmap"))
    }
    if (input$visualization == "boxplots" || input$visualization == "both") {
      output_plots <- tagAppendChild(output_plots, plotlyOutput("boxplots"))
    }
    
    return(output_plots)
  })
  
  df_subset_reactive <- reactive({
    req(input$pathway_label, input$pathway_species)
    
    # Create the chosen subset
    df_subset <- final_frame[final_frame$pathway_label %in% input$pathway_label & final_frame$pathway_species %in% input$pathway_species, ]
    
    # Apply rescaling and log transformation if rescaling is on
    df_subset$abundance <- as.numeric(df_subset$abundance)
    if (input$rescaling == "on") {
      df_subset$abundance <- log1p(df_subset$abundance)
      df_subset$abundance <- scale(df_subset$abundance)
      df_subset$abundance <- log1p(df_subset$abundance)
      
    }
    
    # Get unique pathways
    unique_pathways <- unique(df_subset$pathway)
    
    # Create labels for unique pathways
    pathway_labels <- paste0("pw", seq_along(unique_pathways))
    
    # Create colors for unique pathways
    set.seed(77)
    pathway_colors <- sample(colorRampPalette(c("#36648B", "#CD4F39", "#CDBA96", 
                                                "#CD96CD", "#00868B", "#473C8B", 
                                                "#698B22", "#CD8500", "#8B0A50",
                                                "#00008B", "#8B2323", "#458B00"))(200), 
                             size = length(unique_pathways), replace = TRUE)
    
    # Create a mapping dataframe
    pathway_mapping_df <- data.frame(
      Original = unique_pathways,
      Label = pathway_labels,
      Color = pathway_colors
    )
    
    # Assign labels and colors to pathways
    df_subset <- df_subset |>
      left_join(pathway_mapping_df, by = c("pathway" = "Original")) |>
      mutate(selection_label = Label, pathway_color = Color) |>
      select(-Label, -Color)
    
    # Create a column for host_group colors
    df_subset <- df_subset |>mutate(host_group_color = case_when(
      host_group == "Wild" ~ "#4578BC",
      host_group == "SPF" ~ "#F37C79",
      host_group == "Human" ~ "#138140"
    ))
    
    # Get unique levels
    unique_geographic_locations <- unique(df_subset$geographic_location)
    
    # Create colors
    geographic_location_colors <- sample(colorRampPalette(c("#48D1CC", "#9400D3", "#9A0000", 
                                                            "#7CFC00", "#0000CD", "#008B8B"))(50), 
                                         size = length(unique_geographic_locations), replace = TRUE)
    
    # Create a mapping dataframe
    geographic_location_df <- data.frame(
      Original = unique_geographic_locations,
      Color = geographic_location_colors
    )
    
    # Assign colors 
    df_subset <- df_subset |>
      left_join(geographic_location_df, by = c("geographic_location" = "Original")) |>
      mutate(geographic_location_color = Color) |>
      select(-Color)
    
    # Get unique levels
    unique_sampling_location <- unique(df_subset$sampling_location)
    
    # Create colors
    sampling_location_colors <- sample(colorRampPalette(c("#BDB76B", "#E9967A", "#90EE90"))(5), 
                                       size = length(unique_sampling_location), replace = TRUE)
    
    # Create a mapping dataframe
    sampling_location_df <- data.frame(
      Original = unique_sampling_location,
      Color = sampling_location_colors
    )
    
    # Assign colors 
    df_subset <- df_subset |>
      left_join(sampling_location_df, by = c("sampling_location" = "Original")) |>
      mutate(sampling_location_color = Color) |>
      select(-Color)
    
    # Get unique levels
    unique_young_adult <- unique(df_subset$young_adult)
    
    # Create colors
    young_adult_colors <- sample(colorRampPalette(c("#8B4513", "#B0E0E6", "#EE82EE"))(5), 
                                 size = length(unique_young_adult), replace = TRUE)
    
    # Create a mapping dataframe
    young_adult_df <- data.frame(
      Original = unique_young_adult,
      Color = young_adult_colors
    )
    
    # Assign colors 
    df_subset <- df_subset |>
      left_join(young_adult_df, by = c("young_adult" = "Original")) |>
      mutate(young_adult_color = Color) |>
      select(-Color)
    
    
    df_subset <- df_subset |>
      dplyr::arrange(pathway_code, pathway_species, host_group, geographic_location, sampling_location, young_adult, sample_id)
    
    df_subset
  })
  
  output$heatmap <- renderPlotly({
    df_subset <- df_subset_reactive()
    
    # Create a matrix for the heatmap
    heatmap_data <- df_subset |>
      dplyr::select(selection_label, sample_id, abundance) |>
      tidyr::spread(key = sample_id, value = abundance) |>
      column_to_rownames("selection_label")
    
    # Get unique sample_ids in the order they appear in df_subset
    unique_sample_ids <- unique(df_subset$sample_id)
    
    # Convert the matrix to a data frame
    heatmap_data <- as.data.frame(heatmap_data)

    # Reorder the columns of heatmap_data to match the order of unique_sample_ids
    heatmap_data <- heatmap_data |>dplyr::select(all_of(unique_sample_ids))
    
    # Convert the data frame back to a matrix
    heatmap_data <- as.matrix(heatmap_data)
    
    # Create a data frame for the row side colors
    row_side_color <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      distinct() |>
      column_to_rownames("selection_label")
    names(row_side_color)[names(row_side_color) == "pathway_color"] <- "Pathway label"
    # Create a data frame for the column side colors
    col_side_color <- df_subset |>
      dplyr::select(sample_id, host_group_color, geographic_location_color, sampling_location_color, young_adult_color) |>
      distinct() |>
      arrange(match(sample_id, colnames(heatmap_data))) |> # Arrange rows to match the order of columns in heatmap_data
      column_to_rownames("sample_id") 
    names(col_side_color)[names(col_side_color) == "host_group_color"] <- "Host type"
    names(col_side_color)[names(col_side_color) == "geographic_location_color"] <- "Geographic origin"
    names(col_side_color)[names(col_side_color) == "sampling_location_color"] <- "Sampling location"
    names(col_side_color)[names(col_side_color) == "young_adult_color"] <- "Developmental stage"
    
    
    # Create the heatmap
    heatmaply(heatmap_data, 
              RowSideColors = row_side_color, 
              ColSideColors = col_side_color, 
              dendrogram = input$clustering,
              color = colorRampPalette(rev(brewer.pal(n = 11, name =
                                                        "RdBu")))(1000),
              custom_hovertext = matrix(
                paste("Pathway label:", rownames(heatmap_data), "<br>Sample:", colnames(heatmap_data)),
                nrow = nrow(heatmap_data),
                ncol = ncol(heatmap_data)
              )              ) |>
      layout(
        xaxis = list(showticklabels = FALSE),# Hide x-axis tick labels
        showlegend = c(row_side_colors = FALSE) 
      )
  })
  
  
  # Modify server function
  output$boxplots <- renderPlotly({
    df_subset <- df_subset_reactive()
    
    # Create a named vector of colors
    color_vector <- df_subset |>
      dplyr::select(selection_label, pathway_color) |>
      distinct() |>
      pull(pathway_color, name = selection_label)  # Use pull to create a named vector
    
    # Determine the grouping variable based on the selected radio button
    grouping_variable <- switch(input$grouping_variable,
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
      labs(x = input$grouping_variable, y = "Relative abundance")  # Set the x and y axis labels
    
    # Remove the x-axis labels if there are more than 4 unique values in grouping_variable
    if (length(unique(grouping_variable)) > 4) {
      boxplots <- boxplots + scale_x_discrete(labels = NULL)
    }
    
    ggplotly(boxplots)
  })
  
  output$table <- renderUI({
    df_subset <- df_subset_reactive()
    
    # Create a data frame for the table
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
      mutate(variable_index = ifelse(variable_index=="host_group", "Host type",
                                     ifelse(variable_index=="geographic_location", "Geographic origin",
                                            ifelse(variable_index=="sampling_location", "Sampling location",
                                                   ifelse(variable_index=="young_adult", "Developmental stage", variable_index))))) |>
      mutate(variable_index = factor(variable_index, levels = c("Host type", "Geographic origin", "Sampling location", "Developmental stage")))
    
    
    # Create the legend table
    legend_html <- legend_data |>
      arrange(variable_index) |>
      mutate(Colors = cell_spec(Colors, "html", color = "white", background = adjustcolor(color_index, alpha.f = 0.7))) |>
      select(Colors) |>
      kable("html", escape = FALSE) |>
      pack_rows(index = table(arrange(legend_data |>
                                        select(variable_index) |>
                                        arrange(variable_index))), 
                background = "grey80") |>
      kable_styling("striped", full_width = FALSE)
    
    # Render the legend table as HTML
    HTML(legend_html)
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)