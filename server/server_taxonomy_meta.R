# server/server_taxonomy_meta.R

observe({
  req(input$taxonomic_level_meta)
  selected_level <- input$taxonomic_level_meta
  
  level_mapping <- c(
    "Phylum" = "Phylum",
    "Class" = "Class",
    "Order" = "Order",
    "Family" = "Family",
    "Genus" = "Genus",
    "Species" = "Species"
  )
  
  data_level <- level_mapping[selected_level]
  
  relevant_data <- taxonomy_meta_frame %>%
    filter(taxonomic_level == data_level)
  
  taxonomy_choices <- unique(relevant_data$taxonomy)
  
  updatePickerInput(
    session = session,
    inputId = "taxonomy_meta",
    choices = taxonomy_choices,
    selected = NULL
  )
})

df_subset_reactive_meta <- reactiveVal(NULL)

observeEvent(input$apply_meta, {
  req(input$taxonomic_level_meta)
  
  if (is.null(input$taxonomy_meta) || length(input$taxonomy_meta) == 0) {
    shinyWidgets::show_alert(
      title = "No taxa selected",
      text = "Please select at least one taxon for the visualisation.",
      type = "error"
    )
    return(NULL)
  }
  
  subset_data <- transform_and_subset_data_meta(
    raw_data = taxonomy_meta_frame,
    taxonomic_level_filter = input$taxonomic_level_meta,
    taxonomy_filter = input$taxonomy_meta,
    location_filter = input$location_meta,
    group_filter = input$group_meta,
    sampling_location_filter = input$sampling_location_meta,
    gender_filter = input$gender_meta,
    young_adult_filter = input$young_adult_meta
  )
  
  df_subset_reactive_meta(subset_data)
})

output$plot_output_meta <- renderPlotly({
  req(df_subset_reactive_meta())
  df_subset_meta <- df_subset_reactive_meta()
  
  if (nrow(df_subset_meta) == 0) {
    return(NULL)
  }
  
  if (input$visualisation_meta == "barplot") {
    hovertext <- paste(
      "Sample:", df_subset_meta$Sample, "<br>",
      "Taxonomy:", df_subset_meta$taxonomy, "<br>",
      "Relative Abundance:", round(df_subset_meta$relative_abundance, 4), "<br>",
      "Location:", df_subset_meta$Location, "<br>",
      "Group:", df_subset_meta$Group, "<br>",
      "SamplingLocation:", df_subset_meta$SamplingLocation, "<br>",
      "Gender:", df_subset_meta$Gender, "<br>",
      "Young_Adult:", df_subset_meta$Young_Adult
    )
    
    plot_ly(df_subset_meta, x = ~Sample, y = ~relative_abundance, type = 'bar', color = ~taxonomy,
            colors = setNames(df_subset_meta$taxonomy_meta_colour, df_subset_meta$taxonomy),
            hoverinfo = 'text', text = hovertext) %>%
      layout(barmode = 'stack', 
             title = paste(input$taxonomic_level_meta),
             xaxis = list(title = 'Sample'),
             yaxis = list(title = 'Relative Abundance'),
             showlegend = FALSE)
    
  } else {
    translation_map_meta <- c(
      "Location" = "Geographic origin", 
      "Group" = "Host type", 
      "SamplingLocation" = "Sampling location", 
      "Gender" = "Sex", 
      "Young_Adult" = "Developmental stage"
    )
    
    grouping_var_meta <- input$grouping_variable_meta
    
    boxplots <- df_subset_meta %>%
      ggplot(aes_string(x = grouping_var_meta, y = "relative_abundance", fill = "taxonomy")) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      facet_wrap(~ taxonomy) +
      scale_fill_manual(values = setNames(unique(df_subset_meta$taxonomy_meta_colour), unique(df_subset_meta$taxonomy))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(
              hjust = 0.5,  # Center-align the title
              vjust = 4    # Adjust vertical position (increase to move higher)
              #size = 16,    # Optional: adjust title size
              #face = "bold" # Optional: make the title bold
            )) +
      labs(x = translation_map_meta[[grouping_var_meta]],  
           y = "Relative Abundance",
           title = paste(input$taxonomic_level_meta))
    
    ggplotly(boxplots)
  }
})

output$taxonomy_meta_info <- renderUI({
  req(df_subset_reactive_meta())
  df_subset_meta <- df_subset_reactive_meta()
  
  if (nrow(df_subset_meta) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset_meta %>%
    group_by(taxonomy, taxonomy_meta_colour) %>%
    summarise(mean_abundance = mean(relative_abundance), .groups = "drop") %>%
    arrange(desc(mean_abundance)) %>%
    mutate(
      mean_abundance = sprintf("%.4f", mean_abundance),
      taxonomy = cell_spec(taxonomy, "html", 
                           color = "white", 
                           background = adjustcolor(taxonomy_meta_colour, alpha.f = 0.7))
    )
  
  table_html <- table_data %>%
    select(taxonomy, mean_abundance) %>%
    kable("html", escape = FALSE, col.names = c("Taxonomy", "Mean Relative Abundance")) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(paste("<h4>Mean Relative Abundance</h4>", table_html))
})