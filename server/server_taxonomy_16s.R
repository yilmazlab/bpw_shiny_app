# server/server_taxonomy_16s.R

observe({
  req(input$taxonomic_level)
  selected_level <- tolower(input$taxonomic_level)
  taxonomy_choices <- unique(taxonomy_16s_frame$taxonomy[taxonomy_16s_frame$taxonomic_level == selected_level])
  
  updatePickerInput(
    session = session,
    inputId = "taxonomy",
    choices = taxonomy_choices,
    selected = NULL
  )
})

df_subset_reactive_16s <- reactiveVal(NULL)

observeEvent(input$apply_16s, {
  req(input$taxonomic_level)
  
  if (is.null(input$taxonomy) || length(input$taxonomy) == 0) {
    shinyWidgets::show_alert(
      title = "No taxa selected",
      text = "Please select at least one taxon for the visualisation.",
      type = "error"
    )
    return(NULL)
  }
  
  subset_data <- transform_and_subset_data_16s(
    raw_data = taxonomy_16s_frame,
    taxonomic_level_filter = tolower(input$taxonomic_level),
    taxonomy_filter = input$taxonomy,
    origin_filter = input$origin,
    group_filter = input$group,
    sample_type_filter = input$sample_type,
    gender_filter = input$gender,
    age_filter = input$age,
    season_filter = input$season
  )
  
  df_subset_reactive_16s(subset_data)
})

output$plot_output_16s <- renderPlotly({
  req(df_subset_reactive_16s())
  df_subset_16s <- df_subset_reactive_16s()
  
  if (nrow(df_subset_16s) == 0) {
    return(NULL)
  }
  
  if (input$visualisation_16s == "barplot") {
    hovertext <- paste(
      "Sample:", df_subset_16s$SampleID, "<br>",
      "Taxonomy:", df_subset_16s$taxonomy, "<br>",
      "Relative Abundance:", round(df_subset_16s$relative_abundance, 4), "<br>",
      "Origin:", df_subset_16s$Origin, "<br>",
      "Group:", df_subset_16s$Group, "<br>",
      "SampleType:", df_subset_16s$SampleType, "<br>",
      "Gender:", df_subset_16s$Gender, "<br>",
      "Age:", df_subset_16s$Adult_Pups, "<br>",
      "Season:", df_subset_16s$Season
    )
    
    plot_ly(df_subset_16s, x = ~SampleID, y = ~relative_abundance, type = 'bar', color = ~taxonomy,
            colors = setNames(df_subset_16s$taxonomy_16s_colour, df_subset_16s$taxonomy),
            hoverinfo = 'text', text = hovertext) %>%
      layout(barmode = 'stack', 
             title = paste(input$taxonomic_level),
             xaxis = list(title = 'Sample'),
             yaxis = list(title = 'Relative abundance'),
             showlegend = FALSE)
    
  } else {
    translation_map_16s <- c(
      "Origin" = "Geographic origin", 
      "Group" = "Host type", 
      "SampleType" = "Sampling location", 
      "Gender" = "Sex", 
      "Season" = "Sampling season"
    )
    
    grouping_var_16s <- input$grouping_variable_16s
    
    boxplots <- df_subset_16s %>%
      ggplot(aes_string(x = grouping_var_16s, y = "relative_abundance", fill = "taxonomy")) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      facet_wrap(~ taxonomy) +
      scale_fill_manual(values = setNames(unique(df_subset_16s$taxonomy_16s_colour), unique(df_subset_16s$taxonomy))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none",
            plot.title = element_text(
              hjust = 0.5,  # Center-align the title
              vjust = 4    # Adjust vertical position (increase to move higher)
              #size = 16,    # Optional: adjust title size
              #face = "bold" # Optional: make the title bold
            )) +
      labs(x = translation_map_16s[[grouping_var_16s]], 
           y = "Relative Abundance",
           title = paste(input$taxonomic_level))
    
    if (length(unique(grouping_var_16s)) > 4) {
      boxplots <- boxplots + scale_x_discrete(labels = NULL)
    }
      
    ggplotly(boxplots)
  }
})

output$taxonomy_16s_info <- renderUI({
  req(df_subset_reactive_16s())
  df_subset_16s <- df_subset_reactive_16s()
  
  if (nrow(df_subset_16s) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset_16s %>%
    group_by(taxonomy, taxonomy_16s_colour) %>%
    summarise(mean_abundance = mean(relative_abundance), .groups = "drop") %>%
    arrange(desc(mean_abundance)) %>%
    mutate(
      mean_abundance = sprintf("%.4f", mean_abundance),
      taxonomy = cell_spec(taxonomy, "html", 
                           color = "white", 
                           background = adjustcolor(taxonomy_16s_colour, alpha.f = 0.7))
    )
  
  table_html <- table_data %>%
    select(taxonomy, mean_abundance) %>%
    kable("html", escape = FALSE, col.names = c("Taxonomy", "Mean Relative Abundance")) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(paste("<h4>Mean Relative Abundance</h4>", table_html))
})