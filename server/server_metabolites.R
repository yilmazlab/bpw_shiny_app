# server/server_metabolites.R

df_subset_reactive_metabolites <- shiny::reactiveVal(tibble::tibble())

observeEvent(input$apply_metabolites, {
  # Generate the title based on the rescaling input
  title_metabolites <- if (input$rescaling_metabolites == "off") {
    "Metabolite concentration (nmol / mg)"
  } else {
    "Sequential log(1 + x)-standardisation rescaled metabolite concentration (nmol / mg)"
  }
  
  output$visualisation_output_metabolites <- renderUI({
    output_plots_metabolites <- tagList()
    
    input_visualisation_metabolites <- isolate(input$visualisation_metabolites)
    
    output_plots_metabolites <- tagAppendChild(output_plots_metabolites, h3(title_metabolites, style = "text-align: center;"))
    
    if (input_visualisation_metabolites %in% c("heatmap", "both")) {
      output_plots_metabolites <- tagAppendChild(
        output_plots_metabolites, 
        div(
          plotlyOutput("heatmap_metabolites", height = "800px")
        )
      )
    }
    if (input_visualisation_metabolites %in% c("boxplots", "both")) {
      output_plots_metabolites <- tagAppendChild(
        output_plots_metabolites, 
        plotlyOutput("boxplots_metabolites", height = "600px")
      )
    }
    if (input_visualisation_metabolites %in% c("boxplots", "both")) {
      output_plots_metabolites <- tagAppendChild(
        output_plots_metabolites,
        uiOutput("metabolite_table")
      )
    }
    
    return(output_plots_metabolites)
  })
  
  subset_data <- transform_and_subset_data_metabolites(
    raw_data = metabolites_frame,
    metabolite_group_filter = unique(metabolites_frame$metabolite_group),  # Use all groups
    metabolite_name_filter = input$metabolite_name,
    rescaling = input$rescaling_metabolites
  )
  
  df_subset_reactive_metabolites(subset_data)
})

output$heatmap_metabolites <- renderPlotly({
  df_subset_metabolites <- df_subset_reactive_metabolites()
  
  if (nrow(df_subset_metabolites) == 0) {
    return(NULL)
  }
  
  heatmap_data <- df_subset_metabolites %>%
    select(metabolite_name, sample_id, metabolite_abundancy) %>%
    spread(key = sample_id, value = metabolite_abundancy) %>%
    column_to_rownames("metabolite_name")
  
  row_side_colour <- df_subset_metabolites %>%
    select(metabolite_name, metabolite_group_colour_metabolites) %>%
    distinct() %>%
    column_to_rownames("metabolite_name")
  
  col_side_colour <- df_subset_metabolites %>%
    select(sample_id, geographic_location_colour_metabolites) %>%
    distinct() %>%
    column_to_rownames("sample_id")
  
  clustering <- isolate(input$clustering_metabolites)
  if (clustering == "both" && nrow(heatmap_data) == 1) {
    shinyWidgets::show_alert(
      title = "Cannot operate clustering on only 1 metabolite",
      text = "Please choose more than 1 metabolite to operate the clustering",
      type = "error"
    )
    return(NULL)
  }
  
  names(row_side_colour) <- "Metabolite group"
  names(col_side_colour) <- "Geographic origin"
  
  title <- if(input$rescaling_metabolites == "off") {
    "Metabolite concentration (nmol / mg)"
  } else {
    "Sequential log(1 + x)-standardisation rescaled metabolite concentration (nmol / mg)"
  }
  
  heatmaply(heatmap_data, 
            RowSideColors = row_side_colour, 
            ColSideColors = col_side_colour, 
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

output$boxplots_metabolites <- renderPlotly({
  df_subset_metabolites <- df_subset_reactive_metabolites()
  
  if (nrow(df_subset_metabolites) == 0) {
    return(NULL)
  }
  
  boxplots <- df_subset_metabolites %>%
    ggplot(aes(x = geographic_location, y = metabolite_abundancy, fill = metabolite_name)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(position = position_jitterdodge(jitter.width = 0.2), alpha = 0.5) +
    facet_wrap(~ metabolite_name) +
    scale_fill_manual(values = setNames(unique(df_subset_metabolites$metabolite_name_colour_metabolites), unique(df_subset_metabolites$metabolite_name)), guide = FALSE) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Geographic origin", y = "Metabolite concentration")
  
  ggplotly(boxplots)
})

output$legend_table_metabolites <- renderUI({
  df_subset_metabolites <- df_subset_reactive_metabolites()
  
  if (nrow(df_subset_metabolites) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset_metabolites %>%
    select(geographic_location, sample_id, geographic_location_colour_metabolites) %>%
    distinct() %>%
    mutate(geographic_location = cell_spec(geographic_location, "html", 
                                           color = "white", 
                                           background = adjustcolor(geographic_location_colour_metabolites, alpha.f = 0.7))) %>%
    select(geographic_location, sample_id)
  
  table_html <- table_data %>%
    kable("html", escape = FALSE, col.names = c("Geographic location", "Sample")) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(table_html)
})

output$metabolite_table <- renderUI({
  df_subset_metabolites <- df_subset_reactive_metabolites()
  
  if (nrow(df_subset_metabolites) == 0) {
    return(NULL)
  }
  
  table_data <- df_subset_metabolites %>%
    select(metabolite_name, metabolite_group, metabolite_name_colour_metabolites) %>%
    distinct() %>%
    mutate(metabolite_name = cell_spec(metabolite_name, "html", 
                                       color = "white", 
                                       background = adjustcolor(metabolite_name_colour_metabolites, alpha.f = 0.7))) %>%
    select(metabolite_name, metabolite_group)
  
  table_html <- table_data %>%
    kable("html", escape = FALSE, col.names = c("Metabolite", "Group")) %>%
    kable_styling("striped", full_width = FALSE)
  
  HTML(table_html)
})