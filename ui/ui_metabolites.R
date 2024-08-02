# ui/ui_metabolites.R

ui_metabolites <- shiny::sidebarLayout(
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
      inputId = "visualisation_metabolites",
      label = "Select visualisation",
      choices = c("Heatmap only" = "heatmap", "Boxplots only" = "boxplots", "Both" = "both"),
      selected = "heatmap"
    ),
    
    # Rescaling option always available
    shiny::radioButtons(
      inputId = "rescaling_metabolites",
      label = "Rescaling",
      choices = c("On" = "on", "Off" = "off"),
      selected = "off"
    ),
    
    shiny::conditionalPanel(
      condition = "input.visualisation_metabolites == 'heatmap' || input.visualisation_metabolites == 'both'",
      shiny::radioButtons(
        inputId = "clustering_metabolites",
        label = "Heatmap clustering",
        choices = c("On" = "both", "Off" = "none"),
        selected = "none"
      )
    ),
    
    shiny::actionButton("apply_metabolites", "Apply"),
    shiny::uiOutput("legend_table_metabolites")
  ),
  
  shiny::mainPanel(
    shiny::uiOutput("visualisation_output_metabolites")
  )
)
