# ui/ui_taxonomy_16s.R

ui_taxonomy_16s <- shiny::sidebarLayout(
  sidebarPanel(
    width = 3,
    radioButtons(
      inputId = "visualisation_16s",
      label = "Select visualisation",
      choices = c("Barplot" = "barplot", "Boxplot" = "boxplot"),
      selected = "barplot"
    ),
    pickerInput(
      inputId = "taxonomic_level",
      label = "Select taxonomic level",
      choices = c("Phylum", "Family", "Genus"),
      selected = "Phylum",
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      multiple = FALSE
    ),
    pickerInput(
      inputId = "taxonomy",
      label = "Select taxa",
      choices = NULL,
      selected = NULL,
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    conditionalPanel(
      condition = "input.visualisation_16s == 'boxplot'",
      selectInput(
        inputId = "grouping_variable_16s",  
        label = "Select grouping variable",
        choices = c("Geographic origin" = "Origin", 
                    "Host type" = "Group", 
                    "Sampling location" = "SampleType", 
                    "Sex" = "Gender", 
                    "Sampling season" = "Season"),
        selected = "Group"
      )
    ),
    hr(),
    h4("Select subsetting options:"),
    pickerInput(
      inputId = "origin",
      label = "Geographic origin",
      choices = unique(taxonomy_16s_frame$Origin),
      selected = unique(taxonomy_16s_frame$Origin),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "group",
      label = "Host type",
      choices = unique(taxonomy_16s_frame$Group),
      selected = unique(taxonomy_16s_frame$Group),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "sample_type",
      label = "Sampling location",
      choices = unique(taxonomy_16s_frame$SampleType),
      selected = unique(taxonomy_16s_frame$SampleType),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "gender",
      label = "Sex",
      choices = unique(taxonomy_16s_frame$Gender),
      selected = unique(taxonomy_16s_frame$Gender),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "age",
      label = "Developmental stage",
      choices = unique(taxonomy_16s_frame$Adult_Pups),
      selected = unique(taxonomy_16s_frame$Adult_Pups),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    pickerInput(
      inputId = "season",
      label = "Sampling season",
      choices = unique(taxonomy_16s_frame$Season),
      selected = unique(taxonomy_16s_frame$Season),
      options = list(`actions-box` = TRUE, `live-search` = TRUE, multiple = TRUE),
      multiple = TRUE
    ),
    actionButton("apply_16s", "Apply")
  ),
  mainPanel(
    h3("Relative taxa abundance", style = "text-align: center;"),
    plotlyOutput("plot_output_16s", height = "800px"),
    uiOutput("taxonomy_16s_info")
  )
)