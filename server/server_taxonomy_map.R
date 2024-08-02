# server/server_taxonomy_map.R

observe({
  req(input$display_taxonomy_map)
  
  output$leaflet_taxonomy_map <- leaflet::renderLeaflet({
    taxonomy_phylum_data <- read_and_transform_taxonomy_xlsx(
      "data/taxonomy_world_map.xlsx",
      sheet = 1
    ) %>%
      mutate(category = "Phylum")
    
    taxonomy_genus_data <- read_and_transform_taxonomy_xlsx(
      "data/taxonomy_world_map.xlsx",
      sheet = 2
    ) %>%
      mutate(category = "Genus")
    
    taxonomy_data <- bind_rows(taxonomy_phylum_data, taxonomy_genus_data) %>%
      arrange(location) %>%
      mutate(location = as.factor(location))
    
    taxonomy_data_split <- split(taxonomy_data, taxonomy_data$location)
    
    plot_list <- map(taxonomy_data_split, generate_leaflet_plot_list)
    
    temp_files <- map(plot_list, function(plot) {
      temp_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(plot, file = temp_file)
      temp_file
    })
    
    leaflet_data <- taxonomy_data %>%
      distinct(location, longitude, latitude) %>%
      arrange(location)
    
    leaflet(leaflet_data) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, group = "location") %>%
      leafpop:::addPopupIframes(temp_files, group = "location", width = 600, height = 400)
  })
})