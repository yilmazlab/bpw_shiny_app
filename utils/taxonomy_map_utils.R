# utils/taxonomy_map_utils.R

source("utils/general_utils.R")

generate_leaflet_plot_list <- function(data_to_consider) {
  # Generate colours for all taxa
  unique_taxa <- unique(data_to_consider$taxo_name)
  withr::with_seed(seed_to_use, {
    taxa_colours <- generate_colours("colour_group_1", length(unique_taxa), 200)
  })
  
  colour_mapping <- setNames(taxa_colours, unique_taxa)
  
  ggplot_obj <- ggplot(data_to_consider, aes(location, y = taxo_value, fill = taxo_name)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = colour_mapping) +
    labs(x = NULL, y = NULL) +
    guides(fill = guide_legend(title = NULL)) +
    facet_wrap(~category, scales = "free_y") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  plotly::ggplotly(ggplot_obj)
}