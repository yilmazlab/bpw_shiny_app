# utils/general_utils.R

generate_colours <- function(type, size, n) {
  colour_lists <- list(
    colour_group_1 = c("#36648B", "#CD4F39", "#CDBA96", "#CD96CD", "#00868B", "#473C8B", "#698B22", "#CD8500", "#8B0A50", "#00008B", "#8B2323", "#458B00"),
    colour_group_2 = c("#48D1CC", "#9400D3", "#9A0000", "#7CFC00", "#0000CD", "#008B8B"),
    colour_group_3 = c("#BDB76B", "#E9967A", "#90EE90"),
    colour_group_4 = c("#8B4513", "#B0E0E6", "#EE82EE")
  )
  
  colour_list <- colour_lists[[type]]
  if (is.null(colour_list)) stop("Invalid type specified")
  
  if (size <= length(colour_list)) {
    return(sample(colour_list, size = size))
  } else {
    colour_generator <- colorRampPalette(colour_list)
    return(sample(colour_generator(n), size = size, replace = TRUE))
  }
}