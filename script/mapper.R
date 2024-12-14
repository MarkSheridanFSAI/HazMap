mapper <- function(data_,title_){
ggplot(data = data_) +
  geom_sf(fill = "lightblue", color = "black") +
  ggtitle(title_) +
  annotation_scale(location = "br", width_hint = 0.3) + # Scale bar at bottom-right
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) + # North arrow above the scale bar
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
}