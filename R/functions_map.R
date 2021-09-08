
library(ggplot2)

# map_metapr2 : Draw maps of metapr2 data ----------------------------------------------

#' @title Draw maps of metapr2 data
#'
#' @description
#' Create maps (normal and polar) for metapr2 data (asv corresponding to cell either from culture or from single cell)
#' @param df data frame with the number of reads where a given culture asv is present
#' @param samples list of all samples
#'
#' @examples
#' 
#' @md
#' @export

map_metapr2 <- function(df, samples) {

# Must transform data frame to sf object (if not coordinates are not correctly transformed)
# Cannot use jitter
# In polar coordinates, the limits are a bit tricky to determine (must transform lat long to polar)

 
  # df <- df_cultures # Debugging
  # samples <- samples_diatoms # Debugging

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  proj_polar <-"+proj=laea +lat_0=75 +lon_0=20 "

  color_substrate = c("algae"="green","ice"="red", "water"="blue", "sediment" = "brown")

  
  for (one_label in sort(unique(df$label))) {
  
  # one_culture <- "HE492-43" # Debugging
    
  cat("species, ASV, culture: ", one_label,"\n")  

    
  cell_present <- df %>% 
    filter(label == one_label) %>% 
    select(label, 
           file_code, fraction_name, substrate, latitude, longitude, 
           n_reads_label, n_reads_label_pct) %>% 
    mutate(substrate = str_replace(substrate, "first year ", "")) %>% 
    distinct() %>% 
    sf::st_as_sf( coords = c("longitude", "latitude"),
                  crs = proj_longlat)
  
    cell_absent <- samples %>% 
    filter(!is.na(file_code %in% cell_present$file_code)) %>% 
    select(fraction_name, substrate, latitude, longitude) %>% 
    distinct() %>% 
    sf::st_as_sf( coords = c("longitude", "latitude"),
                  crs = proj_longlat)
  
  z_max = max(cell_present$n_reads_label_pct)
  z_max = case_when(z_max > 10 ~ 100,
                    z_max > 1  ~ 10,
                    TRUE ~1 )
  z_limits <- c(0,z_max)
  z_breaks <- z_max*c(0.10,0.25, 0.50, 0.75, 1)
  # z_limits <- c(0,100)
  # z_breaks <- c(0.1, 1, 10, 100)
  
  # Normal projections

  g_map_sf <- ggplot() + 
    geom_sf(data = world, color="grey", fill="grey")+
    xlab("Longitude") + ylab("Latitude") +
    geom_sf(data=cell_absent,
            color="grey40", shape=3, size=1) +
    geom_sf(data=cell_present,
            aes(fill=substrate, color=substrate, size = n_reads_label_pct))+
    scale_size_area(name = "% of diatoms", max_size = 5,limits = z_limits, breaks=z_breaks) +
    # scale_size(name = "% of diatoms", range = c(0,5) ,limits = z_limits, breaks=z_breaks, trans="log10" ) +
    coord_sf(expand = FALSE) +
    theme_light() +
    scale_x_continuous(breaks = (-2:2) * 90) +
    scale_y_continuous(breaks = (-2:2) * 30)  +
    scale_fill_manual(values = color_substrate)+
    scale_color_manual(values = color_substrate) +
    theme(plot.tag = element_text(size= 20)) +
    labs(title=one_label)
                 
  print(g_map_sf)
  ggsave(plot= g_map_sf , filename=str_c("../figs/map_world_",one_label,".pdf"),
       width = 15 , height = 12, scale=1.80, 
       units="cm", useDingbats=FALSE)

  # Polar projection
  
  xmin <- sf::sf_project(from=proj_longlat, to=proj_polar, matrix(c(-90,55),ncol=2))
  xmin <- xmin[,1]
  xmax <- sf::sf_project(from=proj_longlat, to=proj_polar, matrix(c(+90,55),ncol=2))
  xmax <- xmax[,1]
  ymin <- sf::sf_project(from=proj_longlat, to=proj_polar, matrix(c(0,55),ncol=2))
  ymin <- ymin[,2]
  ymax <- sf::sf_project(from=proj_longlat, to=proj_polar, matrix(c(180,55),ncol=2))
  ymax <- ymax[,2]

  g_map_polar_sf <- ggplot() + 
    geom_sf(data = world, color="grey", fill="grey") +
    # geom_sf(data=cell_species_present) +
    geom_sf(data=cell_present, 
            mapping=aes(fill=substrate, color=substrate, size=n_reads_label_pct)) +
    scale_size_area(name = "% of diatoms", max_size = 5,limits = z_limits, breaks=z_breaks) +
    # scale_size(name = "% of diatoms", range = c(0, 5),limits = z_limits, breaks=z_breaks) +
    geom_sf(data=cell_absent, color="grey40", shape=3, ) +
    coord_sf(crs = proj_polar, expand = FALSE, xlim = c(xmin, xmax), ylim= c(ymin,ymax)) +
    scale_fill_manual(values = color_substrate)+
    scale_color_manual(values = color_substrate)+
    theme_light()  +
    theme(plot.tag = element_text(size= 20),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_line(color="grey35"),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())  +
    labs(title=one_label)
  
  print(g_map_polar_sf)
  
  ggsave(plot= g_map_polar_sf , filename=str_c("../figs/map_polar_",one_label,".pdf"),
       width = 15 , height = 12, scale=1.80, 
       units="cm", useDingbats=FALSE)
  
  }  

}  
