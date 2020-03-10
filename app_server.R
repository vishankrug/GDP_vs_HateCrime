library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")

hate_crimes <- read.csv("data/hate_crimes.csv", stringsAsFactors = FALSE)
gdp_by_state <- read.csv("data/gdp_by_state.csv", stringsAsFactors = FALSE)





server <- function(input, output) {
  output$plot <- renderPlot({ 
    map_coor <- map_data("state") %>% mutate(state_name = toupper(region))
    
    gdp_data <- gdp_by_state %>% 
      mutate(gdp_change = (
        gdp_by_state[[paste0("GDP_in_dollars_", input$year[2])]] - gdp_by_state[[paste0("GDP_in_dollars_", input$year[1])]])/gdp_by_state[[paste0("GDP_in_dollars_", input$year[1])]] * 100 , state_name = toupper(NAME)) %>%
      select(gdp_change, state_name)
    
    gdp_data_all <- left_join(gdp_data, map_coor, by="state_name")
    
    plot <- ggplot(data = gdp_data_all) +
    geom_polygon(aes(x = long, y = lat, group= group, fill = gdp_change)) +
    coord_quickmap() +
    scale_fill_distiller(palette = "Purples") +
    theme_void() +
    labs(title = "Percent Growth in GDP", fill = "Change")
    
    return(plot)
  })
}