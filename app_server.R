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
    scale_fill_distiller(palette = "Spectral") +
    theme_void() +
    labs(title = "Percent Growth in GDP", fill = "Change")
    
    return(plot)
  })
  
  output$plot_diversity <- renderPlot({
    
    hate_crimes_share_non_white <- hate_crimes %>% select(NAME = "state", "share_non_white")
    
    gdp_by_state_mutated <- gdp_by_state %>% select(NAME, GDP_in_dollars_2014:GDP_in_dollars_2016) %>% 
      mutate(growth_in_GDP = ((GDP_in_dollars_2016 - GDP_in_dollars_2014)/GDP_in_dollars_2014)*100)
    
    joined_data_vishank <- left_join(gdp_by_state_mutated, hate_crimes_share_non_white, by = "NAME") %>% arrange(-share_non_white)
    
    joined_data_vishank$cut <- cut(joined_data_vishank$share_non_white, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("< 0.2%", "0.2% to 0.4%", "0.4% to 0.6%", "0.6% to 0.8%", " > 0.8%"))
    
    
    joined_data_vishank <- joined_data_vishank %>% filter(growth_in_GDP < input$gdp_input[2], growth_in_GDP > input$gdp_input[1])
    
    if(input$state_one_choice != "State" & input$state_two_choice != "State"){
      joined_data_vishank <- joined_data_vishank %>% filter(NAME %in% c(input$state_one_choice, input$state_two_choice))
    }
  
    plot_vishank <- ggplot(data = joined_data_vishank) + geom_col(mapping = aes(x = reorder(NAME, -growth_in_GDP), y = growth_in_GDP, fill = cut)) +
      labs(title = "How does diversity affect GDP Growth in States of US", x = "State", y = "% Growth in GDP (2014 to 2016)", fill = "Share of population = non-white")+
      theme(axis.text.x = element_text(size = 7, angle = 90))
    
    return(plot_vishank)
  })
}

