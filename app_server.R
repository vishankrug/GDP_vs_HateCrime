library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")

hate_crimes <- read.csv("data/hate_crimes.csv", stringsAsFactors = FALSE)
gdp_by_state <- read.csv("data/gdp_by_state.csv", stringsAsFactors = FALSE)

# data wrangling up here if you do not need to use input variables for it

#Mohit's Part
selected_data_crimes <- hate_crimes%>% filter(hate_crimes$avg_hatecrimes_per_100k_fbi > 2) %>% 
  select("state", "share_non_citizen", "share_non_white", 
         "share_unemployed_seasonal", "avg_hatecrimes_per_100k_fbi")

server <- function(input, output) {
  output$plot <- renderPlot({  
    selected_data_gdp <- gdp_by_state %>% 
      select(state = "NAME", "GDP_in_dollars_2016", "Percent_of_US_2016")
    
    joined_data_mohit <- full_join(selected_data_crimes,selected_data_gdp,by= "state")
    joined_data_mohit <- joined_data_mohit %>% filter(joined_data_mohit$share_non_citizen != "Na")
    View(joined_data_mohit)
    temp <- ggplot(data = joined_data_mohit, 
                   mapping = aes_string(x= "state", y= input$color_choice,color= input$feature_choice)) +
      geom_point() +
      theme(axis.text.x = element_text(size = 7, angle = 90))   +
      labs(color = input$feature_choice, y= input$color_choice, 
           x = "States", title = "Percent of Diverse Characteristics by states and their GDP/Percent Contribution", 
           shape = "Category") +
      geom_smooth(se = FALSE)
    
    return(temp)
    
  })

  # make your plots in here 
server <- function(input, output) {
  output$plot_time <- renderPlot({ 
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
}

