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
  
  output$voter_plot <- renderPlot({
    voter_data <- voter_source %>% 
      select("share_voters_voted_trump", "voters_voted_trump", "state", input$voter_values)
    
    voter_names <- voter_data %>% 
      select(-c("state", "voters_voted_trump"))
    
    voter_data <- gather(voter_data, key = category, value = value, -state, -voters_voted_trump)
    
    axis_values <- c(axis_names[colnames(voter_names)]) %>% 
      unlist()
    
    voter_return <- ggplot(data = voter_data, mapping = aes(x = reorder(state, voters_voted_trump))) + 
      geom_point(mapping = aes(y = value, shape = category, color = category)) +
      labs(title = "Voting", x = "States", y = "Value", color = "Category", shape = "Category") +
      theme(axis.text.x = element_text(size = 8, angle = 90)) +
      scale_shape(labels = axis_values) + 
      scale_color_discrete(labels = axis_values)
    
    return(voter_return)
  })
  
  output$correlation_results <- renderText({
    correlation <- cor(x = voter_source$share_voters_voted_trump, y = voter_source[input$voter_values], use = "complete.obs") %>% 
      round(digits = 4)
    sentence_finish <- list("median_household_income" = "median household income",
                       "share_unemployed_seasonal" = "percent of the population that is seasonally unemployed",
                       "share_population_in_metro_areas" = "percent of the population living in metro areas",
                       "share_population_with_high_school_degree" = "percent of adults 25 and older with a high school degree",
                       "share_non_citizen" = "percent of the population that are not US citizens",
                       "share_white_poverty" = "percent of white residents living in poverty",
                       "gini_index" = "Gini index, an inequality coefficient",
                       "share_non_white" = "percent of the population that is not white",
                       "gdp_1997_2016" = "change in GDP from 1997-2016",
                       "gdp_2008_2016" = "change in GDP from 2008-2016")
    if (correlation >= 0 & correlation < 0.4){
      cor_finding <- "weak positive"
    } else if (correlation >= 0.4 & correlation < 0.6){
      cor_finding <- "moderately positive"
    } else if (correlation >= 0.6 & correlation <= 1){
      cor_finding <- "strong positive"
    } else if (correlation < 0 & correlation > -0.4){
      cor_finding <- "weak negative"
    } else if (correlation <= -0.4 & correlation > -0.6){
      cor_finding <- "moderately negative"
    } else {
      cor_finding <- "strong negative"
    } 
    cor_statement <- paste0("There is a ", cor_finding, " correlation between how people voted in 2016 and the ", 
                           sentence_finish[input$voter_values], ", with a correlation coefficient of ", correlation, ".")
    return(cor_statement)
  })
} 
  

# data for the voter panel
gdp_rate_of_change <- gdp_by_state %>% 
    select("NAME", "GDP_in_dollars_1997":"GDP_in_dollars_2016") %>% 
    gather(key = year, value = gdp, -NAME) %>% 
    group_by(NAME) %>% 
    arrange(year, .by_group = TRUE) %>% 
    mutate("percent_change" = (((gdp / lag(gdp)) - 1) * 100))

avg_change_1997_2016 <- summarize(gdp_rate_of_change, "gdp_1997_2016" = mean(percent_change, na.rm = TRUE)) %>% 
  rename("state" = NAME)

avg_change_2008_2016 <- gdp_rate_of_change %>% 
  filter(year > "GDP_in_dollars_2008") %>% 
  summarize("gdp_2008_2016" = mean(percent_change, na.rm = TRUE)) %>% 
  rename("state" = NAME)

axis_names <- list("share_voters_voted_trump" = "Population that voted for Trump (%)",
                   "median_household_income" = "Median household income (in $10,000s)",
                   "share_unemployed_seasonal" = "Percent of population unemployed (seasonally adjusted)",
                   "share_population_in_metro_areas" = "Percent of population in metro areas",
                   "share_population_with_high_school_degree" = "Percent of adults 25 and older with a high school degree",
                   "share_non_citizen" = "Percent of population that are not US citizens",
                   "share_white_poverty" = "Percent of white residents living in poverty",
                   "gini_index" = "Gini index (an inequality coefficient)",
                   "share_non_white" = "Percent of population that is not white",
                   "gdp_1997_2016" = "Change in GDP, 1997-2016",
                   "gdp_2008_2016" = "Change in GDP, 2008-2016")

voter_source<- hate_crimes %>% 
  left_join(avg_change_1997_2016, by = "state") %>% 
  left_join(avg_change_2008_2016, by = "state") %>% 
  mutate("voters_voted_trump" = share_voters_voted_trump) %>% 
  mutate("median_household_income" = median_household_income/10000) %>% 
  select(-c("hate_crimes_per_100k_splc", "avg_hatecrimes_per_100k_fbi"))
