library("shiny")
library("dplyr")
library("ggplot2")
library("tidyr")

hate_crimes <- read.csv("data/hate_crimes.csv", stringsAsFactors = FALSE)
gdp_by_state <- read.csv("data/gdp_by_state.csv", stringsAsFactors = FALSE)

# data for the voter panel (Isabella)
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

axis_names <- list("voters_voted_trump" = "Population that voted for Trump (%)",
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




server <- function(input, output) {
  output$mohit_plot <- renderPlot({  
    
    selected_data_crimes <- hate_crimes%>% filter(hate_crimes$avg_hatecrimes_per_100k_fbi > 2) %>% 
      select("state", "share_non_citizen", "share_non_white", 
             "share_unemployed_seasonal", "avg_hatecrimes_per_100k_fbi")
    selected_data_gdp <- gdp_by_state %>% 
      select(state = "NAME", "GDP_in_dollars_2016", "Percent_of_US_2016")
    
    joined_data_mohit <- full_join(selected_data_crimes,selected_data_gdp,by= "state")
    joined_data_mohit <- joined_data_mohit %>% filter(joined_data_mohit$share_non_citizen != "Na")
    temp <- ggplot(data = joined_data_mohit, 
                   mapping = aes_string(x= "state", y= input$color_choice,color= input$feature_choice)) +
      geom_point() +
      theme(axis.text.x = element_text(size = 7, angle = 90), rect = element_rect(fill = '#fcfcfc', colour = '#fcfcfc'))   +
      labs(color = input$feature_choice, y= input$color_choice, 
           x = "States", title = "Percent of Diverse Characteristics by states and their GDP/Percent Contribution", 
           shape = "Category") +
      geom_smooth(se = FALSE)
    
    return(temp)
  })
  
  # GDP progression (Jaimie)
  output$plot_time <- renderPlot({ 
    map_coor <- map_data("state") %>% mutate(state_name = toupper(region))
    
    gdp_data <- gdp_by_state %>% 
      mutate(gdp_change = (
        gdp_by_state[[paste0("GDP_in_dollars_", input$year[2])]] - gdp_by_state[[paste0("GDP_in_dollars_", input$year[1])]])/gdp_by_state[[paste0("GDP_in_dollars_", input$year[1])]] * 100 , state_name = toupper(NAME)) %>%
      select(gdp_change, state_name)
    
    gdp_mean <- mean(gdp_data$gdp_change)
    
    gdp_max <- max(gdp_data$gdp_change)
    
    gdp_data_all <- left_join(gdp_data, map_coor, by="state_name")
    
    plot <- ggplot(data = gdp_data_all) +
    geom_polygon(aes(x = long, y = lat, group= group, fill = gdp_change)) +
    coord_quickmap() +
    scale_fill_distiller(palette = "Spectral") +
    theme_void() +
    labs(title = "Percent Growth in GDP", fill = "Change")
    
    return(plot)
  })
  
  # diversity (Vishank)
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
      theme(axis.text.x = element_text(size = 7, angle = 90), rect = element_rect(fill = '#fcfcfc', colour = '#fcfcfc'))
    
    return(plot_vishank)
  })
  
  
    output$mohit_plot <- renderPlot({  
      
      selected_data_crimes <- hate_crimes%>% filter(hate_crimes$avg_hatecrimes_per_100k_fbi > 2) %>% 
        select("state", "share_non_citizen", "share_non_white", 
               "share_unemployed_seasonal", "avg_hatecrimes_per_100k_fbi")
      selected_data_gdp <- gdp_by_state %>% 
        select(state = "NAME", "GDP_in_dollars_2016", "Percent_of_US_2016")
      
      joined_data_mohit <- full_join(selected_data_crimes,selected_data_gdp,by= "state")
      joined_data_mohit <- joined_data_mohit %>% filter(joined_data_mohit$share_non_citizen != "Na")
      temp <- ggplot(data = joined_data_mohit, 
                     mapping = aes_string(x= "state", y= input$color_choice,color= input$feature_choice)) +
        geom_point() +
        theme(axis.text.x = element_text(size = 7, angle = 90), rect = element_rect(fill = '#fcfcfc', colour = '#fcfcfc'))   +
        labs(color = input$feature_choice, y= input$color_choice, 
             x = "States", title = "Percent of Diverse Characteristics by states and their GDP/Percent Contribution", 
             shape = "Category") +
        geom_smooth(se = FALSE)
      
      return(temp)
    }) 
  
  
# voter connections (Isabella)
    
  voter_ranges <- reactiveValues(x = NULL, y = NULL)  
  
  output$voter_plot <- renderPlot({
    voter_data <- voter_source %>% 
      select("share_voters_voted_trump", "voters_voted_trump", "state", input$voter_values)
    
    voter_names <- voter_data %>% 
      select(-c("state", "share_voters_voted_trump"))
    
    voter_data <- gather(voter_data, key = category, value = value, -state, -share_voters_voted_trump) %>% 
      arrange()
    
    axis_values <- c(axis_names[colnames(voter_names)]) %>% 
      unlist()
    
    voter_return <- ggplot(data = voter_data, mapping = aes(x = reorder(state, share_voters_voted_trump))) + 
      geom_point(mapping = aes(y = value, shape = category, color = category)) +
      labs(x = "States", y = "Value", color = "Category", shape = "Category") +
      theme(axis.text.x = element_text(size = 8, angle = 90), legend.position = "top", rect = element_rect(fill = '#fcfcfc', colour = '#fcfcfc')) +
      scale_shape(labels = axis_values) + 
      scale_color_discrete(labels = axis_values)
    
    return(voter_return)
  })
  
  output$voter_plot_zoom <- renderPlot({
    voter_data <- voter_source %>% 
      select("share_voters_voted_trump", "voters_voted_trump", "state", input$voter_values)
    
    voter_names <- voter_data %>% 
      select(-c("state", "share_voters_voted_trump"))
    
    voter_data <- gather(voter_data, key = category, value = value, -state, -share_voters_voted_trump) %>% 
      arrange()
    
    axis_values <- c(axis_names[colnames(voter_names)]) %>% 
      unlist()
    
    voter_return <- ggplot(data = voter_data, mapping = aes(x = reorder(state, share_voters_voted_trump))) + 
      geom_point(mapping = aes(y = value, shape = category, color = category)) +
      labs(x = "States", y = "Value", color = "Category", shape = "Category") +
      theme(axis.text.x = element_text(size = 8, angle = 90), legend.position = "none", 
            rect = element_rect(fill = '#fcfcfc', colour = '#fcfcfc')) +
      scale_shape(labels = axis_values) + 
      scale_color_discrete(labels = axis_values) + 
      coord_cartesian(xlim = voter_ranges$x, ylim = voter_ranges$y, expand = FALSE)
    
    return(voter_return)
  })
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      voter_ranges$x <- c(brush$xmin, brush$xmax)
      voter_ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      voter_ranges$x <- NULL
      voter_ranges$y <- NULL
    }
  })
  
  output$correlation_results <- renderText({
    correlation <- cor(x = voter_source$voters_voted_trump, y = voter_source[input$voter_values], use = "complete.obs") %>% 
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
    if (correlation >= 0 & correlation < 0.2){
      cor_finding <- "very weak positive"
    } else if (correlation >= 0.2 & correlation < 0.4){
      cor_finding <- "weak positive"
    } else if (correlation >= 0.4 & correlation <= 0.5){
      cor_finding <- "moderately positive"
    } else if (correlation >= 0.5 & correlation <= 1){
      cor_finding <- "strong positive"
    }else if (correlation < 0 & correlation > -0.2){
      cor_finding <- "very weak negative"
    } else if (correlation <= -0.2 & correlation > -0.4){
      cor_finding <- "weak negative"
    } else if (correlation <= -0.4 & correlation > -0.5){
      cor_finding <- "moderately negative"
    }else {
      cor_finding <- "strong negative"
    } 
    cor_statement <- paste0("There is a ", cor_finding, " correlation between how people voted in 2016 and the ", 
                           sentence_finish[input$voter_values], ", with a correlation coefficient of ", correlation, ".")
    return(cor_statement)
  })
} 


