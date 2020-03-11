library("shiny")

hate_crimes <- read.csv("data/hate_crimes.csv", stringsAsFactors = FALSE)
gdp_by_state <- read.csv("data/gdp_by_state.csv", stringsAsFactors = FALSE)

#Mohit's Part
sidebar_content <- sidebarPanel(
  
  selectInput(
    inputId = "feature_choice",
    label = "GDP/Percent Contribution",
    choices = c("GDP_in_dollars_2016","Percent_of_US_2016"),
    selected = NULL
  ),
  
  selectInput(
    inputId = "color_choice",
    label = "Choice of Diversity Characteristics",
    choices = c("share_non_citizen", "share_non_white", "share_unemployed_seasonal"),
    selected = NULL
  )
)

main_content <- mainPanel(
  plotOutput("plot"),
  p(
    "Authored By: Mohit Sane"
  )
)

final_panel <- tabPanel(
  "Economy Trends",
  titlePanel("GDP/Percent Contribution of every state to the US Economy and their diverse classifications"),
  sidebarLayout(
    sidebar_content,
    main_content
  )
)


ui <- navbarPage(
  title = "Diversity affecting US Economy",
  final_panel
)

#######



gdp_by_state_mutated_growth <- gdp_by_state %>% 
                               select(NAME, GDP_in_dollars_2014:GDP_in_dollars_2016) %>% 
                               mutate(growth_in_GDP = ((GDP_in_dollars_2016 - GDP_in_dollars_2014)/GDP_in_dollars_2014)*100)

max_gdp_growth <- ceiling(gdp_by_state_mutated_growth 
                          %>% filter ( growth_in_GDP == max(growth_in_GDP)) 
                          %>% pull(growth_in_GDP))
min_gdp_growth <- trunc(gdp_by_state_mutated_growth 
                        %>% filter ( growth_in_GDP == min(growth_in_GDP)) 
                        %>% pull(growth_in_GDP))

states <- c(gdp_by_state %>% pull(NAME))

sidebar_content <- sidebarPanel(
  sliderInput(
    inputId = "year",
    label = "Year",
    sep = "",
    min = 1997,
    max = 2016,
    value = c(1997, 2016)
  )
)

sidebar_content_diversity <- sidebarPanel(
  sliderInput(
    inputId = "gdp",
    label = "GDP",
    min = min_gdp_growth,
    max = max_gdp_growth,
    value = c(min_gdp_growth, max_gdp_growth)
  ),
  selectInput(
    inputId = "state_one_choice",
    label = "State 1",
    choice = states,
  )
)

main_content <- mainPanel(
  plotOutput(
    outputId = "plot"
  )
)

main_content_diversity <- mainPanel(
  plotOutput(
    outputId = "plot_diversity"
  )
)

home <- tabPanel(
  title = ("Home"),
  
  titlePanel("Introduction"),
  
  p(strong("Authors"),
    ": Isabella Heppe, Jaimie Jin, Mohit Sane, Vishank Rughwani"),
  
  p(strong("Group"),
    ": G4"),
  
  p("In the ten days after the 2016 election, there was a much higher number of hate incidents reported to the Southern Poverty Law center than normal - around 900, an average of 90 per day. 
     Most data collected by federal sources includes only hate crimes, which refers to prosecutable offenses - only a fraction of hate incidents. Our data focuses on all hate incidents, which includes non-prosecutable events as well. 
     Our data combines the number of hate crimes per 100,000 people, as well as a variety of other socioeconomic factors, such as household income, racial data, and the Gini Index, which measures economic inequality. 
     There is also data on voting percentages in each state. We are combining this dataset with another dataset that contains records on gross domestic product (GDP) by state from 1997 to 2016. 
     Although GDP is not a complete measure of economic wealth and productivity, it provides a universal measure of comparison. Through comparing both sets of data, we hope to determine what factors make certain locations more vulnerable to hate incidents. 
    In understanding this, we can possibly figure out how to prevent more hate incidents in the future."
    ),
  
  p("Our first dataset comes from an open Github contributor called dmil. The data is sourced from the Southern Poverty Law Center, a well-known nonprofit that specializes in monitoring hate crimes. 
     Although there are known instances of them mislabeling people/events, they are the best source for quickly released data on hate crimes, as FBI data tends to take many months to analyze.",
     a(href = "https://github.com/Shoklan/data-projects/tree/master/dataworld/gdp-per-state", "(source)")
    ),
  
  p("Our second dataset comes from ArcGIS HUB and was updated just last year. It is shared by user lisa_berry and puts down services.arcgis.com as the source. 
     The data itself is from US Bureau of Economic Analysis which is reputable as a agency that has better access to this type of data.",
     a(href = "https://hub.arcgis.com/datasets/a2a8826c4ba44f89a684000fe7c05b8c_0", "(source)")
    )
  )


change_in_GDP_over_time <- tabPanel(
  title = "Change in GDP over time",
  titlePanel("How has each state's GDP changed over time?"),
  sidebarLayout(
    sidebar_content,
    main_content
  )
)

diversity_vs_GDP <- tabPanel(
  title = "Diversity vs. GDP",
  titlePanel("How has diversity affected GDP growth by state?"),
  sidebarLayout(
    sidebar_content_diversity,
    main_content_diversity
  )
)


ui <- navbarPage(title = "GDP and Hate Crimes",
                 home,
                 change_in_GDP_over_time,
                 diversity_vs_GDP)

