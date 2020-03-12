library("shiny")
library("dplyr")
library("tidyr")
library("shinythemes")
hate_crimes <- read.csv("data/hate_crimes.csv", stringsAsFactors = FALSE)
gdp_by_state <- read.csv("data/gdp_by_state.csv", stringsAsFactors = FALSE)

# (Isabella) voter data
voter_choices <- selectInput(inputId = "voter_values", 
             width = '600px',
             label = "Possible comparison values",
             choices = list("Median household income (in $10,000s)" = "median_household_income", 
                                "Percent of population unemployed (seasonally adjusted)" = "share_unemployed_seasonal",
                                "Percent of population in metro areas" = "share_population_in_metro_areas",
                                "Percent of adults 25 and older with a high school degree" = "share_population_with_high_school_degree",
                                "Percent of population that is not a US citizen" = "share_non_citizen",
                                "Percent of white residents living in poverty" = "share_white_poverty",
                                "Gini index (an inequality coefficient)" = "gini_index",
                                "Percent of population that is not white" = "share_non_white",
                                "Change in GDP, 1997-2016" = "gdp_1997_2016",
                                "Change in GDP, 2008-2016" = "gdp_2008_2016"),
             selected = "median_household_income"
)
outlier_check <- checkboxInput(inputId = "out_check", label = "Exclude outliers", value = FALSE)

voter_panel <- tabPanel(
  title = "Voting Factors",
  h2("What factors affect how people vote?"),
  hr(),
  h4("Click and drag on the left plot to zoom."),
  fluidRow(
    column(6,
           plotOutput(outputId = "voter_plot", 
                      brush = brushOpts(
                        id = "plot_brush",
                        resetOnNew = TRUE
                        )
           )   
    ), column(6,
              plotOutput(outputId = "voter_plot_zoom")
    )
  ),
  hr(),
  voter_choices,
  outlier_check,
  p(textOutput(outputId = "correlation_results"))
)


#Mohit's Part
hate_crimes_mohit <- hate_crimes%>% filter(hate_crimes$avg_hatecrimes_per_100k_fbi > 2) %>% 
        select("state", "share_non_citizen", "share_non_white", 
               "share_unemployed_seasonal", "avg_hatecrimes_per_100k_fbi")

gdp_by_state_mohit <- gdp_by_state %>% 
        select(state = "NAME", "GDP_in_dollars_2016", "Percent_of_US_2016")

non_citizens <- median(hate_crimes_mohit$share_non_citizen)
percent <- median(gdp_by_state_mohit$Percent_of_US_2016)

sidebar_content_mohit <- sidebarPanel(
  
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
  plotOutput("mohit_plot"),
  p(
    strong("Authored By") , ": Mohit Sane"
  ),
  p("As the plot indicates, the states which have more GDP/Percentage contribution have relatively high diversity proportions. The most visible case if that of California which has high GDP and high diversity as well.",
    "Using certain statistical methods, I found the median of percent contribution to national GDP for every state to be " ,percent,". This was useful to get a parameter for higher percent of contribution among the states. I analysed the data and found 11 states which have higher percentages than the median",
    "It was highly intituitive to see that most of these states had a higher share of non-citizens than the median which was 0.05")
)

final_panel <- tabPanel(
  "Economy Trends",
  h2("GDP/Percent Contribution of every state to the US Economy and their diverse classifications"),
  sidebarLayout(
    sidebar_content_mohit,
    main_content
  )
)


ui <- navbarPage(
  title = "Diversity affecting US Economy",
  final_panel
)

# Jaimie Jin
  sidebar_content_time <- sidebarPanel(
    checkboxInput(
      inputId = "checkbox", 
      label = "View Individual Year", 
      value = FALSE),
  conditionalPanel(
    condition = "input.checkbox == false",
    sliderInput(
      inputId = "range",
      label = "Year",
      sep = "",
      min = 1997,
      max = 2016,
      value = c(1997, 2016)
    )
  ),
  conditionalPanel(
    condition="input.checkbox == true",
    sliderInput(
    inputId = "single",
    label = "Year",
    sep = "",
    min = 1997,
    max = 2016,
    value = 1997
    )
  )
)

main_content_time <- mainPanel(
  plotOutput(
    outputId = "plot_time"
  )
)

change_time <- tabPanel(
  title = "Change in GDP over time",
  h2("How has each state's GDP changed over time?"),
  p(strong("Author: "), "Jaimie Jin"),
  sidebarLayout(
    sidebar_content_time,
    main_content_time),
  h3("About the Plot"),
  p("This is  a choropleth map. When in the state of comparing GDP over different years, I used the GDP data, took the GDP in dollars from the higher year and subtracted it from the lower year and calculated the percent growth. 
    Meanwhile, for the individual year, I use the GDP data in dollars."),
  h3("Why does it matter?"),
  p("We can use rates of change to compare growth, which would help standardize any values we get. This would allow us to compare different states to each other more equally. In doing so, we can see which states are most affected by any overall GDP trends more effectively. We can use the GDP dataset to allow us to track this."),
  h3("What do we learn from the above graph?"),
  p("From the map, we can see how each state compares to each other. ", textOutput(outputId = "analysis_result"))
)


#Vishank
gdp_by_state_mutated_growth <- gdp_by_state %>% 
  select(NAME, GDP_in_dollars_2014:GDP_in_dollars_2016) %>% 
  mutate(growth_in_GDP = ((GDP_in_dollars_2016 - GDP_in_dollars_2014)/GDP_in_dollars_2014)*100)

mean_growth <- round(mean(gdp_by_state_mutated_growth$growth_in_GDP), 2)
mean_diversity <- round(mean(hate_crimes$share_non_white), 2)

max_gdp_growth <- ceiling(gdp_by_state_mutated_growth 
                          %>% filter ( growth_in_GDP == max(growth_in_GDP)) 
                          %>% pull(growth_in_GDP))
min_gdp_growth <- trunc(gdp_by_state_mutated_growth 
                        %>% filter ( growth_in_GDP == min(growth_in_GDP)) 
                        %>% pull(growth_in_GDP))

states <- c("State", gdp_by_state %>% pull(NAME))

sidebar_content_diversity <- sidebarPanel(
  p("Move the slider to limit the range for GDP Growth."),
  p(em("Note: The graph may take some time to load.")),
  sliderInput(
    inputId = "gdp_input",
    label = "GDP Growth",
    min = min_gdp_growth,
    max = max_gdp_growth,
    value = c(min_gdp_growth, max_gdp_growth)
  ),
  p("The next two inputs are to compare two states. *To deselect please choose 'State'"),
  p("Enter the first state you want to compare:"),
  selectInput(
    inputId = "state_one_choice",
    label = "State 1",
    choice = states,
    selected = "State"
  ),
  p("Enter the second state you want to compare:"),
  selectInput(
    inputId = "state_two_choice",
    label = "State 2",
    choice = states,
    selected = "State"
  ),
  p("*Please reset the GDP growth scale if both chosen states do not show")
)

main_content_diversity <- mainPanel(
  plotOutput(
    outputId = "plot_diversity"
  )
)


diversity_vs_GDP <- tabPanel(
  title = "Diversity vs. GDP",
  h2("How has diversity affected GDP growth by state?"),
  p(strong("Author"), ": Vishank Rughwani"),
  sidebarLayout(
    sidebar_content_diversity,
    main_content_diversity
  ),
  p("On average, most of the states with the highest GDP growth have a good amount of diversity. Most of the places with under 0.2 population diversity do have growth, however, it's is not as pronounced as the other states. 
    Although, this graph can be looked at from many perspectives and there is no objective answer to this question as of yet. For the most part the data seems to be inconclusive. This may be the case because there are a lot of different factors that effect GDP growth and I only took into account diversity. 
    On average though, the states GDP's have increased. The mean growth is ", strong(mean_growth), "while the mean diversity is ", strong(mean_diversity), "This shows an increase in GDP while there is diversity")
)

#Home page
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
     There is also data on voting percentages in each state."), 
     
  p("We are combining this dataset with another dataset that contains records on gross domestic product (GDP) by state from 1997 to 2016. 
    Although GDP is not a complete measure of economic wealth and productivity, it provides a universal measure of comparison. Through comparing both sets of data, we hope to determine what factors make certain locations more vulnerable to hate incidents. 
    In understanding this, we can possibly figure out how to prevent more hate incidents in the future."),
  
  p("Our first dataset comes from an open Github contributor called fivethirtyeight. The data is sourced from the Southern Poverty Law Center, a well-known nonprofit that specializes in monitoring hate crimes. 
     Although there are known instances of them mislabeling people/events, they are the best source for quickly released data on hate crimes, as FBI data tends to take many months to analyze.",
     a(href = "https://github.com/fivethirtyeight/data/tree/master/hate-crimes", em("(source)"))
    ),
  
  p("Our second dataset comes from ArcGIS HUB and was updated just last year. It is shared by user lisa_berry and puts down services.arcgis.com as the source. 
     The data itself is from US Bureau of Economic Analysis which is reputable as a agency that has better access to this type of data.",
     a(href = "https://hub.arcgis.com/datasets/a2a8826c4ba44f89a684000fe7c05b8c_0", em("(source)"))
    ),
  p("For more information on the data and a data report, please go ", a(href = "https://info201a-wi20.github.io/project-report-jinjaimie/", em("here")), ".")
  )


change_in_GDP_over_time <- tabPanel(
  title = "Change in GDP over time",
  h1("How has each state's GDP changed over time?"),
  sidebarLayout(
    sidebar_content_time,
    main_content_time
  )
)


ui <- navbarPage(title = "GDP and Hate Crimes",
                 theme = shinytheme("simplex"),
                 home, 
                 change_time, 
                 voter_panel,
                 final_panel, 
                 diversity_vs_GDP)

