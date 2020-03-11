library("shiny")

voter_choices <- radioButtons(inputId = "voter_values", 
             label = "Possible comparison values",
             choices = list("Median household income (in $10,000s)" = "median_household_income", 
                                "Percent of population unemployed (seasonally adjusted)" = "share_unemployed_seasonal",
                                "Percent of population in metro areas" = "share_population_in_metro_areas",
                                "Percent of adults 25 and older with a high school degree" = "share_population_with_high_school_degree",
                                "Percent of population that are not citizens" = "share_non_citizen",
                                "Percent of white residents living in poverty" = "share_white_poverty",
                                "Gini index (an inequality coefficient)" = "gini_index",
                                "Percent of population that is not white" = "share_non_white",
                                "Change in GDP, 1997-2016" = "gdp_1997_2016",
                                "Change in GDP, 2008-2016" = "gdp_2008_2016"),
             selected = "median_household_income"
)

voter_panel <- tabPanel(
  title = "Voting Factors",
  titlePanel("What factors affect how people vote?"),
  sidebarLayout(
    sidebarPanel(
      voter_choices
    ),
    mainPanel(
      plotOutput(outputId = "voter_plot")
    )
  )
)

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

main_content <- mainPanel(
  plotOutput(
    outputId = "plot"
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


ui <- navbarPage(title = "GDP and Hate Crimes",
                 change_in_GDP_over_time, voter_panel)

