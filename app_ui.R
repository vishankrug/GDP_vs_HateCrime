library("shiny")

<<<<<<< HEAD
ui <- navbarPage(title = "")

voter_panel <- tabPanel(
  title = "Voting",
  titlePanel("What factors affect how people vote?"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "voter_values", 
                   label = "Possible comparison values",
                   choiceNames = list("Median household income", 
                              "Percent of population unemployed (seasonally adjusted)",
                              "Percent of population in metro areas",
                              "Percent of adults 25 and older with a high school degree",
                              "Percent of population that are not citizens",
                              "Percent of white residents living in poverty",
                              "Gini index (an inequality coefficient)",
                              "Percent of population that is not white",
                              "Change in GDP, 1997-2016",
                              "Change in GDP, 2008-2016"),
                   choiceValues = list("median_household_income",
                                   "share_unemployed_seasonal",
                                   "share_population_in_metro_areas",
                                   "share_population_with_high_school_degree",
                                   "share_non_citizen",
                                   "share_white_poverty",
                                   "gini_index",
                                   "share_non_white",
                                   "gdp_1997_2016",
                                   "gdp_2008_2016")
                   )
    ),
    mainPanel(
      plotOutput(outputId = "voter_plot")
    )
  )
  
)
=======
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
                 change_in_GDP_over_time)
>>>>>>> master
