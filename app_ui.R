library("shiny")

sidebar_content <- sidebarPanel(
  sliderInput(
    inputId = "year",
    label = "Year",
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