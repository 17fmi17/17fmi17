library(shiny)

today <- Sys.Date()
ui <- fluidPage(

  # App title ----
  titlePanel("Calendar"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      dateInput("date", 
               label = "日付", 
               value = today
      )   
    ),
    mainPanel(
      textOutput(outputId = "dairy"),
      textOutput(outputId = "holiday"),
      textOutput(outputId = "three_daysOff")
    )
  )
)
