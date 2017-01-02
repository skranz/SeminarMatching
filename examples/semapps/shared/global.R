library(shinyEvents)
app = eventsApp()
app$ui = tags$p("Access forbidden.")
appReadyToRun(app)