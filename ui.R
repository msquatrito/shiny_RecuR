# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define UI for application 
shinyUI(
  navbarPage(title = strong("RecuR"), windowTitle = "RecuR - Visualization Tools for Recurrent Glioma Datasets", 
             fluid = TRUE, theme = shinytheme("cosmo"), footer = includeHTML("tools/footer.html"),
             
             # source("tabs/homeTab.R", local = TRUE)$value,
             source("tabs/exploreTab.R", local = TRUE)$value,
             source("tabs/aboutTab.R", local = TRUE)$value
             
  )
)
