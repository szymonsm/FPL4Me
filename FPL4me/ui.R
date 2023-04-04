#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "FPL4me"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Players", tabName = "players", icon = icon("user-friends")),
        menuItem("Clubs", tabName = "clubs", icon = icon("users-class")),
        menuItem("Positions", tabName = "positions", icon = icon("arrows-alt"))
      )
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
       .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
      tabItems(
        # First tab content
        tabItem(tabName = "players",
                h2("Players"),
                fluidRow(
                  column(
                    width = 6,
                    box(selectInput("player",
                                    label = "Choose player:",
                                    sort(unique(merged_gw$name))),
                        width = NULL,
                        status = "danger",
                        height = 90),
                    box(checkboxInput("formTrend",
                          label = "Form trend"),
                        plotOutput("formPlot"),
                        width = NULL,
                        status = "primary",
                        title = "Player form",
                        solidHeader = TRUE),
                  ),
                  column(
                    width = 6,
                    infoBoxOutput("correlation"),
                    infoBoxOutput("vpc"),
                    infoBoxOutput("ownership"),
                    box(DT::dataTableOutput("corrtab"),
                        title = "ICT-PTS corr",
                        solidHeader = TRUE,
                        status = "info",
                        width = 4),
                    box(DT::dataTableOutput("topvpc"),
                        title = "Most valuable players",
                        solidHeader = TRUE,
                        status = "warning",
                        width = 4),
                    box(DT::dataTableOutput("topttop"),
                        title = "Most minutes",
                        solidHeader = TRUE,
                        status = "success",
                        width = 4)
                  )
                ),
                fluidRow(
                  box(checkboxInput("priceTrend",
                                    label = "Price trend"),
                      plotOutput("pricePlot"),
                      status = "primary",
                      title = "Market value",
                      solidHeader = TRUE,
                      width = 6),
                  box(checkboxInput("xPTrend",
                                    label = "xP trend"),
                      plotOutput("xPPlot"),
                      width = 6,
                      status = "primary",
                      title = "xP Trend",
                      solidHeader = TRUE)
                )
        ),
        
        # Second tab content
        tabItem(tabName = "clubs",
                h2("Clubs"),
                fluidRow(
                  box(checkboxGroupInput("clubs",
                                     "Select clubs:",
                                     choices = sort(unique(merged_gw$team)),
                                     inline = TRUE),
                      width = 12,
                      )
                  ),
                fluidRow(
                  box(tableOutput("valueRank"),
                      width = 4,
                      title = "Top 10 valuable players",
                      solidHeader = TRUE),
                  box(tableOutput("informRank"),
                      width = 4,
                      title = "Top 10 inform players",
                      solidHeader = TRUE),
                  box(tableOutput("minutesRank"),
                      width = 4,
                      title = "Top 10 players with the most minutes",
                      solidHeader = TRUE)
                )
        )
      )
    )
  )
)
