library(shiny)
library(tidyverse)
library(DT)

# allLOs <- readRDS("./data/allLOs.rds")
# processed <- readRDS("./data/processed.rds")
# ASLOs <- readRDS("./data/ASLOs.rds")
LOs <- readRDS("./data/LOs.rds")

system_options <- unique(LOs$System)
cohorts <- unique(LOs$Cohort)
sessns <- unique(LOs$`Session Name`)
src <- levels(LOs$src)

ui <- fluidPage(
  titlePanel("HARC LOs mapping project"),
  tabsetPanel(
    tabPanel("HARC LOs",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput("sys", "System/Region", choices = system_options, multiple = FALSE),
                            selectInput("coh", "Cohort", choices = cohorts, multiple = FALSE)
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          h4("Select LOs to see new mapped LOs"),
                          dataTableOutput("tableOG")
                   ),
                   column(6,
                          h4("Mapped LOs"),
                          dataTableOutput("tableNew")
                   )
                 )
                 
               )
             )
    ),
    tabPanel("New LOs",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput("src", "Source", choices = src),
                            selectInput("sys2", "System/Region", choices = system_options, multiple = FALSE)
               ),
               mainPanel(width = 10,
                         fluidRow(width = 12,
                                  column(6,
                                         h4("Select new LOs to see HARC mapped LOs"),
                                         dataTableOutput("tableAS")
                                  ),
                                  column(6,
                                         h4("Mapped LOs"),
                                         dataTableOutput("tableHARC")
                                  )
                         )
               )
             )
    ),
    tabPanel("Plot")
  )
)



server <- function(input, output, session) {
  #### Tab 1: From HARC LOs ######
  selectedLOs <- reactive(LOs %>%
                            filter(System == input$sys & Cohort == input$coh) %>%
                            group_by(LO_ID) %>%
                            add_tally() %>%
                            ungroup() %>%
                            distinct(LO_ID, .keep_all = TRUE))
  
  # update cohort choices based on system selected
  observe({
    input$sys
    choices <- LOs %>%
      filter(System == input$sys) %>%
      distinct(Cohort)
    
    updateSelectInput(session, "coh",
                      choices = choices)
  })
  
  # HARC LO table
  output$tableOG <- DT::renderDataTable(selectedLOs() %>%
                                          select(LO_ID, LO_Description_OG, n), 
                                        options = list(pageLength = 10),
                                        rownames = FALSE,
                                        colnames = c('ID', 'Original LO', "n"),
                                        selection = 'single')
  
  # Get ID of selected LO 
  LO <- reactive({
    input$tableOG_rows_selected
    selectedLOs() %>%
      filter(row_number()==input$tableOG_rows_selected) %>%
      select(LO_ID)%>%
      pull(1)
  })
  
  # AS Los mapped to selected HARC LO
  output$tableNew <- DT::renderDataTable({
    req(input$tableOG_rows_selected)
    LOs %>%
      filter(LO_ID == LO()) %>%
      select(LOID, LO_Description, Notes)
  })
  
  ##### Tab 2: From AS Los #######
  selected2 <- reactive(LOs %>%
                          filter(src == input$src & System == input$sys2) %>%
                          group_by(LOID) %>%
                          add_tally() %>%
                          ungroup() %>%
                          distinct(LOID, .keep_all = TRUE))
  # AS LO table
  output$tableAS <- DT::renderDataTable(selected2() %>%
                                          select(LOID, LO_Description, n, Notes), 
                                        options = list(pageLength = 10),
                                        rownames = FALSE,
                                        colnames = c('ID', 'Description', "n","Notes"),
                                        selection = 'single')
  # Get ID of selected LO 
  LO2 <- reactive({
    input$tableAS_rows_selected
    selected2() %>%
      filter(row_number()==input$tableAS_rows_selected) %>%
      select(LOID)%>%
      pull(1)
  })
  
  # AS Los mapped to selected HARC LO
  output$tableHARC <- DT::renderDataTable({
    req(input$tableAS_rows_selected)
    LOs %>%
      filter(LOID == LO2()) %>%
      select(LO_ID, LO_Description_OG)
  })
  
  
}

shinyApp(ui, server)