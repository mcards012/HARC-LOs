# rsconnect::writeManifest()
library(shiny)
library(tidyverse)
library(DT)
library('igraph')
library(networkD3)


# allLOs <- readRDS("./data/allLOs.rds")
# processed <- readRDS("./data/processed.rds")
# ASLOs <- readRDS("./data/ASLOs.rds")
LOs <- readRDS("./data/LOs.rds")
tmp1 <- LOs %>%
  select(LO_ID, LO_Description_OG)
tmp2 <- LOs %>%
  select(LOID, LO_Description)
names(tmp1) <- names(tmp2)
descriptions <- rbind(tmp1,tmp2)
rm(tmp1,tmp2)

ColourScale <- 'd3.scaleOrdinal()
            .domain(["LOID", "Source", "LO_ID"])
           .range(["#FF6900", "#498c55","#694489"]);'

system_options <- unique(LOs$System)
cohorts <- unique(LOs$Cohort)
sessns <- unique(LOs$`Session Name`)
src <- levels(LOs$src)

## functions ##
forPlot <- function(df){
  tmp1 <- df %>%
    select(LOID, LO_ID)
  tmp2 <- df %>%
    select(src, LOID)
  names(tmp1) <- c("Parent", "Child")
  names(tmp2) <- names(tmp1)
  data <- rbind(tmp1, tmp2)
  return(data)
}

getGroups <- function(df){
  tmp4 <- df %>%
    distinct(LOID) %>%
    mutate(grps = "LOID")
  tmp5 <- df %>%
    distinct(src) %>%
    mutate(grps = "Source")
  tmp6 <- df %>%
    distinct(LO_ID) %>%
    mutate(grps = "LO_ID")
  names(tmp4)[1] <- "ID"
  names(tmp5)[1] <- "ID"
  names(tmp6)[1] <- "ID"
  
  grps <- rbind(tmp4,tmp5,tmp6)
  rm(tmp4,tmp5,tmp6)
  
  grps <-grps %>%
    distinct(ID, .keep_all = T)
  
  g <- as.character(grps$grps)
  names(g) <- grps$ID
  return(g)
}

ui <- fluidPage(
  titlePanel("HARC LOs mapping project"),
  tabsetPanel(
    tabPanel("HARC LOs",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput("sys", "System/Region", choices = system_options, multiple = FALSE),
                            selectInput("coh", "Cohort", choices = cohorts, multiple = FALSE)
               ),
               mainPanel(width = 10,
                 fluidRow(
                   column(6,
                          h4("Select LOs"),
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
                         fluidRow(
                                  column(6,
                                         h4("Select LOs to see HARC mapped LOs"),
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
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(width = 2,
                            selectInput("src2", "Syllabus", choices = src, selected = "AS-Medicine"),
                            h5("Orange: New LOs"),
                            h5("Purple: Original LOs")),
               mainPanel(width = 10,
                         h3("Select a node to see LO description"),
                         fluidRow(
                           column(6,
                             forceNetworkOutput("force")
                           ),
                           column(4,
                                  h5("LO description:"),
                                  textOutput("desc")
                           )
                         )
                         )
             )
             )
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
  
  #### Tab 3: plot #######
  plotLOs <- reactive(LOs %>% filter(src == input$src2))
  
  plt_d3 <- reactive({
    input$src2
    igraph_to_networkD3(graph_from_data_frame(forPlot(plotLOs()), directed = T), group = getGroups(plotLOs()))
    
  })
  
  output$force <- renderForceNetwork({
    req(input$src2)
    forceNetwork(Links = plt_d3()$links, Nodes = plt_d3()$nodes,
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', zoom = T, opacity = 0.8, colourScale = ColourScale,
                 clickAction = 'Shiny.onInputChange("id", d.name)')
  })
  
  output$desc <- renderText({ 
    req(input$id)
    descriptions %>%
      unique() %>%
      filter(LOID == input$id) %>%
      pull(LO_Description)
      
  })

}

shinyApp(ui, server)