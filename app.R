library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)

source("R/data_load.R")
source("R/filters.R")
source("R/validation.R")

mfox <- load_mfox_data()
validation_errors <- validate_mfox(mfox)

theme <- bs_theme(
  version = 5,
  bg = "#FFFDFC",
  fg = "#40131F",
  primary = "#9F203D",
  secondary = "#E98478"
)

ui <- page_navbar(
  title = div(
    style="display:flex;align-items:center;gap:10px;",
    tags$img(src="mfox_logo.png", height="42px"),
    div(tags$strong("MFOX"), tags$small(" Menstrual Fluid Omics Explorer"))
  ),
  theme = theme,
  header = tagList(
    tags$link(rel="stylesheet", type="text/css", href="mfox.css"),
    tags$style(HTML("
      .mfox-card {border:1px solid #f0d9de;border-radius:14px;padding:18px;background:white;}
      .metric {font-size:2rem;font-weight:700;color:#8f1d39;}
      .muted {color:#78666b;}
    "))
  ),

  nav_panel("Landscape",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("scope", "Specimen scope",
          c("All menstrual-derived evidence",
            "Direct menstrual fluid only",
            "Cultured menstrual-derived cells",
            "Experimental derivatives")),
        selectInput("landscape_measure", "Cell value",
          c("Assays"="assays","Studies"="studies")),
        helpText("Empty cells mean no eligible record is currently represented in this MFOX version.")
      ),
      card(
        card_header("Clinical context × omics modality"),
        plotlyOutput("landscape_plot", height="620px")
      )
    )
  ),

  nav_panel("Explore",
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("explore_population","Population / condition", choices=NULL, multiple=TRUE),
        selectizeInput("explore_omics","Omics modality", choices=NULL, multiple=TRUE),
        selectizeInput("explore_biospecimen","Biospecimen class", choices=NULL, multiple=TRUE),
        checkboxInput("public_only","Only assays with an indexed public dataset", FALSE),
        actionButton("clear_filters","Clear filters")
      ),
      card(
        card_header("Studies and assays"),
        DTOutput("explore_table")
      )
    )
  ),

  nav_panel("Gaps",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("gap_row","Rows",
          c("Population / condition"="condition","Population category"="population_category",
            "Biospecimen class"="biospecimen_class","Derivation class"="derivation_class")),
        selectInput("gap_col","Columns",
          c("Omics modality"="omics_modality","Biospecimen class"="biospecimen_class",
            "Derivation class"="derivation_class")),
        helpText("This is an evidence-density map, not a ranking of research importance.")
      ),
      card(
        card_header("Evidence coverage"),
        plotlyOutput("gap_plot", height="620px")
      )
    )
  ),

  nav_panel("Plan a Study",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("plan_condition","Clinical context", choices=NULL),
        selectInput("plan_omics","Omics modality", choices=NULL),
        selectInput("plan_biospecimen","Biospecimen class", choices=NULL),
        selectInput("plan_longitudinal","Longitudinal design", c("Any","Yes","No")),
        actionButton("plan_run","Find related evidence", class="btn-primary")
      ),
      uiOutput("plan_summary"),
      card(card_header("Closest MFOX records"), DTOutput("plan_table"))
    )
  ),

  nav_panel("Data",
    card(
      card_header("Indexed public datasets"),
      DTOutput("dataset_table")
    )
  ),

  nav_panel("New & Unreviewed",
    card(
      card_header("Candidate queue"),
      p(class="muted","Automated discovery should write here first. Candidate records are not part of the curated landscape until reviewed."),
      DTOutput("candidate_table")
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class="muted", paste0("MFOX prototype • ", nrow(mfox$studies), " studies")))
)

server <- function(input, output, session) {
  observe({
    ev <- mfox$evidence
    updateSelectizeInput(session,"explore_population", choices=nz_choices(c(ev$condition,ev$population_category)), server=TRUE)
    updateSelectizeInput(session,"explore_omics", choices=nz_choices(ev$omics_modality), server=TRUE)
    updateSelectizeInput(session,"explore_biospecimen", choices=nz_choices(ev$biospecimen_class), server=TRUE)
    updateSelectInput(session,"plan_condition", choices=c("Any",nz_choices(c(ev$condition,ev$population_category))))
    updateSelectInput(session,"plan_omics", choices=c("Any",nz_choices(ev$omics_modality)))
    updateSelectInput(session,"plan_biospecimen", choices=c("Any",nz_choices(ev$biospecimen_class)))
  })

  scoped <- reactive(apply_scope(mfox$evidence, input$scope))

  output$landscape_plot <- renderPlotly({
    d <- scoped() |> mutate(context=coalesce(condition,population_category,"Not reported"))
    if (input$landscape_measure=="studies") {
      z <- d |> distinct(study_id,context,omics_modality) |> count(context,omics_modality,name="n")
    } else {
      z <- d |> count(context,omics_modality,name="n")
    }
    req(nrow(z)>0)
    p <- ggplot(z,aes(x=omics_modality,y=context,fill=n,
                      text=paste0("Context: ",context,"<br>Omics: ",omics_modality,"<br>N: ",n))) +
      geom_tile(color="white",linewidth=.8) +
      geom_text(aes(label=n),size=4) +
      labs(x=NULL,y=NULL,fill="N") +
      theme_minimal(base_size=12) +
      theme(axis.text.x=element_text(angle=35,hjust=1),panel.grid=element_blank())
    ggplotly(p,tooltip="text")
  })

  explore_filtered <- reactive({
    d <- scoped()
    if (length(input$explore_population)) d <- d |> filter(condition %in% input$explore_population | population_category %in% input$explore_population)
    if (length(input$explore_omics)) d <- d |> filter(omics_modality %in% input$explore_omics)
    if (length(input$explore_biospecimen)) d <- d |> filter(biospecimen_class %in% input$explore_biospecimen)
    if (isTRUE(input$public_only)) d <- d |> filter(assay_id %in% mfox$datasets$assay_id)
    d
  })

  observeEvent(input$clear_filters,{
    updateSelectizeInput(session,"explore_population",selected=character())
    updateSelectizeInput(session,"explore_omics",selected=character())
    updateSelectizeInput(session,"explore_biospecimen",selected=character())
    updateCheckboxInput(session,"public_only",value=FALSE)
  })

  output$explore_table <- renderDT({
    explore_filtered() |>
      select(study_id,year,title,condition,omics_modality,biospecimen_class,
             derivation_class,platform,longitudinal,source_url) |>
      distinct() |>
      datatable(filter="top", options=list(pageLength=15,scrollX=TRUE))
  })

  output$gap_plot <- renderPlotly({
    d <- scoped()
    req(input$gap_row != input$gap_col)
    z <- d |> mutate(.row=coalesce(.data[[input$gap_row]],"Not reported"),
                     .col=coalesce(.data[[input$gap_col]],"Not reported")) |>
      distinct(study_id,.row,.col) |> count(.row,.col,name="n")
    req(nrow(z)>0)
    p <- ggplot(z,aes(x=.col,y=.row,fill=n,
                      text=paste0(.row," × ",.col,"<br>Studies: ",n))) +
      geom_tile(color="white",linewidth=.8) + geom_text(aes(label=n),size=4) +
      labs(x=NULL,y=NULL,fill="Studies") + theme_minimal(base_size=12) +
      theme(axis.text.x=element_text(angle=35,hjust=1),panel.grid=element_blank())
    ggplotly(p,tooltip="text")
  })

  plan_results <- eventReactive(input$plan_run,{
    d <- scoped()
    if (!is.null(input$plan_condition) && input$plan_condition!="Any")
      d <- d |> filter(condition==input$plan_condition | population_category==input$plan_condition)
    if (!is.null(input$plan_omics) && input$plan_omics!="Any")
      d <- d |> filter(omics_modality==input$plan_omics)
    if (!is.null(input$plan_biospecimen) && input$plan_biospecimen!="Any")
      d <- d |> filter(biospecimen_class==input$plan_biospecimen)
    if (!is.null(input$plan_longitudinal) && input$plan_longitudinal!="Any")
      d <- d |> filter(longitudinal==input$plan_longitudinal)
    d
  }, ignoreInit=FALSE)

  output$plan_summary <- renderUI({
    d <- plan_results()
    nstud <- n_distinct(d$study_id); nassay <- n_distinct(d$assay_id)
    ndat <- mfox$datasets |> filter(assay_id %in% d$assay_id) |> nrow()
    div(class="mfox-card",
      h3("Current MFOX coverage"),
      fluidRow(
        column(4,div(class="metric",nstud),p("studies")),
        column(4,div(class="metric",nassay),p("assays")),
        column(4,div(class="metric",ndat),p("indexed datasets"))
      ),
      if (nstud==0) p("No eligible record matching this exact combination is currently represented in this MFOX version.")
    )
  })

  output$plan_table <- renderDT({
    plan_results() |> select(study_id,year,title,condition,omics_modality,biospecimen_class,study_design,longitudinal) |>
      distinct() |> datatable(options=list(pageLength=10,scrollX=TRUE))
  })

  output$dataset_table <- renderDT({
    mfox$datasets |>
      left_join(mfox$studies |> select(study_id,title,year),by="study_id") |>
      left_join(mfox$assays |> select(assay_id,omics_modality,biospecimen_class),by="assay_id") |>
      select(dataset_id,year,title,omics_modality,biospecimen_class,repository,accession,
             raw_available,processed_available,metadata_available,code_available,dataset_url) |>
      datatable(filter="top",options=list(pageLength=15,scrollX=TRUE))
  })

  output$candidate_table <- renderDT({
    mfox$candidates |> datatable(filter="top",options=list(pageLength=15,scrollX=TRUE))
  })

  if (length(validation_errors)) {
    showNotification(paste(validation_errors, collapse=" | "), type="error", duration=NULL)
  }
}

shinyApp(ui, server)
