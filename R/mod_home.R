# R/mod_home.R
library(shiny)

home_ui <- function(id){
  ns <- NS(id)
  
  div(
    id = ns("home_root"),
    class = "home2",
    
    # HERO bandeau full-width
    div(
      class = "home2-hero",
      div(
        class = "home2-hero-inner",
        h1("GlobAgri Africa 2050"),
        p("Interactive exploration of model results across African countries")
      )
    ),
    
    # Contenu (responsive)
    div(
      class = "home2-wrap",
      
      # 2 boutons (segmented)
      div(
        class = "home2-switch",
        radioButtons(
          ns("section"),
          label = NULL,
          choices = c(
            "What is this application" = "what",
            "Scenarios" = "scenarios",
            "How to use" = "how"
          ),
          selected = "what",
          inline = TRUE
        )
        
      ),
      
      # Contenu conditionnel
      uiOutput(ns("section_ui"))
    )
  )
}

home_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    output$section_ui <- renderUI({
      
      if (identical(input$section, "what")) {
        
        tagList(
          tags$br(),
          div(
            p(
              class = "home2-what-text",
              "This web application provides an interactive, country-by-country exploration of six 2050
              agricultural scenarios in Africa. It helps interpret how different diet scenarios and constraints 
              could shape agricultural balances, land use, trade dependency, and emissions between 2018 (base year) and 2050."
            ),
            
            # Placeholder image (you'll replace src later)
            div(
              class = "home2-what-media",
              tags$img(
                src = "fond.jpg",   # <- you will provide the real file later
                alt = "Application overview",
                class = "home2-what-img"
              )
              )
            )
          )
        
      } else if (identical(input$section, "how")) {
        
        # HOW TO USE view (inchangé)
        tagList(
          tags$br(),
          div(
            class = "home2-card",
            h2(class = "home2-card-title", "How to use the application ?"),
            tags$ol(
              class = "home2-ol",
              tags$li(tags$strong("Select a country (top banner)"), tags$br(),
                      span("The selected country acts as a global filter and is automatically applied to all charts.")),
              tags$li(tags$strong("Select an additional scenario (top banner)"), tags$br(),
                      span("The three diet scenarios are always displayed; you can add one derived scenario via the selector.")),
              tags$li(tags$strong("Navigate by tabs"), tags$br(),
                      span("Each tab provides a dedicated view of results (assumptions, synthesis, crops, livestock, land use, trade, emissions).")),
              tags$li(tags$strong("Use chart interactivity"), tags$br(),
                      span("Hover values, legend toggles (show/hide series), zoom/filters when available. Download data via the chart menu."))
            )
          ),
          div(
            class = "home2-card",
            div(class="home2-divider"),
            h2(class = "home2-card-title", "Tabs at a glance"),
            p(tags$strong("Assumptions:"), " diets and yields hypothesis"),
            p(tags$strong("Big picture:"), " main results on land use, balance and dependancy"),
            p(tags$strong("Crops:"), " area harvested and crop balance"),
            p(tags$strong("Livestock:"), " land requirement, stocks, livestock systems and animal products balance"),
            p(tags$strong("Land use:"), " change in land use and forest land evolution"),
            p(tags$strong("Balance:"), " flows of agricultural product by ressources and uses"),
            p(tags$strong("Dependancy:"), " food and agricultural dependancy indicators"),
            p(tags$strong("Emissions:"), " GHG emissions"),
            p(tags$strong("Continent:"), " maps at the continent scale"),
            p(tags$strong("About:"), " definitions, methodological notes, raw data."),
          ),
          div(class="home2-divider"),
          tags$div(style = "height:18px;"),
          div(
            class = "home2-footer",
            p(class="home2-muted",
              "Questions about the model, scenarios, or results? We would also be happy to work with you on new GlobAgri scenarios."
            ),
            tags$a(class="home2-btn home2-btn-primary", href="mailto:faivredupaigreb@afd.fr", "Email contact")
          )
        )
        
      } else {
        
        # SCENARIOS view (inchangé)
        tagList(
          tags$br(),
          div(
            class = "home2-card",
            h2(class = "home2-card-title", "Scenarios 2050"),
            p(
              "Six scenarios were constructed to model potential tensions on the agricultural and food systems of 45 African countries. ",
              "There are three basic scenarios (",
              tags$em("Same diet"), ", ", tags$em("Healthy diet"), ", ", tags$em("Likely diet"),
              ") in which three potential diets are projected (described in the hypotheses tab). ",
              "Then, three other scenarios were derived from the diet of the ", tags$em("likely"), " diet scenario by applying different constraints (",
              tags$em("Total area stress"), ", ", tags$em("No deforestation"), ", ", tags$em("Self-sufficiency"),
              ")."
            ),
            tags$br(),
            tags$details(
              class = "home2-acc",
              tags$summary(class="home2-acc-sum", "More on derived scenarios (applied to the likely diet)"),
              tags$ul(
                tags$li(tags$strong("No deforestation:"), " no forest area can be converted into agricultural area. Any additional need for
                        agricultural land is met by imports"),
                tags$li(tags$strong("Total area stress:"), " agricultural areas cannot exceed the country’s available area (seen as cropland + pastures and 
                        meadows + forest land). Any additional need for agricultural land is met by imports"),
                tags$li(tags$strong("Self-sufficiency:"), " cereals and legumes import rates capped at 20% (or base-year level if already above 20%).")
              )
            ),
            tags$details(
              class = "home2-acc",
              tags$summary(class="home2-acc-sum", "Assumptions common to all scenarios"),
              tags$ul(
                tags$li("The intensities of cultivation are maintained constant at the national level at their base year level: 2018."),
                tags$li("The demographic projections are extracted from the 2022 data of the United Nations median scenario"),
                tags$li("Yiels are projected thanks to studies from Muller and al. (2012) and from Mueller and Robertson (2014) for the effect of climate change."),
                tags$li("Agricultural losses are kept constant at the national level at their base year level: 2018."),
                tags$li("Import rates are kept constant by product and by country at the level of the model’s base year: 2018 (except the derived scenarios)."),
                tags$li("Export shares by country are held constant at the global level at base year level: 2018.")
              )
            )
          ),
          div(class="home2-divider"),
          tags$div(style = "height:18px;"),
          div(
            class = "home2-footer",
            p(class="home2-muted",
              "Questions about the model, scenarios, or results ? We would also be happy to work with you on new GlobAgri scenarios."
            ),
            tags$a(class="home2-btn home2-btn-primary", href="mailto:faivredupaigreb@afd.fr", "Email contact")
          )
        )
      }
    })
    
    
  })
}
