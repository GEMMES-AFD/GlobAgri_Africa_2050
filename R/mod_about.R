# R/mod_about.R
library(shiny)
library(dplyr)
library(readr)

mod_about_ui <- function(id){
  ns <- NS(id)
  
  div(
    id = ns("about_root"),
    class = "home2",  # important: réutilise exactement le CSS Home2
    
    # HERO (même structure que Home2)
    div(
      class = "home2-hero",
      div(
        class = "home2-hero-inner",
        tags$h1("Data and model"),
        tags$p("Model description, simplified diagram, and downloadable methodology document")
      )
    ),
    
    # WRAP
    div(
      class = "home2-wrap",
      
      # SECTION 1 — Model
      div(
        class = "home2-card home2-card-light",
        tags$h2(class = "home2-card-title", "GlobAgri model"),
        
        tags$p(
          "GlobAgri is a model of biomass balance, in physical units, which at the world level ensures for each agricultural product considered ",
          "the balance between its supply and its different uses. The model defines a balance such that domestic production plus imports equalize ",
          "the sum of uses :"
        ),
        
        tags$p(
          tags$strong(
            tags$code("Production + imports = Food + Feed + Seed + Other non-food uses + Losses")
          )
        ),
        
        tags$p(
          "The original model was built on 21 regions worldwide, including 3 large regions that together represented Africa and 18 other regions ",
          "for the rest of the world. With AFD’s support, these three African regions were disaggregated into 45 African countries, so the version ",
          "of the model used in this study now consists of the 45 African countries plus the remaining 18 world regions. Furthermore, the modelling is ",
          "carried out to 2050 starting from a base year of 2018 (before Covid, which disrupted trade and was slow to recover)."
        ),
        
        tags$p(
          "If you want to know more about the model, visit the model website : ",
          tags$a(href = "https://globagri.org/", target = "_blank", rel = "noopener", "globagri.org"),
          ". You can also consult the methodological paper: ",
          tags$a(href = "https://agritrop.cirad.fr/588822/", target = "_blank", rel = "noopener", "agritrop.cirad.fr"),
          "."
        )
      ),
      
      tags$div(class = "home2-divider"),
      
      # SECTION 2 — Diagram (carte claire + image responsive)
      tags$div(style = "height:18px;"),
      
      div(
        class = "home2-card home2-card-light",
        tags$h2(class = "home2-card-title", "Simplified diagram of the model"),
        
        tags$img(
          src = "schema_globagri.png",
          alt = "Simplified GlobAgri diagram",
          class = "about2-diagram-img"
        ),
        
        tags$div(style = "height:12px;"),
        
        tags$p(
          "The diagram distinguishes the main components of the GlobAgri system and the direction of causality. Green boxes and arrows represent ",
          "exogenous assumptions (inputs) fixed by the scenario framework (population, diets, climate change). These inputs determine food demand, which ",
          "is then translated into crop and livestock production requirements. Blue and red boxes represent model outputs (results): yields, land use ",
          "(cropland and pasture) and trade flows (imports/exports) are jointly adjusted to balance supply and demand, and selected emissions indicators ",
          "are derived from these production and land-use outcomes."
        )
      ),
      
      tags$div(class = "home2-divider"),
      tags$div(style = "height:18px;"),
      
      # SECTION 3 — Download (carte claire + boutons)
      div(
        class = "home2-card home2-card-light",
        tags$h2(class = "home2-card-title", "Download"),
        tags$p("Download the definitions and standards or the full dataset :"),
        tags$div(style = "height:12px;"),
        
        # Ligne de boutons (PDF + data)
        div(
          class = "about2-download-row",
          tags$a(
            class = "home2-btn home2-btn-primary",
            href = "Definitions_and_standards.pdf",
            target = "_blank",
            rel = "noopener",
            "PDF"
          ),
          downloadButton(ns("dl_fact_csv"), "Data (.csv)", class = "home2-btn home2-btn-primary")
        )
      )
    )
  )
}

# fact peut être un data.frame OU une reactive() qui renvoie un data.frame
mod_about_server <- function(id, fact){
  moduleServer(id, function(input, output, session){
    
    .get_fact <- function(x) if (is.function(x)) x() else x
    
    output$dl_fact_rds <- downloadHandler(
      filename = function() paste0("fact_", Sys.Date(), ".rds"),
      content  = function(file){
        df <- .get_fact(fact)
        saveRDS(df, file, compress = "xz")
      }
    )
    
    output$dl_fact_csv <- downloadHandler(
      filename = function() paste0("fact_", Sys.Date(), ".csv"),
      content  = function(file){
        df <- .get_fact(fact)
        df <- dplyr::mutate(df, dplyr::across(where(is.factor), as.character))
        df <- dplyr::mutate(df, dplyr::across(
          where(\(x) inherits(x, "list")),
          ~purrr::map_chr(., jsonlite::toJSON, auto_unbox = TRUE, null = "null")
        ))
        readr::write_delim(df, file, delim = ";", na = "", escape = "double")
      }
    )
  })
}
