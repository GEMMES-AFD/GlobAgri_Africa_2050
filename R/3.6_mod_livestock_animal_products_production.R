# R/3.6_mod_livestock_animal_products_production.R
# -------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------------------------------------------------
# UI
# -------------------------------------------------
mod_livestock_animal_products_prod_ui <- function(id, height = "480px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      h2("Livestock production by scenario (tons)"),
      tags$div(style="height:15px"),
      div(
        class = "row",
        div(
          class = "col-sm-6",
          tags$div(
            class = "u-input-title",
            tags$strong("Select animal group :")
          ),
          tags$div(style="height:8px"),
          selectInput(
            ns("grp"),
            label = NULL,
            choices = c(
              "Ruminants"    = "ruminants",
              "Monogastrics" = "monogastrics"
            ),
            selected = "ruminants"
          )
        )
      ),
      
      plotly::plotlyOutput(ns("plot"), height = height, width = "100%"),
      
      tags$br(),
      div(
        class = "text-right",
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_csv"),
            label = tagList(icon("download"), "CSV")
          )
        )
      ),
      tags$br(),
      uiOutput(ns("note"))
    )
  )
}

# -------------------------------------------------
# SERVER
# -------------------------------------------------
mod_livestock_animal_products_prod_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,
    ...
){
  moduleServer(id, function(input, output, session){
    
    shiny::validate(
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module."),
      shiny::need(is.data.frame(fact), "fact must be a data.frame.")
    )
    
    # Base year scenario code
    scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)
    } else {
      scenario_code("Année de base")
    }
    
    # Extra label -> code mapping (if needed)
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      setNames(
        unname(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)),
        names(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE))
      )
    } else {
      NULL
    }
    
    norm_scenario <- function(x){
      x <- scenario_code(x)
      if (!is.null(EXTRA_LABEL_TO_CODE)) {
        x <- dplyr::recode(x, !!!EXTRA_LABEL_TO_CODE, .default = x)
      }
      x
    }
    
    is_empty <- function(x){
      xx <- stringr::str_squish(as.character(x))
      is.na(xx) | xx == ""
    }
    
    parse_num_safe <- function(x){
      if (is.numeric(x)) return(x)
      x_chr <- as.character(x)
      out <- suppressWarnings(as.numeric(x_chr))
      if (anyNA(out)) {
        out <- readr::parse_number(
          x_chr,
          locale = readr::locale(decimal_mark = ",", grouping_mark = " ")
        )
      }
      out
    }
    
    # -------------------------------------------------
    # Keys for bindCache
    # -------------------------------------------------
    scen_show_key <- shiny::reactive({
      sc <- norm_scenario(r_scenarios())
      paste(sc, collapse = "|")
    })
    
    grp_key <- shiny::reactive({
      input$grp %||% "ruminants"
    })
    
    # -------------------------------------------------
    # Scenario levels effective (present in data after filters)
    # -------------------------------------------------
    scen_levels_effective <- shiny::reactive({
      req(r_country())
      
      wanted <- r_scenarios()
      shiny::validate(shiny::need(!is.null(wanted) && length(wanted) > 0, "No scenarios provided by r_scenarios()."))
      wanted <- norm_scenario(wanted)
      
      lvls_default <- norm_scenario(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
      
      # Required columns (based on your screenshot)
      shiny::validate(
        shiny::need(all(c("Region","Scenario","Element","Item","Year","Animal","Unit","Value") %in% names(fact)),
                    "Missing required columns in fact (expected Region, Scenario, Element, Item, Year, Animal, Unit, Value).")
      )
      
      present <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          .data$Element == "Production",
          .data$Year %in% c(2018, 2050),
          !is_empty(.data$Animal)
        ) %>%
        dplyr::mutate(Scenario = norm_scenario(.data$Scenario)) %>%
        dplyr::pull("Scenario") %>%
        unique()
      
      lvls_default[lvls_default %in% wanted & lvls_default %in% present]
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # Data filtered (Production only, Animal non-empty)
    # -------------------------------------------------
    data_filtered <- shiny::reactive({
      req(r_country())
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No scenarios available for this country (after filters)."))
      
      fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          .data$Element == "Production",
          .data$Year %in% c(2018, 2050),
          !is_empty(.data$Animal)
        ) %>%
        dplyr::mutate(
          Scenario = norm_scenario(.data$Scenario),
          Scenario = factor(.data$Scenario, levels = lvls),
          Item     = stringr::str_squish(as.character(.data$Item)),
          Unit     = as.character(.data$Unit),
          Value    = parse_num_safe(.data$Value)
        ) %>%
        dplyr::filter(.data$Scenario %in% lvls) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # Item grouping (ruminants vs monogastrics)
    # -------------------------------------------------
    RUM_ITEMS_CANON <- c("Bovine meat", "Small ruminants meat", "Dairy")
    
    item_canon <- function(x){
      x <- stringr::str_squish(as.character(x))
      # tolerant recodes
      x <- dplyr::recode(
        x,
        "Bovin meat"            = "Bovine meat",
        "Small ruminant meat"   = "Small ruminants meat",
        .default = x
      )
      x
    }
    
    data_summarised <- shiny::reactive({
      df <- data_filtered()
      req(nrow(df) > 0)
      
      grp <- grp_key()
      
      df2 <- df %>%
        dplyr::mutate(
          Item_canon  = item_canon(.data$Item),
          Animal_low  = stringr::str_to_lower(stringr::str_squish(as.character(.data$Animal)))
        )
      
      RUM_ITEMS_CANON <- c("Bovine meat", "Small ruminants meat", "Dairy")
      
      if (grp == "ruminants") {
        
        df2 <- df2 %>% dplyr::filter(.data$Item_canon %in% RUM_ITEMS_CANON)
        
        df2 <- df2 %>%
          dplyr::mutate(
            Item_main = factor(.data$Item_canon, levels = c("Bovine meat","Small ruminants meat","Dairy")),
            
            # Sous-composantes empilées dans la barre de l'item
            Component = dplyr::case_when(
              .data$Item_canon == "Dairy" ~ "Dairy",
              
              .data$Item_canon == "Bovine meat" &
                (stringr::str_detect(.data$Animal_low, "dairy") | .data$Animal_low == "dairy") ~
                "Bovine meat from dairy cattle",
              .data$Item_canon == "Bovine meat" ~
                "Bovine meat from beef cattle",
              
              .data$Item_canon == "Small ruminants meat" &
                (stringr::str_detect(.data$Animal_low, "dairy") | .data$Animal_low == "dairy") ~
                "Small ruminants meat from dairy cattle",
              .data$Item_canon == "Small ruminants meat" ~
                "Small ruminants meat from meat animals",
              
              TRUE ~ .data$Item_canon
            ),
            
            Component = factor(
              .data$Component,
              levels = c(
                "Bovine meat from beef cattle",
                "Bovine meat from dairy cattle",
                "Small ruminants meat from meat animals",
                "Small ruminants meat from dairy cattle",
                "Dairy"
              )
            )
          )
        
      } else {
        
        # Monogastrics = tout sauf les 3 ruminants
        df2 <- df2 %>% dplyr::filter(!(.data$Item_canon %in% RUM_ITEMS_CANON))
        
        ord <- df2 %>%
          dplyr::group_by(.data$Item_canon) %>%
          dplyr::summarise(Total = sum(.data$Value, na.rm = TRUE), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(.data$Total)) %>%
          dplyr::pull(.data$Item_canon)
        
        df2 <- df2 %>%
          dplyr::mutate(
            Item_main = factor(.data$Item_canon, levels = ord),
            Component = factor(.data$Item_canon, levels = ord)  # 1 composante = pas d'empilement
          )
      }
      
      # Agrégation Scenario/Year/Item_main/Component
      df_sum <- df2 %>%
        dplyr::group_by(.data$Scenario, .data$Year, .data$Item_main, .data$Component) %>%
        dplyr::summarise(
          Value = sum(.data$Value, na.rm = TRUE),
          Unit  = dplyr::first(.data$Unit),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
      
      # règle année : base=2018, autres=2050
      df_sum %>%
        dplyr::mutate(
          target_year = dplyr::if_else(as.character(.data$Scenario) == scen_base, 2018L, 2050L)
        ) %>%
        dplyr::filter(.data$Year == .data$target_year)
    }) %>% bindCache(r_country(), scen_show_key(), grp_key())
    
    # -------------------------------------------------
    # Plot
    # -------------------------------------------------
    output$plot <- plotly::renderPlotly({
      dfp <- data_summarised()
      shiny::validate(shiny::need(nrow(dfp) > 0, "No data available for this selection."))
      
      grp <- grp_key()
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color  = "#111827",
        muted_color = "#6B7280",
        gridcolor   = "rgba(0,0,0,.15)"
      )
      gg_txt <- th$font_color %||% "#111827"
      
      # Millions of tonnes
      dfp <- dfp %>% dplyr::mutate(Value_m = .data$Value / 1e6)
      
      key_from_component <- function(comp){
        comp <- stringr::str_squish(as.character(comp))
        
        dplyr::case_when(
          comp == "Dairy" ~ "Dairy",
          
          stringr::str_starts(comp, "Bovine meat from beef cattle")  ~ "Beef cattle",
          stringr::str_starts(comp, "Bovine meat from dairy cattle") ~ "Dairy cattle",
          
          # tu gardes le nom "from dairy cattle" mais tu le mappe sur une clé palette dédiée
          stringr::str_starts(comp, "Small ruminants meat from dairy cattle") ~ "Dairy sheep and goats",
          stringr::str_starts(comp, "Small ruminants meat from meat animals") ~ "Meat sheep and goats",
          
          stringr::str_detect(comp, regex("egg", ignore_case = TRUE))     ~ "Poultry eggs",
          stringr::str_detect(comp, regex("poultry", ignore_case = TRUE)) ~ "Poultry meat",
          
          TRUE ~ comp
        )
      }
      
      
      comp_levels <- levels(dfp$Component)
      fill_cols <- if (exists("emissions_animal_colors_for", mode = "function")) {
        keys <- vapply(comp_levels, key_from_component, FUN.VALUE = character(1))
        cols_keys <- emissions_animal_colors_for(keys)
        setNames(unname(cols_keys), comp_levels)
      } else {
        setNames(scales::hue_pal()(length(comp_levels)), comp_levels)
      }
      
      # ---------- Plot ----------
      if (grp == "ruminants") {
        # 3 barres collées au sein de chaque scénario, avec un espace entre scénarios
        scen_lvls <- levels(dfp$Scenario)
        scen_idx  <- match(as.character(dfp$Scenario), scen_lvls)
        
        # offsets = distance entre centres = largeur => barres collées dans un scénario
        offsets <- c(
          "Bovine meat"          = -0.25,
          "Small ruminants meat" =  0.00,
          "Dairy"                =  0.25
        )
        dfp <- dfp %>%
          dplyr::mutate(
            Item_main_chr = as.character(.data$Item_main),
            x = scen_idx + (offsets[.data$Item_main_chr] %||% 0)
          )
        
        gg <- ggplot2::ggplot(
          dfp,
          ggplot2::aes(
            x = .data$x,
            y = .data$Value_m,
            fill = .data$Component,
            group = interaction(.data$Scenario, .data$Item_main),
            text = paste0(
              "Scenario: ", scenario_label(as.character(.data$Scenario)), "<br>",
              "Bar: ", as.character(.data$Item_main), "<br>",
              "Component: ", as.character(.data$Component), "<br>",
              "Production: ", format(round(.data$Value_m, 3), nsmall = 3, decimal.mark = ".", big.mark = ""), " million tons<br>",
              "Year: ", .data$Year
            )
          )
        ) +
          ggplot2::geom_col(width = 0.25) +  # largeur = 0.25 => collé entre items du scénario
          ggplot2::scale_x_continuous(
            breaks = seq_along(scen_lvls),
            labels = scenario_label(scen_lvls),
            expand = ggplot2::expansion(mult = c(0.02, 0.02))
          ) +
          ggplot2::scale_fill_manual(values = fill_cols, name = NULL) +
          ggplot2::scale_y_continuous(
            labels = scales::label_number(big.mark = " ", accuracy = 1),
            expand = ggplot2::expansion(mult = c(0, 0.10))
          ) +
          ggplot2::labs(x = NULL, y = "million tonnes") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            axis.text.x      = ggplot2::element_text(hjust = 0.5, colour = gg_txt),
            axis.text.y      = ggplot2::element_text(colour = gg_txt),
            axis.title.y     = ggplot2::element_text(colour = gg_txt),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = th$gridcolor %||% "rgba(0,0,0,.15)"),
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            legend.position  = "top"
          )
        
      } else {
        pos <- ggplot2::position_dodge2(width = 0.85, preserve = "single", padding = 0)
        item_lvls <- levels(dfp$Item_main)
        item_cols <- if (exists("emissions_animal_colors_for", mode = "function")) {
          keys <- vapply(item_lvls, key_from_component, FUN.VALUE = character(1))
          cols_keys <- emissions_animal_colors_for(keys)
          setNames(unname(cols_keys), item_lvls)
        } else {
          setNames(scales::hue_pal()(length(item_lvls)), item_lvls)
        }
        
        gg <- ggplot2::ggplot(
          dfp,
          ggplot2::aes(
            x    = .data$Scenario,
            y    = .data$Value_m,
            fill = .data$Item_main,
            text = paste0(
              "Scenario: ", scenario_label(as.character(.data$Scenario)), "<br>",
              "Item: ", as.character(.data$Item_main), "<br>",
              "Production: ", format(round(.data$Value_m, 3), nsmall = 3, decimal.mark = ".", big.mark = ""), " million tonnes<br>",
              "Year: ", .data$Year
            )
          )
        ) +
          ggplot2::geom_col(position = pos, width = 0.85) +
          ggplot2::scale_x_discrete(labels = function(x) scenario_label(x)) +
          ggplot2::scale_fill_manual(values = item_cols, name = NULL) +
          ggplot2::scale_y_continuous(
            labels = scales::label_number(big.mark = " ", accuracy = 1),
            expand = ggplot2::expansion(mult = c(0, 0.10))
          ) +
          ggplot2::labs(x = NULL, y = "million tonnes") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
            axis.text.x      = ggplot2::element_text(hjust = 0.5, colour = gg_txt),
            axis.text.y      = ggplot2::element_text(colour = gg_txt),
            axis.title.y     = ggplot2::element_text(colour = gg_txt),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = th$gridcolor %||% "rgba(0,0,0,.15)"),
            plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
            panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
            legend.position  = "top"
          )
      }
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      p <- plotly::layout(
        p,
        legend = list(orientation = "h", x = 0, y = 1.12, xanchor = "left"),
        margin = list(l = 55, r = 20, t = 20, b = 30),
        yaxis  = list(separatethousands = TRUE)
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    })
    # -------------------------------------------------
    # Download
    # -------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("animal_products_production_", r_country(), "_", grp_key(), ".csv")
      },
      content = function(file){
        dfp <- data_summarised() %>%
          dplyr::mutate(
            Scenario = as.character(.data$Scenario),
            Scenario_label = scenario_label(.data$Scenario),
            Item_main = as.character(.data$Item_main),
            Component = as.character(.data$Component)
          ) %>%
          dplyr::select(Scenario, Scenario_label, Year, Item_main, Component, Value, Unit)
        
      }
    )
    
    # -------------------------------------------------
    # Note
    # -------------------------------------------------
    output$note <- renderUI({
      grp <- grp_key()
      
      if (grp == "ruminants") {
        txt <- "
    <p>
      This chart shows the production of <strong>ruminant animal products</strong> (in <strong>million tonnes</strong>)
      for the selected country and scenarios. For each scenario, three product categories are displayed as separate bars:
      <strong>Bovine meat</strong>, <strong>Small ruminants meat</strong>, and <strong>Dairy</strong>.
    </p>

    <p>
      The <strong>Bovine meat</strong> and <strong>Small ruminants meat</strong> bars are <strong>stacked</strong> to
      show the contribution of different animal types based on the <strong>Animal</strong> column:
      <ul>
        <li><strong>Bovine meat from beef cattle</strong>: beef from meat livestock </li>
        <li><strong>Bovine meat from dairy cattle</strong>: beef from reform dairy cows </li>
        <li><strong>Small ruminants meat from meat animals</strong>:  small ruminants meat from livestock for meat</li>
        <li><strong>Small ruminants meat from dairy cattle</strong>: small ruminants meat from reform dairy small ruminants</li>
      </ul>
    </p>
    "
      } else {
        txt <- "
    <p>
      This chart shows the production of <strong>monogastric animal products</strong> (in <strong>million tonnes</strong>)
      for the selected country and scenarios.
    </p>

    <p>
      Monogastrics include <strong>all production items except</strong> the three ruminant items:
      <strong>Bovine meat</strong>, <strong>Small ruminants meat</strong>, and <strong>Dairy</strong>.
      Each item is displayed as a separate bar within each scenario.
    </p>
    "
      }
      
      htmltools::HTML(txt)
    })
    
  })
}
