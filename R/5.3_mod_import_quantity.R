# R/5.3_mod_import_quantity.R
# -------------------------------------------------
# Energy flows (item = "All") by scenario
# - Single-flow view: 1 bar per scenario, dashed baseline (2018) + % change
# - Scenarios: STRICTLY driven by r_scenarios() (codes) + global order config
# -------------------------------------------------

mod_import_quantity_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    h2("Evolution of sources and uses in energy quantities (Gcal)"),
    tags$div(style="height:8px"),
    div(
      class = "u-controls",
        selectInput(
          ns("view_mode"),
          label = "Flow considered",
          choices = c(
            "Imports"                   = "imports",
            "Domestic supply"           = "domestic",
            "Exports"                   = "exports",
            "Production"                = "production",
            "Food"                      = "food",
            "Feed"                      = "feed",
            "Losses"                    = "losses",
            "Seed"                      = "seed",
            "Other uses (non-food)"     = "other_uses"
          ),
          selected = "imports"
        )
    ),
    
    plotly::plotlyOutput(ns("plot"), height = "auto"),
    
    div(
      class = "u-actions",
      downloadLink(
        ns("dl_csv"),
        label = tagList(icon("download"), "CSV")
      )
    ),
    uiOutput(ns("note"))
  )
  
  if (isTRUE(wrap_in_card)) {
    div(class = "card", div(class = "card-body", content))
  } else {
    content
  }
}


mod_import_quantity_server <- function(
    id,
    fact,
    r_country,
    r_scenarios,   # reactive(): vector of SCENARIO CODES to display (baseline + diets + optional extra)
    ...
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shiny::validate(
      shiny::need(exists("scenario_code", mode = "function"), "Missing scenario_code()."),
      shiny::need(exists("scenario_label", mode = "function"), "Missing scenario_label()."),
      shiny::need(exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE), "Missing SCENARIO_LEVELS_DEFAULT."),
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)
    } else {
      scenario_code("Année de base")
    }
    
    # Map label -> code (au cas où fact contient un label UI pour l'extra)
    EXTRA_LABEL_TO_CODE <- if (exists("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)) {
      setNames(
        unname(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE)),
        names(get("SCENARIOS_EXTRA_CHOICES", inherits = TRUE))
      )
    } else {
      NULL
    }
    
    # Normalisation unique: code() + recode(label->code) sans casser la normalisation
    norm_scenario <- function(x){
      x <- scenario_code(x)
      if (!is.null(EXTRA_LABEL_TO_CODE)) {
        x <- dplyr::recode(x, !!!EXTRA_LABEL_TO_CODE, .default = x)
      }
      x
    }
    
    # ------------------------------------------------------------------
    # Scalar key for bindCache (NEVER pass the vector directly)
    # ------------------------------------------------------------------
    scen_show_key <- shiny::reactive({
      sc <- norm_scenario(r_scenarios())
      paste(sc, collapse = "|")
    })
    
    scen_levels_effective <- shiny::reactive({
      req(r_country())
      
      wanted <- r_scenarios()
      shiny::validate(shiny::need(!is.null(wanted) && length(wanted) > 0, "No scenarios provided by r_scenarios()."))
      
      wanted <- norm_scenario(wanted)
      lvls_default <- norm_scenario(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
      
      present <- fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          stringr::str_starts(.data$Element, "Energy "),
          .data$Item == "All",
          .data$Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(Scenario = norm_scenario(.data$Scenario)) %>%
        dplyr::pull("Scenario") %>%
        unique()
      
      lvls_default[lvls_default %in% wanted & lvls_default %in% present]
    }) %>% bindCache(r_country(), scen_show_key())
    
    #--------------------------------------------
    # Données filtrées + typage des flux (brut, en Gcal) + scénarios effectifs
    #--------------------------------------------
    data_filtered <- shiny::reactive({
      req(fact, r_country())
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No energy scenarios available for this country (after r_scenarios ∩ present)."))
      
      fact %>%
        dplyr::filter(
          .data$Region == r_country(),
          stringr::str_starts(.data$Element, "Energy "),
          .data$Item == "All",
          .data$Year %in% c(2018, 2050)
        ) %>%
        dplyr::mutate(
          Scenario = norm_scenario(.data$Scenario),
          Scenario = factor(.data$Scenario, levels = lvls),
          
          Flow = dplyr::case_when(
            stringr::str_detect(.data$Element, "Production")       ~ "Production",
            stringr::str_detect(.data$Element, "Import")           ~ "Imports",
            stringr::str_detect(.data$Element, "Export")           ~ "Exports",
            stringr::str_detect(.data$Element, "Domestic supply")  ~ "Domestic supply",
            stringr::str_detect(.data$Element, "Food")             ~ "Food",
            stringr::str_detect(.data$Element, "Feed")             ~ "Feed",
            stringr::str_detect(.data$Element, "Loss")             ~ "Losses",
            stringr::str_detect(.data$Element, "Seed")             ~ "Seed",
            stringr::str_detect(.data$Element, "Other uses")       ~ "Other uses (non-food)",
            TRUE                                                   ~ NA_character_
          )
        ) %>%
        dplyr::filter(
          !is.na(.data$Flow),
          .data$Scenario %in% lvls,
          .data$Flow %in% c(
            "Imports", "Domestic supply", "Exports", "Production",
            "Food", "Feed", "Losses", "Seed", "Other uses (non-food)"
          )
        ) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
    }) %>% bindCache(r_country(), scen_show_key())
    
    # Résumé agrégé par scénario / flux / année (toujours en Gcal ici)
    data_summarised <- shiny::reactive({
      df <- data_filtered()
      req(nrow(df) > 0)
      
      df %>%
        dplyr::group_by(.data$Scenario, .data$Unit, .data$Flow, .data$Year) %>%
        dplyr::summarise(
          Value = sum(.data$Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
    }) %>% bindCache(r_country(), scen_show_key())
    
    # Petite fonction: couleur par flux (via palette Sankey si dispo)
    get_flow_color <- function(flow) {
      if (exists("sankey_node_palette")) {
        base_cols <- sankey_node_palette()
        col <- base_cols[flow]
        if (!is.na(col)) return(col)
      }
      scales::hue_pal()(1)
    }
    
    #--------------------------------------------
    # Graphique
    #--------------------------------------------
    output$plot <- plotly::renderPlotly({
      df_sum <- data_summarised()
      req(nrow(df_sum) > 0)
      
      mode <- input$view_mode %||% "imports"
      
      # >>> R/99 tokens (dark/light)
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color     = "#111827",
        muted_color    = "#6B7280",
        axis_linecolor = "rgba(0,0,0,.18)"
      )
      
      # Passage en millions de Gcal
      scale_factor <- 1e6
      df_sum <- df_sum %>%
        dplyr::mutate(Value = Value / scale_factor)
      
      raw_unit   <- paste(unique(df_sum$Unit), collapse = ", ")
      unit_label <- paste("million", raw_unit)
      
      mode_to_flow <- c(
        "imports"    = "Imports",
        "domestic"   = "Domestic supply",
        "exports"    = "Exports",
        "production" = "Production",
        "food"       = "Food",
        "feed"       = "Feed",
        "losses"     = "Losses",
        "seed"       = "Seed",
        "other_uses" = "Other uses (non-food)"
      )
      
      flow_pretty <- c(
        "Imports"               = "Biomass import in energy (million of Gcal)",
        "Domestic supply"       = "Biomass domestic supply in energy (million of Gcal)",
        "Exports"               = "Biomass export in energy (million of Gcal)",
        "Production"            = "Biomass production in energy (million of Gcal)",
        "Food"                  = "Biomass used for food in energy (million of Gcal)",
        "Feed"                  = "Biomass used for feed in energy (million of Gcal)",
        "Losses"                = "Biomass lost in energy (million of Gcal)",
        "Seed"                  = "Biomass used as seed in energy (million of Gcal)",
        "Other uses (non-food)" = "Biomass used as non-food in energy (million of Gcal)"
      )
      
      flow_name <- mode_to_flow[[mode]]
      req(!is.null(flow_name))
      
      # baseline = (base scenario, 2018) ; autres scénarios = 2050
      df_plot <- df_sum %>%
        dplyr::filter(.data$Flow == flow_name) %>%
        dplyr::mutate(
          target_year = dplyr::if_else(as.character(.data$Scenario) == scen_base, 2018L, 2050L)
        ) %>%
        dplyr::filter(.data$Year == .data$target_year) %>%
        dplyr::select("Scenario", "Unit", "Year", "Value")
      
      shiny::validate(shiny::need(nrow(df_plot) > 0, "No data available for the selected flow (baseline 2018 + scenarios 2050)."))
      
      baseline <- df_plot %>%
        dplyr::filter(as.character(.data$Scenario) == scen_base, .data$Year == 2018L) %>%
        dplyr::pull(.data$Value)
      if (length(baseline) == 0 || !is.finite(baseline)) baseline <- NA_real_
      
      df_plot <- df_plot %>%
        dplyr::mutate(
          pct_change = dplyr::case_when(
            is.finite(baseline) & baseline != 0 & as.character(.data$Scenario) != scen_base ~ 100 * (.data$Value - baseline) / baseline,
            TRUE ~ NA_real_
          ),
          pct_label = dplyr::if_else(
            is.na(.data$pct_change),
            NA_character_,
            sprintf("%+d%%", round(.data$pct_change))
          )
        )
      
      max_val <- suppressWarnings(max(df_plot$Value, na.rm = TRUE))
      if (!is.finite(max_val)) max_val <- 0
      
      df_labels <- df_plot %>%
        dplyr::filter(as.character(.data$Scenario) != scen_base, !is.na(.data$pct_label)) %>%
        dplyr::mutate(label_y = .data$Value + 0.03 * max_val)
      
      this_color <- get_flow_color(flow_name)
      nice_label <- flow_pretty[[flow_name]] %||% flow_name
      
      gg_txt   <- th$font_color %||% "#111827"
      gg_muted <- th$muted_color %||% "#6B7280"
      
      gg <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = .data$Scenario,
          y = .data$Value,
          text = paste0(
            "Scenario: ", scenario_label(as.character(.data$Scenario)), "<br>",
            nice_label, ": ", scales::comma(.data$Value, accuracy = 0.1), " ", unit_label, "<br>",
            "Year: ", .data$Year
          )
        )
      ) +
        { if (is.finite(baseline))
          ggplot2::geom_hline(
            yintercept = baseline,
            linetype   = "dashed",
            colour     = gg_muted
          ) else NULL } +
        ggplot2::geom_col(fill = this_color, show.legend = FALSE) +
        ggplot2::geom_text(
          data = df_labels,
          ggplot2::aes(y = .data$label_y, label = pct_label),
          vjust = 0,
          size  = 3.5,
          colour = gg_txt
        ) +
        ggplot2::labs(
          x = NULL,
          y = nice_label
        ) +
        ggplot2::scale_x_discrete(labels = function(x) scenario_label(x)) +
        ggplot2::scale_y_continuous(
          labels = scales::label_comma(),
          expand = ggplot2::expansion(mult = c(0, 0.25))
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x       = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1, colour = gg_txt),
          axis.text.y       = ggplot2::element_text(colour = gg_txt),
          axis.title.y      = ggplot2::element_text(colour = gg_txt),
          panel.grid.minor  = ggplot2::element_blank(),
          panel.grid.major  = ggplot2::element_line(colour = th$gridcolor %||% "rgba(0,0,0,.15)"),
          plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA)
        )
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      p <- plotly::layout(p, margin = list(l = 40, r = 20, t = 20, b = 60))
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    })
    
    #--------------------------------------------
    # Export CSV (brut, en Gcal) — limité aux scénarios effectifs + 2018/2050
    #--------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("energy_flows_", r_country(), ".csv")
      },
      content = function(file){
        readr::write_csv(data_filtered(), file)
      }
    )
    
    #--------------------------------------------
    # Note explicative (EN — générique)
    #--------------------------------------------
    output$note <- renderUI({
      df <- data_filtered()
      if (nrow(df) == 0) return(NULL)
      
      unit_label <- paste(unique(df$Unit), collapse = ", ")
      mode <- input$view_mode %||% "imports"
      
      mode_to_flow <- c(
        "imports"    = "Imports",
        "domestic"   = "Domestic supply",
        "exports"    = "Exports",
        "production" = "Production",
        "food"       = "Food",
        "feed"       = "Feed",
        "losses"     = "Losses",
        "seed"       = "Seed",
        "other_uses" = "Other uses (non-food)"
      )
      
      flow_pretty <- c(
        "Imports"               = "energy imports",
        "Domestic supply"       = "total energy supply",
        "Exports"               = "energy exports",
        "Production"            = "energy production",
        "Food"                  = "energy used for food",
        "Feed"                  = "energy used for feed",
        "Losses"                = "energy losses",
        "Seed"                  = "energy used as seed",
        "Other uses (non-food)" = "other non-food energy uses"
      )
      
      flow_name <- mode_to_flow[[mode]]
      nice_flow <- flow_pretty[[flow_name]] %||% "this energy flow"
      
      txt <- glue::glue(
        "<p>
        This chart shows how <strong>{nice_flow}</strong> evolves for the selected country
        between the <strong>base year</strong> and the different diet scenarios in
        2050.<br>
        Each bar represents the yearly volume of this flow
        (in <strong>million {unit_label}</strong>). The dashed horizontal line marks the
        base year level, and the percentages above the bars indicate the change compared to
        this baseline.
        </p>"
      )
      
      htmltools::HTML(txt)
    })
    
  })
}
