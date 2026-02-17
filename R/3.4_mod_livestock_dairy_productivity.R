# R/3.4_mod_livestock_dairy_productivity.R
# -------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------------------------------------------------
# UI
# -------------------------------------------------
mod_livestock_dairy_productivity_ui <- function(id, height = "480px", full_width = TRUE){
  ns <- NS(id)
  div(
    class = if (isTRUE(full_width)) "card full-bleed" else "card",
    div(
      class = "card-body",
      h2("Dairy productivity by scenario (tons/TLU)"),
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
mod_livestock_dairy_productivity_server <- function(
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
      shiny::need(is.function(r_scenarios), "r_scenarios must be provided to this module.")
    )
    
    scen_base <- if (exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)
    } else {
      scenario_code("Année de base")
    }
    
    # Map label -> code (extra scenario UI labels that may appear in fact)
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
    
    get_schema <- function(df){
      has <- function(...) all(c(...) %in% names(df))
      if (has("Region","Scenario","Year","Unit","Value","Item","Element"))  return("item_element")
      if (has("Region","Scenario","Year","Unit","Value","Objet","Element")) return("objet_element")
      NA_character_
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
    
    is_empty <- function(x){
      xx <- stringr::str_squish(as.character(x))
      is.na(xx) | xx == ""
    }
    
    system_col_name <- function(df){
      if ("system" %in% names(df)) return("system")
      if ("System" %in% names(df)) return("System")
      NA_character_
    }
    
    # -------------------------------------------------
    # bindCache key
    # -------------------------------------------------
    scen_show_key <- shiny::reactive({
      sc <- norm_scenario(r_scenarios())
      paste(sc, collapse = "|")
    })
    
    # -------------------------------------------------
    # Stable scenario levels
    # -------------------------------------------------
    scen_levels_effective <- shiny::reactive({
      req(r_country())
      
      wanted <- r_scenarios()
      shiny::validate(shiny::need(!is.null(wanted) && length(wanted) > 0, "No scenarios provided by r_scenarios()."))
      
      wanted <- norm_scenario(wanted)
      lvls_default <- norm_scenario(get("SCENARIO_LEVELS_DEFAULT", inherits = TRUE))
      
      schema <- get_schema(fact)
      shiny::validate(shiny::need(!is.na(schema), "Dairy productivity: unsupported fact schema."))
      
      sys_col <- system_col_name(fact)
      shiny::validate(
        shiny::need("Animal" %in% names(fact), "Dairy productivity: missing column 'Animal' in fact."),
        shiny::need(!is.na(sys_col), "Dairy productivity: missing column 'system' (or 'System') in fact.")
      )
      
      present <- if (schema == "item_element") {
        fact %>%
          dplyr::filter(
            .data$Region == r_country(),
            .data$Item == "Dairy",
            .data$Element %in% c("Production","LSU"),
            .data$Year %in% c(2018, 2050),
            is_empty(.data$Animal),
            is_empty(.data[[sys_col]])
          ) %>%
          dplyr::mutate(Scenario = norm_scenario(.data$Scenario)) %>%
          dplyr::pull("Scenario") %>%
          unique()
      } else {
        fact %>%
          dplyr::filter(
            .data$Region == r_country(),
            .data$Element == "Dairy",
            .data$Objet %in% c("Production","LSU"),
            .data$Year %in% c(2018, 2050),
            is_empty(.data$Animal),
            is_empty(.data[[sys_col]])
          ) %>%
          dplyr::mutate(Scenario = norm_scenario(.data$Scenario)) %>%
          dplyr::pull("Scenario") %>%
          unique()
      }
      
      lvls_default[lvls_default %in% wanted & lvls_default %in% present]
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # Filtered data (strict: Animal & system empty)
    # -------------------------------------------------
    data_filtered <- shiny::reactive({
      req(fact, r_country())
      lvls <- scen_levels_effective()
      shiny::validate(shiny::need(length(lvls) > 0, "No dairy scenarios available for this country (after r_scenarios ∩ present)."))
      
      schema <- get_schema(fact)
      sys_col <- system_col_name(fact)
      
      if (schema == "item_element") {
        fact %>%
          dplyr::filter(
            .data$Region == r_country(),
            .data$Item == "Dairy",
            .data$Element %in% c("Production","LSU"),
            .data$Year %in% c(2018, 2050),
            is_empty(.data$Animal),
            is_empty(.data[[sys_col]])
          ) %>%
          dplyr::mutate(
            Scenario = norm_scenario(.data$Scenario),
            Scenario = factor(.data$Scenario, levels = lvls),
            Measure  = .data$Element,
            Value    = parse_num_safe(.data$Value)
          ) %>%
          dplyr::filter(.data$Scenario %in% lvls) %>%
          dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
      } else {
        fact %>%
          dplyr::filter(
            .data$Region == r_country(),
            .data$Element == "Dairy",
            .data$Objet %in% c("Production","LSU"),
            .data$Year %in% c(2018, 2050),
            is_empty(.data$Animal),
            is_empty(.data[[sys_col]])
          ) %>%
          dplyr::mutate(
            Scenario = norm_scenario(.data$Scenario),
            Scenario = factor(.data$Scenario, levels = lvls),
            Measure  = .data$Objet,
            Value    = parse_num_safe(.data$Value)
          ) %>%
          dplyr::filter(.data$Scenario %in% lvls) %>%
          dplyr::mutate(Scenario = forcats::fct_drop(.data$Scenario))
      }
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # Summarise + productivity (simple ratio)
    # -------------------------------------------------
    data_productivity <- shiny::reactive({
      df <- data_filtered()
      req(nrow(df) > 0)
      
      prod <- df %>%
        dplyr::filter(.data$Measure == "Production") %>%
        dplyr::group_by(.data$Scenario, .data$Year) %>%
        dplyr::summarise(Production = sum(.data$Value, na.rm = TRUE), .groups = "drop")
      
      lsu <- df %>%
        dplyr::filter(.data$Measure == "LSU") %>%
        dplyr::group_by(.data$Scenario, .data$Year) %>%
        dplyr::summarise(LSU = sum(.data$Value, na.rm = TRUE), .groups = "drop")
      
      out <- dplyr::full_join(prod, lsu, by = c("Scenario","Year"))
      
      shiny::validate(
        shiny::need(any(!is.na(out$Production)), "Missing Production (Dairy) after filters (Animal/system must be empty)."),
        shiny::need(any(!is.na(out$LSU)),        "Missing LSU (Dairy) after filters (Animal/system must be empty).")
      )
      
      out %>%
        dplyr::mutate(
          productivity = dplyr::if_else(is.finite(.data$LSU) & .data$LSU != 0, .data$Production / .data$LSU, NA_real_)
        )
    }) %>% bindCache(r_country(), scen_show_key())
    
    # -------------------------------------------------
    # Plot
    # -------------------------------------------------
    output$plot <- plotly::renderPlotly({
      df <- data_productivity()
      req(nrow(df) > 0)
      
      th <- if (exists("get_plotly_tokens", mode = "function")) get_plotly_tokens() else list(
        font_color  = "#111827",
        muted_color = "#6B7280",
        gridcolor   = "rgba(0,0,0,.15)"
      )
      gg_txt   <- th$font_color %||% "#111827"
      gg_muted <- th$muted_color %||% "#6B7280"
      
      # baseline = (base scenario, 2018) ; others = 2050
      df_plot <- df %>%
        dplyr::mutate(
          target_year = dplyr::if_else(as.character(.data$Scenario) == scen_base, 2018L, 2050L)
        ) %>%
        dplyr::filter(.data$Year == .data$target_year)
      
      shiny::validate(shiny::need(nrow(df_plot) > 0, "No data available for dairy productivity (baseline 2018 + scenarios 2050)."))
      
      baseline <- df_plot %>%
        dplyr::filter(as.character(.data$Scenario) == scen_base, .data$Year == 2018L) %>%
        dplyr::pull(.data$productivity)
      
      shiny::validate(
        shiny::need(length(baseline) > 0 && is.finite(baseline[1]), "Missing baseline (Base year, 2018) dairy productivity.")
      )
      baseline <- baseline[1]
      
      df_plot <- df_plot %>%
        dplyr::mutate(
          pct_change = dplyr::if_else(
            as.character(.data$Scenario) != scen_base & is.finite(.data$productivity) & baseline != 0,
            100 * (.data$productivity - baseline) / baseline,
            NA_real_
          ),
          pct_label = dplyr::if_else(is.na(.data$pct_change), NA_character_, sprintf("%+d%%", round(.data$pct_change)))
        )
      
      max_val <- suppressWarnings(max(df_plot$productivity, na.rm = TRUE))
      if (!is.finite(max_val)) max_val <- 0
      
      df_labels <- df_plot %>%
        dplyr::filter(as.character(.data$Scenario) != scen_base, !is.na(.data$pct_label)) %>%
        dplyr::mutate(label_y = .data$productivity + 0.05 * max_val)
      
      # Scenario colors (codes) + legend labels via scenario_label()
      lvls <- levels(df_plot$Scenario) # scenario CODES (FR)
      pal <- if (exists("scenario_palette", mode = "function")) {
        scenario_palette(lvls)
      } else {
        setNames(scales::hue_pal()(length(lvls)), lvls)
      }
      
      # ---- Force legend to use SCENARIO_LABELS: map fill to UI labels ----
      ui_lvls <- scenario_label(lvls)
      
      df_plot <- df_plot %>%
        dplyr::mutate(
          Scenario_code = as.character(.data$Scenario),
          Scenario_ui   = factor(scenario_label(.data$Scenario_code), levels = ui_lvls)
        )
      
      df_labels <- df_labels %>%
        dplyr::mutate(
          Scenario_code = as.character(.data$Scenario),
          Scenario_ui   = factor(scenario_label(.data$Scenario_code), levels = ui_lvls)
        )
      
      pal_ui <- setNames(unname(pal), ui_lvls)
      
      gg <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = .data$Scenario_ui,
          y = .data$productivity,
          fill = .data$Scenario_ui,
          text = paste0(
            "Scenario: ", .data$Scenario_ui, "<br>",
            "Productivity: ", format(round(.data$productivity, 2), nsmall = 2, decimal.mark = ".", big.mark = ""), " t/TLU<br>",
            "Year: ", .data$Year
          )
        )
      ) +
        ggplot2::geom_hline(yintercept = baseline, linetype = "dashed", colour = gg_muted) +
        ggplot2::geom_col(show.legend = TRUE) +
        ggplot2::scale_fill_manual(values = pal_ui, breaks = ui_lvls) +
        ggplot2::geom_text(
          data = df_labels,
          ggplot2::aes(x = .data$Scenario_ui, y = .data$label_y, label = .data$pct_label),
          vjust  = 0,
          size   = 3.5,
          colour = gg_txt
        ) +
        ggplot2::labs(x = NULL, y = "tons/TLU", fill = NULL) +
        ggplot2::scale_y_continuous(
          labels = scales::label_number(big.mark = "", accuracy = 0.01),
          expand = ggplot2::expansion(mult = c(0, 0.25))
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x      = ggplot2::element_text(hjust = 0.5, colour = gg_txt),
          axis.text.y      = ggplot2::element_text(colour = gg_txt),
          axis.title.y     = ggplot2::element_text(colour = gg_txt),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_line(colour = th$gridcolor %||% "rgba(0,0,0,.15)"),
          plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
          panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
          legend.position  = "right"
        )
      
      p <- plotly::ggplotly(gg, tooltip = "text")
      p <- plotly::layout(
        p,
        yaxis  = list(separatethousands = FALSE),
        margin = list(l = 55, r = 20, t = 10, b = 40)
      )
      
      if (exists("plotly_apply_global_theme", mode = "function")) {
        p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      } else if (exists("plotly_theme_transparent", mode = "function")) {
        p <- plotly_theme_transparent(p)
      }
      
      p
    })
    
    # -------------------------------------------------
    # Download CSV
    # -------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function(){
        paste0("dairy_productivity_", r_country(), ".csv")
      },
      content = function(file){
        df <- data_productivity() %>%
          dplyr::mutate(Scenario = as.character(.data$Scenario)) %>%
          dplyr::select(Scenario, Year, Production, LSU, productivity)
        readr::write_csv(df, file)
      }
    )
    
    # -------------------------------------------------
    # Note
    # -------------------------------------------------
    output$note <- renderUI({
      df <- data_filtered()
      if (!nrow(df)) return(NULL)
      
      htmltools::HTML(glue::glue(
        "<p>
        Dairy productivity is computed as <strong>Production </strong> divided by <strong>tropical livestock unit (TLU)</strong>,
        expressed in <strong>tons/TLU</strong>. If a scenario has no data, then there is no milk production in this scenario. <br>
        </p>"
      ))
    })
    
  })
}
