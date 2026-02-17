# R/3.2_mod_livestock_stocks.R
# ---------------------------------------------------------------

mod_livestock_stocks_ui <- function(id, title = "Stock of animals") {
  ns <- NS(id)
  tagList(
    div(
      class = "card",
      div(
        class = "card-body",
        h2(class = "card-title", title),
        
        # Legend once (centered)
        uiOutput(ns("legend_global")),
        
        tags$br(),
        uiOutput(ns("plots_grid")),
        
        div(
          class = "u-actions",
          downloadLink(
            ns("dl_livestock_stocks_csv"),
            label = tagList(icon("download"), "CSV")
          )
        ),
        
        uiOutput(ns("note"))
      )
    )
  )
}

mod_livestock_stocks_server <- function(
    id,
    fact,
    r_country,
    r_scenarios
){
  exclude_items <- c("Beef cattle equivalent", "Dairy cattle equivalent")
  
  # Items that MUST come from LSU/1000TLU (not from Stocks/heads)
  TLU_ITEMS_DISPLAY <- c("Beef cattle", "Dairy cattle", "Sheep and goats meat")
  
  # In your data, TLU-like values are stored under Element == "LSU" (and sometimes "TLU")
  TLU_ELEMENT_CODES <- c("LSU", "TLU")
  
  # Unit is 1000TLU (robust variants)
  TLU_UNITS <- c("1000TLU", "1000 TLU")
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
    
    # --- Dépendances scénarios (single source of truth) --------------------
    if (is.null(r_scenarios) || !is.function(r_scenarios)) {
      stop("mod_livestock_stocks_server(): 'r_scenarios' must be provided as a reactive/function returning scenario CODES.")
    }
    if (!exists("scenario_label", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'scenario_label(code)'.")
    }
    if (!exists("SCENARIO_LEVELS_DEFAULT", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIO_LEVELS_DEFAULT'.")
    }
    if (!exists("scenario_palette", mode = "function", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'scenario_palette(levels=...)'.")
    }
    if (!exists("SCENARIO_BASE_YEAR_CODE", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIO_BASE_YEAR_CODE'.")
    }
    if (!exists("SCENARIOS_BASE_CODES", inherits = TRUE)) {
      stop("mod_livestock_stocks_server(): missing dependency 'SCENARIOS_BASE_CODES'.")
    }
    
    EXTRA_CODES <- if (exists("SCENARIOS_EXTRA_CODES", inherits = TRUE)) {
      get("SCENARIOS_EXTRA_CODES", inherits = TRUE)
    } else {
      character(0)
    }
    SHOW_PCT_CODES <- unique(c(get("SCENARIOS_BASE_CODES", inherits = TRUE), EXTRA_CODES))
    
    scenario_label_vec <- function(x){
      x <- as.character(x)
      vapply(x, scenario_label, character(1))
    }
    
    base_year <- if (exists("BASE_YEAR", inherits = TRUE)) as.integer(BASE_YEAR) else 2018L
    
    # --- Scénarios demandés (CODES) + ordre central ------------------------
    scen_codes_ordered <- reactive({
      sc <- unique(as.character(r_scenarios()))
      sc <- sc[!is.na(sc) & nzchar(sc)]
      shiny::validate(shiny::need(length(sc) > 0, "No scenario provided to the module."))
      
      known   <- intersect(SCENARIO_LEVELS_DEFAULT, sc)
      unknown <- setdiff(sc, SCENARIO_LEVELS_DEFAULT)
      
      c(known, sort(unknown))
    })
    
    scen_levels_all <- reactive({
      sc <- scen_codes_ordered()
      c(SCENARIO_LEVELS_DEFAULT, setdiff(sc, SCENARIO_LEVELS_DEFAULT))
    })
    
    # IMPORTANT: on affiche tous les scénarios demandés même s'ils sont absents dans les données
    scen_levels_effective <- reactive({
      sc <- scen_codes_ordered()
      ordered <- scen_levels_all()[scen_levels_all() %in% sc]
      rest    <- setdiff(sc, ordered)
      unique(c(ordered, rest))
    })
    
    # Baseline code = base-year si dispo, sinon fallback
    scen_base <- reactive({
      sc_eff <- scen_levels_effective()
      b <- as.character(get("SCENARIO_BASE_YEAR_CODE", inherits = TRUE))
      
      if (nzchar(b) && b %in% sc_eff) return(b)
      
      b2 <- intersect(get("SCENARIOS_BASE_CODES", inherits = TRUE), sc_eff)
      b2[1] %||% sc_eff[1]
    })
    
    # --- Clé scalaire pour bindCache ---------------------------------------
    cache_key <- reactive({
      shiny::req(r_country())
      paste0(
        "livestock_stocks|",
        r_country(), "|base_year=", base_year,
        "|sc=", paste(scen_levels_effective(), collapse = ",")
      )
    })
    
    # >>> GLOBAL LEGEND (ONCE) — uses CONFIG labels & colors & order <<<
    output$legend_global <- renderUI({
      lvls <- scen_levels_effective()
      if (!length(lvls)) return(NULL)
      
      pal <- scenario_palette(levels = lvls)
      
      tags$div(
        style = paste0(
          "display:flex;justify-content:center;align-items:center;gap:14px;",
          "flex-wrap:wrap;margin-top:8px;margin-bottom:2px;"
        ),
        lapply(lvls, function(sc){
          col <- unname(pal[sc])
          lab <- scenario_label(sc)
          tags$div(
            style = "display:flex;align-items:center;gap:8px;",
            tags$span(style = paste0(
              "display:inline-block;width:14px;height:14px;border-radius:4px;",
              "background:", col, ";border:1px solid rgba(0,0,0,0.15);"
            )),
            tags$span(style = "font-size:12px;color:#FFFFFF;opacity:0.95;", lab)
          )
        })
      )
    }) %>% shiny::bindCache(cache_key())
    
    # --- 1) Préparation : Stocks (heads) + LSU/TLU (1000TLU) ---------------
    prep_df <- reactive({
      shiny::req(fact, r_country())
      
      need_cols <- c("Region","Element","Unit","Item","Scenario","Year","Value","System","Animal")
      miss <- setdiff(need_cols, names(fact))
      shiny::validate(shiny::need(length(miss) == 0, paste("Colonnes manquantes :", paste(miss, collapse=", "))))
      
      scen_allowed <- scen_levels_effective()
      shiny::validate(shiny::need(length(scen_allowed) > 0, "No scenario provided to the module."))
      
      # --- A) STOCKS (heads)
      df_heads <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Element  == "Stocks",
          Unit     %in% c("Head","1000 Head"),
          !(Item %in% exclude_items),
          Scenario %in% scen_allowed
        ) %>%
        dplyr::mutate(
          item_trim   = stringr::str_trim(as.character(Item)),
          animal_trim = stringr::str_trim(as.character(Animal)),
          system_trim = stringr::str_trim(as.character(System)),
          
          system_empty = is.na(System) | system_trim == "",
          
          animal_is_dairy = !is.na(Animal) &
            stringr::str_detect(animal_trim, regex("Dairy", ignore_case = TRUE)),
          
          Item_display = dplyr::case_when(
            animal_is_dairy & item_trim %in% c("Dairy cattle", "Beef cattle") ~ "Dairy cattle",
            
            system_empty & stringr::str_detect(item_trim, regex("Beef cattle", ignore_case = TRUE)) ~ "Beef cattle",
            system_empty & stringr::str_detect(item_trim, regex("Sheep and goats meat", ignore_case = TRUE)) ~ "Sheep and goats meat",
            system_empty & stringr::str_detect(item_trim, regex("Pigs", ignore_case = TRUE)) ~ "Pigs",
            system_empty & stringr::str_detect(item_trim, regex("Poultry eggs", ignore_case = TRUE)) ~ "Poultry eggs",
            system_empty & stringr::str_detect(item_trim, regex("Poultry meat", ignore_case = TRUE)) ~ "Poultry meat",
            
            TRUE ~ NA_character_
          ),
          
          # value_raw in HEADS (base unit)
          value_raw = dplyr::if_else(Unit == "Head", Value, Value * 1000),
          unit_kind = "heads"
        ) %>%
        dplyr::filter(!is.na(Item_display)) %>%
        dplyr::group_by(Item_display, unit_kind, Scenario, Year) %>%
        dplyr::summarise(value_raw = sum(value_raw, na.rm = TRUE), .groups = "drop") %>%
        # Crucial: remove items that must be taken from LSU/1000TLU
        dplyr::filter(!(Item_display %in% TLU_ITEMS_DISPLAY))
      
      # --- B) LSU/TLU (1000TLU)
      df_tlu <- fact %>%
        dplyr::filter(
          Region   == r_country(),
          Element  %in% TLU_ELEMENT_CODES,
          Unit     %in% TLU_UNITS,
          Scenario %in% scen_allowed
        ) %>%
        dplyr::mutate(
          item_trim = stringr::str_trim(as.character(Item)),
          
          Item_display = dplyr::case_when(
            item_trim == "Beef cattle"          ~ "Beef cattle",
            item_trim == "Dairy"                ~ "Dairy cattle",
            item_trim == "Meat sheep and goats" ~ "Sheep and goats meat",
            TRUE ~ NA_character_
          ),
          
          # value_raw kept in "1000TLU" units
          value_raw = Value,
          unit_kind = "tlu"
        ) %>%
        dplyr::filter(!is.na(Item_display)) %>%
        dplyr::group_by(Item_display, unit_kind, Scenario, Year) %>%
        dplyr::summarise(value_raw = sum(value_raw, na.rm = TRUE), .groups = "drop") %>%
        dplyr::filter(Item_display %in% TLU_ITEMS_DISPLAY)
      
      dplyr::bind_rows(df_heads, df_tlu)
    }) %>% shiny::bindCache(cache_key())
    
    # --- 2) Compact : baseline + année max par scénario + IMPUTATION -------
    df_compact <- reactive({
      df <- prep_df()
      scen_allowed <- scen_levels_effective()
      b <- scen_base()
      
      if (nrow(df) == 0) {
        return(tibble::tibble(
          Item_display=character(), unit_kind=character(),
          serie_code=character(), value_raw=double(),
          is_imputed=logical(), serie_f=factor()
        ))
      }
      
      base_has_year <- df %>%
        dplyr::filter(Scenario == b, Year == base_year) %>%
        nrow() > 0
      
      base <- if (isTRUE(base_has_year)) {
        df %>%
          dplyr::filter(Scenario == b, Year == base_year) %>%
          dplyr::transmute(
            Item_display,
            unit_kind,
            serie_code = as.character(Scenario),
            value_raw  = value_raw
          )
      } else {
        df %>%
          dplyr::filter(Scenario == b) %>%
          dplyr::group_by(Item_display, unit_kind) %>%
          dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            Item_display,
            unit_kind,
            serie_code = as.character(Scenario),
            value_raw  = value_raw
          )
      }
      
      proj <- df %>%
        dplyr::filter(Scenario != b) %>%
        dplyr::group_by(Item_display, unit_kind, Scenario) %>%
        dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          Item_display,
          unit_kind,
          serie_code = as.character(Scenario),
          value_raw  = value_raw
        )
      
      # IMPORTANT: DEDUP AVANT complete() (sinon -100% + vrai %)
      res <- dplyr::bind_rows(base, proj) %>%
        dplyr::mutate(
          Item_display = as.character(Item_display),
          unit_kind    = as.character(unit_kind),
          serie_code   = as.character(serie_code)
        ) %>%
        dplyr::group_by(Item_display, unit_kind, serie_code) %>%
        dplyr::summarise(
          value_raw = sum(value_raw, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Imputation des scénarios manquants => 0 (et flag is_imputed)
      res2 <- res %>%
        dplyr::mutate(
          serie_code   = as.character(serie_code),
          Item_display = as.character(Item_display),
          unit_kind    = as.character(unit_kind)
        ) %>%
        tidyr::complete(
          Item_display,
          unit_kind,
          serie_code = scen_allowed
        ) %>%
        dplyr::group_by(Item_display, unit_kind, serie_code) %>%
        dplyr::summarise(
          present_any = any(!is.na(value_raw)),
          value_raw   = sum(value_raw, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          is_imputed = !present_any,
          value_raw  = dplyr::if_else(is_imputed, 0, value_raw)
        ) %>%
        # >>> FIX UNIT_KIND: force ruminants in TLU, others in HEADS <<<
        dplyr::mutate(
          unit_kind = dplyr::if_else(Item_display %in% TLU_ITEMS_DISPLAY, "tlu", "heads")
        ) %>%
        dplyr::mutate(
          serie_f = factor(serie_code, levels = scen_allowed)
        ) %>%
        dplyr::arrange(Item_display, serie_f)
      
      
      res2
    }) %>% shiny::bindCache(cache_key())
    
    # --- 3) Liste des espèces (ordre fixe) ---------------------------------
    items <- reactive({
      d <- df_compact()
      if (nrow(d) == 0) return(character(0))
      
      wanted <- c(
        "Beef cattle",
        "Dairy cattle",
        "Sheep and goats meat",
        "Pigs",
        "Poultry eggs",
        "Poultry meat"
      )
      
      present <- d %>%
        dplyr::distinct(Item_display) %>%
        dplyr::pull(Item_display)
      
      wanted[wanted %in% present]
    }) %>% shiny::bindCache(cache_key())
    
    # helper for card titles with unit
    item_title <- function(sp) {
      if (sp %in% TLU_ITEMS_DISPLAY) paste0(sp, " (TLU)") else paste0(sp, " (head)")
    }
    
    # --- 4) UI : cartes en grille, 3 par ligne ------------------------------
    output$plots_grid <- renderUI({
      it <- items()
      if (length(it) == 0) {
        return(
          div(
            class = "alert alert-warning",
            strong("No livestock data for the selected country.")
          )
        )
      }
      
      cards <- lapply(it, function(sp) {
        plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(sp)))
        div(
          class = "u-card u-card--flat u-card--hover ls-stock-card",
          div(
            class = "u-box",
            p(class = "u-title", item_title(sp)),
            plotly::plotlyOutput(ns(plotname), height = 280)
          )
        )
      })
      
      rows <- list()
      i <- 1
      while (i <= length(cards)) {
        slice <- cards[i:min(i + 2L, length(cards))]
        cols  <- lapply(slice, function(card) column(width = 4, card))
        rows[[length(rows) + 1L]] <- fluidRow(cols)
        i <- i + 3L
      }
      
      tagList(rows)
    })
    
    # --- 5) Plots -----------------------------------------------------------
    observe({
      d_all <- df_compact()
      it    <- items()
      if (nrow(d_all) == 0 || length(it) == 0) return(invisible())
      
      series_lvls <- scen_levels_effective()
      pal <- scenario_palette(levels = series_lvls)
      
      b <- scen_base()
      
      base_tbl <- d_all %>%
        dplyr::filter(as.character(serie_code) == b) %>%
        dplyr::group_by(Item_display) %>%
        dplyr::summarise(base_val = sum(value_raw, na.rm = TRUE), .groups = "drop")
      
      is_pig_item_raw <- function(x) grepl("(pig|porc|swine|hog|pork)", x, ignore.case = TRUE)
      
      for (sp in it) local({
        specie   <- sp
        plotname <- paste0("plot_", gsub("[^a-zA-Z0-9]+", "_", tolower(specie)))
        
        df0 <- d_all %>%
          dplyr::filter(Item_display == specie) %>%
          dplyr::left_join(base_tbl, by = "Item_display") %>%
          dplyr::mutate(
            serie_code_chr = as.character(serie_code),
            show_pct       = serie_code_chr %in% SHOW_PCT_CODES
          )
        
        # --- échelle (une seule fois par espèce) ---
        is_pig_spec <- is_pig_item_raw(specie)
        is_tlu_spec <- any(df0$unit_kind == "tlu")
        
        scale_factor <- dplyr::case_when(
          is_tlu_spec ~ 1,
          is_pig_spec ~ 1/1000,
          TRUE        ~ 1/1e6
        )
        
        axis_lab <- dplyr::case_when(
          is_tlu_spec ~ "1000 TLU",
          is_pig_spec ~ "thousand heads",
          TRUE        ~ "million heads"
        )
        
        y_suffix <- dplyr::case_when(
          is_tlu_spec ~ "",
          is_pig_spec ~ " k",
          TRUE        ~ " M"
        )
        
        # base (référence) en unités "plot"
        base_val_raw <- df0 %>%
          dplyr::distinct(base_val) %>%
          dplyr::pull(base_val)
        base_val_raw <- base_val_raw[1]
        base_plot_scalar <- base_val_raw * scale_factor
        
        # --- 1 ligne par scénario, puis calculs ---
        df_sp <- df0 %>%
          dplyr::group_by(serie_code_chr) %>%
          dplyr::summarise(
            value_raw = sum(value_raw, na.rm = TRUE),
            show_pct  = dplyr::first(show_pct),
            # si mélange imputé/non-imputé : on considère NON imputé
            is_imputed = base::all(is_imputed),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            serie_f = factor(serie_code_chr, levels = series_lvls),
            
            value_plot = value_raw * scale_factor,
            base_plot  = base_plot_scalar,
            
            delta_pct = dplyr::if_else(
              is.finite(base_plot) & base_plot > 0,
              100 * (value_plot / base_plot - 1),
              NA_real_
            ),
            
            lbl = dplyr::case_when(
              # baseline : rien
              serie_code_chr == b ~ "",
              
              !show_pct ~ "",
              
              # scénario affiché mais absent de fact -> imputé à 0
              is_imputed & is.finite(base_plot) & base_plot > 0 ~ "-100%",
              
              # sinon on n'affiche pas de label si barre à 0 (évite du texte au pied)
              value_plot <= 0 ~ "",
              
              is.finite(delta_pct) ~ paste0(
                dplyr::if_else(delta_pct > 0, "+", ""),
                scales::number(delta_pct, accuracy = 1, big.mark = " ", decimal.mark = "."),
                "%"
              ),
              
              TRUE ~ ""
            ),
            
            axis_lab = axis_lab,
            y_suffix = y_suffix
          )
        
        
        unit_label <- unique(df_sp$axis_lab)
        if (length(unit_label) != 1L) unit_label <- unit_label[1L]
        
        ymax <- max(df_sp$value_plot, na.rm = TRUE)
        if (!is.finite(ymax)) ymax <- 0
        gap  <- ymax * 0.03
        ypad <- ymax * 0.16 + gap
        
        y_suffix <- unique(df_sp$y_suffix)
        if (length(y_suffix) != 1L) y_suffix <- y_suffix[1L]
        
        output[[plotname]] <- plotly::renderPlotly({
          shiny::validate(shiny::need(nrow(df_sp) > 0, "No data for this species."))
          
          p <- ggplot2::ggplot(
            df_sp,
            ggplot2::aes(x = serie_f, y = value_plot, fill = serie_f)
          ) +
            ggplot2::geom_col(width = 0.7, alpha = 0.95) +
            ggplot2::geom_text(
              ggplot2::aes(label = lbl),
              position = ggplot2::position_nudge(y = gap),
              vjust = 0, size = 3, lineheight = 0.95
            ) +
            ggplot2::scale_x_discrete(labels = function(x) rep("", length(x))) +
            ggplot2::scale_y_continuous(
              limits = c(0, ymax + ypad),
              labels = scales::label_number(
                accuracy = 1,
                big.mark = " ",
                decimal.mark = ".",
                suffix = y_suffix
              )
            ) +
            ggplot2::labs(x = NULL, y = unit_label) +
            ggplot2::theme_minimal(base_size = 8) +
            ggplot2::theme(
              legend.position = "none",
              axis.text.x  = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
              plot.margin = grid::unit(c(4, 24, 12, 40), "pt"),
              panel.background = ggplot2::element_rect(fill = NA, colour = NA),
              plot.background  = ggplot2::element_rect(fill = NA, colour = NA)
            ) +
            ggplot2::scale_fill_manual(
              values = pal[levels(df_sp$serie_f)],
              drop   = FALSE
            )
          
          pl <- plotly::ggplotly(p, tooltip = c("x", "y"))
          
          # >>> FIX: remove the spurious text shown at the base of bars <<<
          # ggplotly can inject 'text' into bar traces; ensure bar traces never display text.
          for (i in seq_along(pl$x$data)) {
            if (isTRUE(pl$x$data[[i]]$type %in% c("bar"))) {
              pl$x$data[[i]]$text <- NULL
              pl$x$data[[i]]$texttemplate <- NULL
              pl$x$data[[i]]$textposition <- "none"
            }
          }
          
          pl <- plotly::layout(
            pl,
            showlegend = FALSE,
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor  = "rgba(0,0,0,0)",
            margin = list(l = 40, r = 24, b = 36, t = 8, pad = 0)
          )
          
          pl <- plotly::config(
            pl,
            displayModeBar = TRUE,
            modeBarButtonsToRemove = c(
              "lasso2d", "select2d", "zoom2d", "zoomIn2d",
              "zoomOut2d", "autoScale2d", "pan2d", "resetScale2d"
            ),
            modeBarButtonsToAdd = list("toImage"),
            toImageButtonOptions = list(
              format = "png",
              filename = paste0("livestock_", gsub("[^a-zA-Z0-9]+","_", specie)),
              width = 1600,
              height = 900,
              scale = 2
            )
          )
          
          pl
        })
      })
    })
    
    # --- Download CSV : unité alignée à l'affichage -------------------------
    output$dl_livestock_stocks_csv <- downloadHandler(
      filename = function() {
        paste0("Livestock_stocks_", gsub(" ", "_", r_country()), ".csv")
      },
      content = function(file) {
        d <- df_compact()
        shiny::req(nrow(d) > 0)
        
        out <- d %>%
          dplyr::mutate(
            is_pig = grepl("(pig|porc|swine|hog|pork)", Item_display, ignore.case = TRUE),
            is_tlu = (unit_kind == "tlu"),
            
            scale_factor = dplyr::case_when(
              is_tlu ~ 1,
              is_pig ~ 1/1000,
              TRUE   ~ 1/1e6
            ),
            
            Unit_display = dplyr::case_when(
              is_tlu ~ "1000 TLU",
              is_pig ~ "1000 heads",
              TRUE   ~ "million heads"
            ),
            
            Value_display = value_raw * scale_factor
          ) %>%
          dplyr::arrange(Item_display, serie_f) %>%
          dplyr::transmute(
            Country        = r_country(),
            Species        = Item_display,
            Scenario_code  = as.character(serie_code),
            Scenario_label = scenario_label_vec(serie_code),
            Value          = Value_display,
            Unit           = Unit_display,
            Is_imputed     = is_imputed
          )
        
        readr::write_delim(out, file, delim = ";")
      }
    )
    
    # --- Note explicative ---------------------------------------------------
    output$note <- renderUI({
      d <- df_compact()
      if (nrow(d) == 0) return(NULL)
      
      txt <- glue::glue(
        "<p>
    This set of charts shows, for the selected country, the stock of animals by species in the base year and under the selected scenarios.<br>
    For <strong>Beef cattle</strong>, <strong>Dairy cattle</strong> and <strong>Sheep and goats meat</strong>, values are taken in <strong>TLU</strong> (Tropical Livestock Unit) and displayed in <strong>1000 TLU</strong>.<br>
    For the other species groups, values are shown as <strong>number of heads</strong> (in million heads, and thousand heads for pigs when needed).<br>
    The percentage labels above the bars indicate the change compared with the baseline for each scenario.
    </p>"
      )
      htmltools::HTML(txt)
    })
    
    invisible(NULL)
  })
}
