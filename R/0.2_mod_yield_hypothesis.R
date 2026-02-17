# R/0.2_mod_yield_hypothesis.R
# -------------------------------------------------

mod_yield_ui <- function(id, wrap_in_card = TRUE){
  ns <- NS(id)
  
  content <- tagList(
    
    # ---- Titre du module -------------------------------------------------
    h2("Crop yield assumptions"),
    
    # ---- Graphique principal --------------------------------------------
    plotly::plotlyOutput(ns("plot"), height = "auto"),
    
    # ---- Bouton CSV ------------------------------------------------------
    div(
      class = "u-actions",
      shiny::downloadLink(
        ns("dl_csv"),
        label = tagList(shiny::icon("download"), "CSV")
      )
    ),
    
    # ---- Note de bas de module (HTML) -----------------------------------
    uiOutput(ns("note"))
  )
  
  if (isTRUE(wrap_in_card)) {
    div(class = "card",
        div(class = "card-body", content))
  } else {
    content
  }
}


mod_yield_server <- function(
    id,
    fact_reactive,  # reactive({ fact })
    country_sel     # reactive du pays (Region)
){
  moduleServer(id, function(input, output, session) {
    
    # --- Note de base ------------------------------------------------------
    output$note <- renderUI({
      htmltools::HTML("
        <p>
          Projected 2050 yields are computed in two steps. First, for each crop and country, we close <strong>50%</strong> of the yield gap between
          the observed baseline yield in <strong>2018</strong> (FAOstat) and the <em>maximum attainable yield</em> provided by Mueller et al. (2012).<br>
          Second, the resulting yield is multiplied by a climate-change yield coefficient provided by Müller, C and al. (2014).<br>
          Thus, 2050 yields combine an agronomic catch-up effect (partial closure of the yield gap)
          and the net effect of climate change relative to the 2018 baseline
          (more informations in the 'about' tab).
        </p>
      ")
    })
    
    # ---------------------------------------------------------------
    # 1. Données pour les rendements de cultures (Yield)
    # ---------------------------------------------------------------
    dat_plot <- reactive({
      req(fact_reactive(), country_sel())
      fact <- fact_reactive()
      
      fact %>%
        dplyr::filter(
          .data$Element == "Yield",
          .data$Region  == country_sel(),
          .data$Year %in% c(2018, 2050)
        ) %>%
        dplyr::group_by(Item, Year) %>%
        dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Value = Value * 1000) %>%  # 1000 t/ha -> t/ha
        dplyr::group_by(Item) %>%
        dplyr::filter(dplyr::n() == 2) %>%
        dplyr::ungroup() %>%
        { df_tmp <- .
        ord <- df_tmp %>%
          dplyr::filter(Year == 2050) %>%
          dplyr::arrange(dplyr::desc(Value)) %>%
          dplyr::pull(Item)
        df_tmp$Item <- factor(df_tmp$Item, levels = ord)
        df_tmp %>% dplyr::mutate(Year = as.character(Year))
        }
    })
    
    # ---------------------------------------------------------------
    # 2. Téléchargement CSV (uniquement cultures)
    # ---------------------------------------------------------------
    output$dl_csv <- downloadHandler(
      filename = function() {
        ctry <- country_sel()
        paste0("yield_", gsub("\\s+", "_", ctry), "_2018_2050.csv")
      },
      content = function(file) {
        df_long <- dat_plot()
        df_wide <- tidyr::pivot_wider(
          df_long,
          names_from  = Year,
          values_from = Value
        ) %>%
          dplyr::mutate(
            Region = country_sel(),
            Unite  = "t/ha"
          ) %>%
          dplyr::select(Region, Item, `2018`, `2050`, Unite)
        
        utils::write.csv(df_wide, file,
                         row.names = FALSE, fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )
    
    # ---------------------------------------------------------------
    # 3. Graphique principal (2018 vs 2050)
    # ---------------------------------------------------------------
    output$plot <- plotly::renderPlotly({
      df <- dat_plot()
      shiny::validate(shiny::need(nrow(df) > 0, "No data available (Yield 2018 and 2050)."))
      
      th <- get_plotly_tokens()
      
      df <- df %>%
        dplyr::mutate(
          Year = factor(as.character(Year), levels = c("2018", "2050"))
        )
      
      n_items <- dplyr::n_distinct(df$Item)
      h <- max(520, 32 * n_items + 260)
      
      shiny::validate(shiny::need(exists("PALETTE_YEARS", inherits = TRUE),
                                  "Palette 'PALETTE_YEARS' not loaded."))
      pal  <- get("PALETTE_YEARS", inherits = TRUE)
      cols <- pal[c("2018", "2050")]
      shiny::validate(shiny::need(all(!is.na(cols)),
                                  "Palette must define '2018' and '2050'."))
      
      d18 <- df %>% dplyr::filter(Year == "2018")
      d50 <- df %>% dplyr::filter(Year == "2050")
      
      p <- plotly::plot_ly(height = h)
      
      p <- p %>% plotly::add_bars(
        data = d18,
        x = ~Item, y = ~Value,
        name   = "Yields in 2018",
        marker = list(color = unname(cols["2018"])),
        offsetgroup    = "2018",
        alignmentgroup = "year",
        width          = 0.36,
        hovertemplate  = "<b>%{x}</b><br>Year: 2018<br>Yield: %{y:.4f} t/ha<extra></extra>"
      )
      
      p <- p %>% plotly::add_bars(
        data = d50,
        x = ~Item, y = ~Value,
        name   = "Yields in 2050",
        marker = list(color = unname(cols["2050"])),
        offsetgroup    = "2050",
        alignmentgroup = "year",
        width          = 0.36,
        hovertemplate  = "<b>%{x}</b><br>Year: 2050<br>Yield: %{y:.4f} t/ha<extra></extra>"
      )
      
      p <- p %>% plotly::layout(
        barmode = "group",
        bargap  = 0.35,
        legend  = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = 1.05, yanchor = "bottom",
          traceorder = "normal"
        ),
        xaxis   = list(
          title      = "",
          automargin = TRUE,
          ticks      = "outside",
          ticklen    = 6,
          tickangle  = -45
        ),
        yaxis   = list(
          title    = "t/ha",
          zeroline = TRUE,
          showline = FALSE,
          showgrid = TRUE,
          dtick    = 5
        ),
        margin  = list(l = 80, r = 40, t = 80, b = 120)
      )
      
      p <- plotly_apply_global_theme(p, bg = "transparent", grid = "y")
      p
    })
    
  })
}
