# R/20_continent_maps.R
# ============================================================
# Cartes CONTINENT calculées dynamiquement (au lieu de PNG statiques)
# Source des données : data_raw/BDD_CLEAN.csv
# Fond de carte       : ne_10m_admin_0_countries.shp (jointure sur ISO_3)
# ============================================================
# ⚠️ CONSTANTES À VÉRIFIER / ADAPTER selon les noms exacts dans BDD_CLEAN.csv
# ============================================================

# --- Noms de colonnes attendus dans BDD_CLEAN.csv --------------------------
# Structure "longue" : Region (nom de pays en anglais), Scenario, Item,
# Element, Year (2018 ou 2050), Value. Adaptez ici si les noms diffèrent.
COL_REGION   <- "Region"
COL_SCENARIO <- "Scenario"
COL_ITEM     <- "Item"
COL_ELEMENT  <- "Element"
COL_YEAR     <- "Year"
COL_VALUE    <- "Value"

# Nom de la colonne ISO_3 ajoutée par load_bdd_clean() (via REGION_TO_ISO3,
# défini dans app.R) — c'est cette colonne qui sert ensuite à la jointure
# avec le fond de carte.
COL_ISO3     <- "ISO_3"

# --- Années de référence ----------------------------------------------------
BASE_YEAR_MAP   <- 2018
TARGET_YEAR_MAP <- 2050

# --- Elements / Items utilisés -----------------------------------------------
ELEMENT_AREA             <- "Area"
ELEMENT_ENERGY_IMPORT    <- "Energy Import Quantity"
ELEMENT_ENERGY_FEED      <- "Energy Feed"
ELEMENT_ENERGY_FOOD      <- "Energy Food"
ELEMENT_ENERGY_OTHERUSES <- "Energy Other uses (non-food)"
ELEMENT_EMISSIONS        <- "Emissions"

ITEM_ALL      <- "All"
ITEM_CROPLAND <- "Cropland"
ITEM_PASTURES <- "Land under perm. meadows and pastures"
ITEM_FOREST   <- "Forest land"

EMISSIONS_ITEMS <- c(
  "Energy for crops",
  "Enteric",
  "Fertilizer application and production and pesticides",
  "Livestock other"
)

# --- Couleurs de base (réutilisées dans les specs de légende ci-dessous) ---
# vert pâle a été remplacé par un vert plus marqué ("vert clair"), et le jaune
# rendu plus saturé pour bien apparaître à l'écran/à l'impression.
COL_VERT_SAPIN <- "#1B5E20"
COL_VERT_CLAIR <- "#66BB6A"   # anciennement "vert pâle" (#A5D6A7), trop pâle
COL_JAUNE      <- "#FFD600"
COL_ORANGE     <- "#FB8C00"
COL_ROUGE      <- "#E53935"
COL_NA         <- "#BDBDBD"

# --- Specs de légende par rubrique -------------------------------------------
# Chaque spec liste, DANS L'ORDRE d'affichage de la légende, les catégories
# réellement atteignables pour cette carte (le "Non disponible" est ajouté
# automatiquement par plot_continent_choropleth). `code` doit correspondre
# exactement aux valeurs de `category` produites par compute_*_map().

DEPENDENCY_SPEC <- data.frame(
  code  = c("vert sapin", "vert pâle", "jaune", "orange", "rouge"),
  label = c(
    "Forte baisse (< -10 pts)",
    "Baisse modérée (-10 à -3 pts)",
    "Stable (-3 à +3 pts)",
    "Hausse modérée (+3 à +10 pts)",
    "Forte hausse (≥ +10 pts)"
  ),
  color = c(COL_VERT_SAPIN, COL_VERT_CLAIR, COL_JAUNE, COL_ORANGE, COL_ROUGE),
  stringsAsFactors = FALSE
)

LANDUSE_SPEC <- data.frame(
  code  = c("vert sapin", "vert pâle", "orange", "rouge"),
  label = c(
    "Agri. diminue, forêt augmente (agri. ≤ 80% du territoire en 2050)",
    "Agri. diminue, forêt augmente (agri. > 80% du territoire en 2050)",
    "Agri. augmente avec perte de forêt ≤ 20%, ou surface agricole > territoire disponible",
    "Agri. augmente, perte de forêt > 20%"
  ),
  color = c(COL_VERT_SAPIN, COL_VERT_CLAIR, COL_ORANGE, COL_ROUGE),
  stringsAsFactors = FALSE
)

EMISSIONS_ABS_SPEC <- data.frame(
  code  = c("vert pâle", "jaune", "orange", "rouge"),
  label = c(
    "< 0 Mt CO2eq",
    "0 à 10 Mt CO2eq",
    "10 à 100 Mt CO2eq",
    "≥ 100 Mt CO2eq"
  ),
  color = c(COL_VERT_CLAIR, COL_JAUNE, COL_ORANGE, COL_ROUGE),
  stringsAsFactors = FALSE
)

EMISSIONS_VAR_SPEC <- data.frame(
  code  = c("vert pâle", "jaune", "orange", "rouge"),
  label = c(
    "Baisse (v < 0%)",
    "0% à 100%",
    "100% à 1000%",
    "≥ 1000%"
  ),
  color = c(COL_VERT_CLAIR, COL_JAUNE, COL_ORANGE, COL_ROUGE),
  stringsAsFactors = FALSE
)

# ============================================================
# CHARGEMENT DES DONNÉES (à appeler une seule fois au démarrage de l'app)
# ============================================================

# Charge et normalise BDD_CLEAN.csv :
# - Scenario recodé avec scenario_code() pour matcher les mêmes codes que `fact$Scenario`
# - Region (nom de pays anglais) convertie en ISO_3 via `region_to_iso3` (table de
#   correspondance fournie par l'appelant, ex. REGION_TO_ISO3 défini dans app.R).
#   Certaines entrées de `region_to_iso3` peuvent pointer vers PLUSIEURS codes ISO_3
#   (ex. "North and South Sudan" -> c("SDN","SSD")) : la ligne source est alors
#   dupliquée sur chacun des pays correspondants.
load_bdd_clean <- function(path, region_to_iso3){
  df <- readr::read_csv(path, show_col_types = FALSE)

  req_cols <- c(COL_REGION, COL_SCENARIO, COL_ITEM, COL_ELEMENT, COL_YEAR, COL_VALUE)
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols)) {
    stop(
      "BDD_CLEAN.csv : colonnes manquantes : ", paste(missing_cols, collapse = ", "),
      ". Vérifiez/adaptez les constantes COL_* en haut de R/20_continent_maps.R."
    )
  }

  df[[COL_REGION]]   <- stringr::str_squish(as.character(df[[COL_REGION]]))
  df[[COL_SCENARIO]] <- scenario_code(df[[COL_SCENARIO]])

  region_unmatched <- setdiff(unique(df[[COL_REGION]]), names(region_to_iso3))
  if (length(region_unmatched)) {
    message(
      "R/20_continent_maps.R : Region(s) sans correspondance ISO_3 dans REGION_TO_ISO3 (ignorées sur les cartes) : ",
      paste(region_unmatched, collapse = ", ")
    )
  }

  # Duplique les lignes pour les Region qui correspondent à plusieurs ISO_3
  # (ex : "North and South Sudan" -> "SDN" et "SSD"). On filtre d'abord sur les
  # Region connues de region_to_iso3, pour éviter les entrées NULL au unnest().
  df <- df[df[[COL_REGION]] %in% names(region_to_iso3), , drop = FALSE]
  df[[COL_ISO3]] <- region_to_iso3[df[[COL_REGION]]]
  df <- tidyr::unnest(df, cols = dplyr::all_of(COL_ISO3))
  df[[COL_ISO3]] <- toupper(stringr::str_squish(as.character(df[[COL_ISO3]])))

  df
}

# Charge le fond de carte. Essaie ISO_3 puis ISO_A3 (nom standard Natural Earth)
# si la colonne ISO_3 n'existe pas telle quelle.
load_continent_shapefile <- function(path){
  shp <- sf::st_read(path, quiet = TRUE)

  if (!("ISO_3" %in% names(shp))) {
    if ("ISO_A3" %in% names(shp)) {
      shp <- dplyr::rename(shp, ISO_3 = ISO_A3)
      message("R/20_continent_maps.R : colonne 'ISO_3' absente du shapefile, 'ISO_A3' utilisée à la place.")
    } else {
      stop("Shapefile : ni 'ISO_3' ni 'ISO_A3' trouvée. Colonnes disponibles : ",
           paste(names(shp), collapse = ", "))
    }
  }
  shp$ISO_3 <- toupper(stringr::str_squish(as.character(shp$ISO_3)))
  shp
}

# --- Code du scénario "Année de base" (2018) --------------------------------
# Les valeurs 2018 ne sont PAS dupliquées sous chaque scénario dans BDD_CLEAN.csv :
# elles existent uniquement sous ce scénario spécial. Les valeurs 2050, elles,
# sont bien spécifiques au scénario choisi.
.base_year_scenario_code <- function(){
  if (exists("SC", inherits = TRUE)) {
    sc_obj <- get("SC", inherits = TRUE)
    if (is.list(sc_obj) && !is.null(sc_obj$base_year)) return(sc_obj$base_year)
  }
  scenario_code("Année de base")
}

# ============================================================
# CALCULS PAR RUBRIQUE (retournent un data.frame ISO_3 + category)
# ============================================================

# ---- DEPENDENCY -------------------------------------------------------------
# Ratio = Energy Import Quantity / (Feed+Food+Other uses), Item "All".
# Le 2018 est lu sous le scénario "Année de base" ; le 2050 sous le scénario choisi.
compute_dependency_map <- function(bdd, scenario){
  base_scenario <- .base_year_scenario_code()

  ratio_for <- function(scen, year){
    bdd %>%
      dplyr::filter(
        .data[[COL_SCENARIO]] == scen,
        .data[[COL_ITEM]] == ITEM_ALL,
        .data[[COL_ELEMENT]] %in% c(ELEMENT_ENERGY_IMPORT, ELEMENT_ENERGY_FEED, ELEMENT_ENERGY_FOOD, ELEMENT_ENERGY_OTHERUSES),
        .data[[COL_YEAR]] == year
      ) %>%
      dplyr::group_by(.data[[COL_ISO3]]) %>%
      dplyr::summarise(
        import_qty = sum(.data[[COL_VALUE]][.data[[COL_ELEMENT]] == ELEMENT_ENERGY_IMPORT], na.rm = TRUE),
        denom      = sum(.data[[COL_VALUE]][.data[[COL_ELEMENT]] %in% c(ELEMENT_ENERGY_FEED, ELEMENT_ENERGY_FOOD, ELEMENT_ENERGY_OTHERUSES)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ratio = dplyr::if_else(denom > 0, import_qty / denom, NA_real_)) %>%
      dplyr::select(ISO_3 = 1, ratio)
  }

  d_base   <- ratio_for(base_scenario, BASE_YEAR_MAP)   %>% dplyr::rename(ratio_base = ratio)
  d_target <- ratio_for(scenario,      TARGET_YEAR_MAP) %>% dplyr::rename(ratio_target = ratio)

  dplyr::full_join(d_base, d_target, by = "ISO_3") %>%
    dplyr::mutate(
      r = ratio_target - ratio_base,
      category = dplyr::case_when(
        is.na(r)  ~ NA_character_,
        r < -0.10 ~ "vert sapin",
        r < -0.03 ~ "vert pâle",
        r < 0.03  ~ "jaune",
        r < 0.10  ~ "orange",
        TRUE      ~ "rouge"
      )
    ) %>%
    dplyr::select(ISO_3, r, category)
}

# ---- LAND USE ----------------------------------------------------------------
# c, f = variations ABSOLUES (2050 - 2018) des surfaces agricoles / forestières.
# Le 2018 est lu sous le scénario "Année de base" ; le 2050 sous le scénario choisi.
compute_landuse_map <- function(bdd, scenario){
  base_scenario <- .base_year_scenario_code()

  areas_for <- function(scen, year){
    bdd %>%
      dplyr::filter(
        .data[[COL_SCENARIO]] == scen,
        .data[[COL_ELEMENT]] == ELEMENT_AREA,
        .data[[COL_ITEM]] %in% c(ITEM_CROPLAND, ITEM_PASTURES, ITEM_FOREST),
        .data[[COL_YEAR]] == year
      ) %>%
      dplyr::group_by(.data[[COL_ISO3]]) %>%
      dplyr::summarise(
        agri   = sum(.data[[COL_VALUE]][.data[[COL_ITEM]] %in% c(ITEM_CROPLAND, ITEM_PASTURES)], na.rm = TRUE),
        forest = sum(.data[[COL_VALUE]][.data[[COL_ITEM]] == ITEM_FOREST], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(total = agri + forest) %>%
      dplyr::select(ISO_3 = 1, agri, forest, total)
  }

  d_base   <- areas_for(base_scenario, BASE_YEAR_MAP)   %>% dplyr::rename(agri_b = agri, forest_b = forest, total_b = total)
  d_target <- areas_for(scenario,      TARGET_YEAR_MAP) %>% dplyr::rename(agri_t = agri, forest_t = forest, total_t = total)

  dplyr::full_join(d_base, d_target, by = "ISO_3") %>%
    dplyr::mutate(
      c_evol          = agri_t   - agri_b,
      f_evol          = forest_t - forest_b,
      f_pct           = dplyr::if_else(forest_b > 0, f_evol / forest_b, NA_real_),
      agri_share_2050 = dplyr::if_else(total_t > 0, agri_t / total_t, NA_real_),
      category = dplyr::case_when(
        is.na(c_evol) | is.na(f_evol) ~ NA_character_,
        agri_t > total_b                                    ~ "rouge",
        c_evol <= 0 & f_evol > 0 & agri_share_2050 <= 0.80 ~ "vert sapin",
        c_evol <= 0 & f_evol > 0 & agri_share_2050 >  0.80 ~ "vert pâle",
        c_evol >  0 & f_evol <  0 & f_pct >= -0.20          ~ "orange",
        c_evol >  0 & f_evol <  0 & f_pct <  -0.20          ~ "rouge",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(ISO_3, c_evol, f_evol, category)
}

# ---- EMISSIONS (niveau absolu 2050) ------------------------------------------
compute_emissions_abs_map <- function(bdd, scenario){
  bdd %>%
    dplyr::filter(
      .data[[COL_SCENARIO]] == scenario,
      .data[[COL_ELEMENT]] == ELEMENT_EMISSIONS,
      .data[[COL_ITEM]] %in% EMISSIONS_ITEMS,
      .data[[COL_YEAR]] == TARGET_YEAR_MAP
    ) %>%
    dplyr::group_by(.data[[COL_ISO3]]) %>%
    dplyr::summarise(s = sum(.data[[COL_VALUE]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::select(ISO_3 = 1, s) %>%
    dplyr::mutate(category = dplyr::case_when(
      is.na(s)   ~ NA_character_,
      s < 0      ~ "vert pâle",
      s < 10e6   ~ "jaune",
      s < 100e6  ~ "orange",
      TRUE       ~ "rouge"
    ))
}

# ---- EMISSIONS (variation 2018 -> 2050) --------------------------------------
# Le 2018 est lu sous le scénario "Année de base" ; le 2050 sous le scénario choisi.
compute_emissions_var_map <- function(bdd, scenario){
  base_scenario <- .base_year_scenario_code()

  s_for <- function(scen, year){
    bdd %>%
      dplyr::filter(
        .data[[COL_SCENARIO]] == scen,
        .data[[COL_ELEMENT]] == ELEMENT_EMISSIONS,
        .data[[COL_ITEM]] %in% EMISSIONS_ITEMS,
        .data[[COL_YEAR]] == year
      ) %>%
      dplyr::group_by(.data[[COL_ISO3]]) %>%
      dplyr::summarise(s = sum(.data[[COL_VALUE]], na.rm = TRUE), .groups = "drop") %>%
      dplyr::select(ISO_3 = 1, s)
  }

  d_base   <- s_for(base_scenario, BASE_YEAR_MAP)   %>% dplyr::rename(s_base = s)
  d_target <- s_for(scenario,      TARGET_YEAR_MAP) %>% dplyr::rename(s_target = s)

  dplyr::full_join(d_base, d_target, by = "ISO_3") %>%
    dplyr::mutate(
      v = dplyr::if_else(s_base != 0, (s_target - s_base) / s_base, NA_real_),
      category = dplyr::case_when(
        is.na(v) ~ NA_character_,
        v < 0    ~ "vert pâle",
        v < 1    ~ "jaune",   # 100%
        v < 10   ~ "orange",  # 1000%
        TRUE     ~ "rouge"
      )
    ) %>%
    dplyr::select(ISO_3, v, category)
}

# ============================================================
# GRILLE 6 SCÉNARIOS (une rubrique, tous les scénarios, légende commune)
# ============================================================
# `compute_fn` : une des compute_*_map() ci-dessus.
# `scenarios`  : vecteur des 6 codes de scénario (ex: r_scenarios_continent()).
# `spec`       : le spec de légende correspondant (ex: LANDUSE_SPEC).
# Nécessite le package patchwork (guides="collect" fusionne les légendes
# identiques des 6 sous-cartes en une seule, positionnée en bas).
plot_continent_scenarios_grid <- function(shp, bdd, scenarios, compute_fn, spec,
                                           ncol = 3,
                                           xlim = c(-20, 55), ylim = c(-36, 38)){
  plots <- lapply(scenarios, function(code){
    df <- compute_fn(bdd, code)
    plot_continent_choropleth(
      shp = shp, df = df,
      title = scenario_label(code),
      spec = spec,
      legend_title = NULL,
      legend_position = "none",
      xlim = xlim, ylim = ylim
    )
  })
  
  patchwork::wrap_plots(plots, ncol = ncol) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5)
    )
}
# `spec` : data.frame avec colonnes code/label/color, dans l'ordre d'affichage
# voulu pour la légende (cf. DEPENDENCY_SPEC / LANDUSE_SPEC / EMISSIONS_*_SPEC
# ci-dessus). Le fait d'apparier couleurs et libellés PAR NOM (et non par
# position) évite tout décalage, y compris pour les catégories jamais
# atteintes (ex: "jaune" en Land use) ou en présence de NA.
plot_continent_choropleth <- function(shp, df, title, spec,
                                       legend_title = NULL,
                                       legend_position = "right",
                                       xlim = c(-20, 55), ylim = c(-36, 38)){
  spec <- rbind(spec, data.frame(code = "Non disponible", label = "Non disponible", color = COL_NA, stringsAsFactors = FALSE))

  joined <- shp %>%
    dplyr::left_join(df, by = "ISO_3") %>%
    dplyr::mutate(
      category = dplyr::if_else(is.na(category), "Non disponible", category),
      category = factor(category, levels = spec$code)
    )

  values_vec <- setNames(spec$color, spec$code)
  labels_vec <- setNames(spec$label, spec$code)

  ggplot2::ggplot(joined) +
    ggplot2::geom_sf(ggplot2::aes(fill = category), color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(
      values = values_vec,
      labels = labels_vec,
      breaks = spec$code,
      name   = legend_title,
      drop   = FALSE
    ) +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      legend.position = legend_position,
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}
