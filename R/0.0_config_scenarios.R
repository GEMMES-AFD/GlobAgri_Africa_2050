# R/0.0_config_scenarios.R
# Ce module permet de centraliser l'ordre et le nom des scénarios. Ainsi les noms, l'ordre peuvent être changés pour toute l'application !
# Par ailleurs, ce module nous permet de définir le coefficient qui determine si 2 scénarios sont redondants 
# ce qui conditionne ensuite l'affichage : aujourd'hui, il est à 00.5 
# ============================================================
# SINGLE SOURCE OF TRUTH — SCENARIOS (CODES in data, LABELS in UI)
# ============================================================

# 0) Normalisation (anti-espaces, anti-facteurs)
scenario_code <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- stringr::str_replace_all(x, "_", "-")
  x
}

# 1) Codes (DOIVENT matcher fact$Scenario)
SCENARIO_BASE_YEAR_CODE <- scenario_code("Année de base")

SCENARIOS_BASE_CODES <- scenario_code(c(
  "Même diète",
  "Diète saine",
  "Diète probable"
))

SCENARIO_REF_CODE <- scenario_code("Diète probable")

# 2) Nom du selecteur ! Extras : CODES uniquement (les noms sont des labels UI)
SCENARIOS_EXTRA_CHOICES <- c(
  "Total area stress" = "Prob-S-limitee"
)
SCENARIOS_EXTRA_CODES <- scenario_code(unname(SCENARIOS_EXTRA_CHOICES))

# 3) Labels UI : names = CODES, values = LABELS
SCENARIO_LABELS <- c(
  "Année de base"  = "Base year",
  "Même diète"     = "Same diet",
  "Diète saine"    = "Healthy diet",
  "Diète probable" = "Likely diet",
  "Prob-S-limitee" = "Total area stress"
)

scenario_label <- function(code){
  code <- scenario_code(code)
  out  <- unname(SCENARIO_LABELS[code])
  ifelse(is.na(out) | out == "", code, out)
}

# 4) Ordre global stable : CODES uniquement
SCENARIO_LEVELS_DEFAULT <- unique(c(
  SCENARIO_BASE_YEAR_CODE,
  SCENARIOS_BASE_CODES,
  SCENARIOS_EXTRA_CODES
))

# 5) Tolérances / seuil redondance (si tu veux centraliser)
SCENARIO_REDUNDANCE_TOL_REL <- 0.02

# ============================================================
# SCENARIOS CONTRACT OBJECT (unique entrée pour les modules)
# ============================================================

SC <- list(
  code = scenario_code,
  base_year = SCENARIO_BASE_YEAR_CODE,
  base_diets = SCENARIOS_BASE_CODES,
  ref = SCENARIO_REF_CODE,
  extra_choices = SCENARIOS_EXTRA_CHOICES,          # names=UI label, values=CODE
  extra_codes = SCENARIOS_EXTRA_CODES,              # vector codes
  labels = SCENARIO_LABELS,                         # names=CODE, values=label UI
  levels_default = SCENARIO_LEVELS_DEFAULT,
  tol_rel_default = SCENARIO_REDUNDANCE_TOL_REL
)

scenario_label <- function(code){
  code <- SC$code(code)
  out  <- unname(SC$labels[code])
  ifelse(is.na(out) | out == "", code, out)
}

