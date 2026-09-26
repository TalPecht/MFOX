apply_scope <- function(df, scope = "All menstrual-derived evidence") {
  if (scope == "Direct menstrual fluid only") {
    df |> filter(tolower(coalesce(direct_mf_scope, "")) == "yes")
  } else if (scope == "Cultured menstrual-derived cells") {
    df |> filter(derivation_class == "Cultured derivative")
  } else if (scope == "Experimental derivatives") {
    df |> filter(derivation_class == "Experimental derivative")
  } else {
    df
  }
}

nz_choices <- function(x) sort(unique(x[!is.na(x) & x != ""]))
