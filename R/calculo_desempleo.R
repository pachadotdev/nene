#' Calculo desempleo
#' @param encuesta string con la ruta a un archivo SPSS
#' @importFrom survey svydesign svytable
#' @importFrom dplyr select mutate filter case_when rename everything as_tibble
#' @importFrom haven read_sav as_factor
#' @importFrom magrittr `%>%`
#' @importFrom tidyr pivot_wider
#' @importFrom janitor adorn_totals
#' @export
calculo_desempleo <- function(encuesta) {
  stopifnot(is.character(encuesta))
  
  datos <- read_sav(encuesta)
  
  if ("id_directorio" %in% colnames(datos)) {
    datos <- datos %>% 
      rename(conglomerado = id_directorio)
  }
  
  if ("estrato" %in% colnames(datos)) {
    datos <- datos %>% 
      rename(conglomerado = id_directorio, estrato_unico = estrato)
  }
  
  datos <- datos %>% 
    mutate(
      activ2 = case_when(
        activ == 1 ~ "ocupados",
        activ == 2 ~ "desocupados",
        activ == 3 ~ "inactivos"
      )
    ) %>% 
    filter(activ2 != "inactivos")
  
  # attr(datos$activ, 'label')
  # print_labels(datos$activ)
  
  pesos <- svydesign(id = ~conglomerado, strata = ~estrato_unico, weights = ~fact_cal, data = datos)
  
  nivel_region <- svytable(~activ2+region, pesos) %>% as_tibble()
  
  desempleo <- nivel_region %>% 
    pivot_wider(names_from = "activ2", values_from = "n") %>% 
    filter(desocupados > 0) %>% 
    adorn_totals() %>% 
    mutate(
      anio = unique(datos$ano_trimestre),
      mes_central = unique(datos$mes_central),
      desocupados = round(desocupados, 0),
      ocupados = round(ocupados, 0),
      desempleo = round(100 * desocupados / (desocupados + ocupados), 1)
    ) %>% 
    select(anio, mes_central, everything()) %>% 
    as_tibble()
  
  return(desempleo)
}
