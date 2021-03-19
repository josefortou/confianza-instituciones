# libraries ####

library(tidyverse)
library(readxl)
library(janitor)
library(naniar)
library(pscl)

# data ####

# load data
wvs <- read_excel("data/Data_0814-18000010 Encuesta Mundial De Valores (Con Etiquetas)_Anonimizado.xlsx")

# NA values ####

# define NA strings
na_strings <- c("NS/NR", "NO aplica", "No aplica", "no sabe", "no responde",
                "No sabe/ no responde", "999")

# clean data
wvs <- wvs %>%
  # lower case column names
  clean_names() %>%
  # add subject id
  rownames_to_column("subject_id") %>%
  # change NA
  replace_with_na_all(condition = ~.x %in% na_strings)

# factor recoding ####

# numeric scales
wvs <- wvs %>%
  mutate(
    across(
      c(q48:q50, q90, q106:q110, q112, q120, q158:q164, q176, q177:q195, q240,
        q240:q252, q288), 
      parse_number
    )
  )

# recoding
wvs <- wvs %>%
  mutate(
    higher_ed = case_when(
      q275 %in% c("Doctorado o equivalente(ISCED 8)", 
                  "Maestría, máster, segundo ciclo de licenciatura o equivalente (ISCED 7)",
                  "Grado, diplomado universitario, primer ciclo de licenciatura, bachelor o equivalente (ISCED 6)",
                  "Educación terciaria de ciclo corto (ISCED 5)",
                  "Post-secundaria no terciaria (ISCED 4)") ~ "Post-secundaria", 
      q275 %in% c("Educación preescolar / Educación preescolar (temprana)/ sin educación",
                  "Educación primaria o primer ciclo de educación básica (ISCED 1)",
                  "Primer ciclo de secundaria o segundo ciclo de la educación básica (ISCED 2)",
                  "Segundo ciclo de secundaria (ISCED 3)") ~ "Otro",
      TRUE ~ NA_character_
    ),
    religious = case_when(
      q289 == "No pertenece a ninguna religión" ~ "No",
      TRUE ~ "Sí"
    ),
    victim = case_when(
      coq290 %in% c("Ambos", "Víctima del conflicto armado interno") ~ "Sí",
      TRUE ~ "No"
    ),
    marital_status = case_when(
      q273 == "Soltero" ~ "Soltero",
      q273 %in% c("Casado", "Vive en unión libre") ~ "Casado/unión",
      TRUE ~ "Otro"
    )
  )

# latent trust estimates ####

# function to dicotomize
to_binary <- function(x) {
  case_when(
    x %in% c("Mucha", "Bastante") ~ 1, 
    x %in% c("No mucha", "Ninguna") ~ 0
  )
}

# select variables and convert to numeric dummies
conf_vars <- wvs %>%
  select(subject_id, q64, q66:q68, q75, q77:coq78) %>%
  mutate(across(c(q64:coq78), to_binary))

# turn into matrix
irt_matrix <- conf_vars %>%
  select(-subject_id) %>%
  as.matrix()

# keep variables names
irt_names <- conf_vars %>%
  names()

# keep subject id
irt_id <- conf_vars %>%
  pull(subject_id)

# create rollcall object
rc <- rollcall(irt_matrix, vote.names = irt_names, legis.names = irt_id)

# estimate ideal points, keep parameters
ideal_mod <- ideal(rc, d = 1, normalize = TRUE, store.item = TRUE)

# function to extract ideal positions
augment_ideal <- function(model, id) {
  tibble(
    subject_id = id,
    conf_xbar = model$xbar[, "D1"]
  )
}

# extract ideal positions
irt_results <- augment_ideal(ideal_mod, conf_vars$subject_id)

# merge with survey data
wvs <- wvs %>%
  left_join(irt_results, by = "subject_id")

# extract difficult and discrimination parameters
ideal_params <- ideal_mod$beta %>% 
  as_tibble() %>%
  pivot_longer(
    cols = everything(),
    names_to = c("item", "param"), 
    names_sep = "\\.",
    values_to = "value"
  ) %>%
  group_by(item, param) %>%
  summarize(mean_value = mean(value),
            sd_value = sd(value),
            hi_value = mean_value + 1.96*sd_value,
            lo_value = mean_value - 1.96*sd_value) %>%
  ungroup()

# save ####

write_rds(ideal_params, "output/ideal_params.rds")
write_rds(wvs, "output/wvs.rds")
