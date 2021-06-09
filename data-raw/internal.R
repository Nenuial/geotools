adm1_russia <- readxl::read_excel(here::here("inst/extdata/countrycodes/russia_adm1.xlsx"),
                                  col_types = c(rosstat.number = "text"))
adm1_switzerland <- readxl::read_excel(here::here("inst/extdata/countrycodes/switzerland_adm1.xlsx"))

crs_regional <- readxl::read_excel(here::here("inst/extdata/gis/crs_regional.xlsx"))

crs_proj <- readxl::read_excel(here::here("inst/extdata/gis/crs_proj.xlsx"))

usethis::use_data(
  adm1_russia,
  adm1_switzerland,
  crs_regional,
  crs_proj,
  internal = T,
  overwrite = T
)
