adm1_russia <- readxl::read_excel(here::here("inst/extdata/countrycodes/russia_adm1.xlsx"),
                                  col_types = c(rosstat.number = "text"))

adm1_switzerland <- readxl::read_excel(here::here("inst/extdata/countrycodes/switzerland_adm1.xlsx"))

adm1_china <- readxl::read_excel(here::here("inst/extdata/countrycodes/china_adm1.xlsx"))

crs_regional <- readxl::read_excel(here::here("inst/extdata/gis/crs_regional.xlsx"))

crs_proj <- readxl::read_excel(here::here("inst/extdata/gis/crs_proj.xlsx"))

# Taken from ?demography::hmd (needs quick cleanup)
hmd_codes <- readr::read_tsv(here::here("inst/extdata/hmdcodes/HMD_codes.tsv"), col_types = "cc")

usethis::use_data(
  adm1_china,
  adm1_russia,
  adm1_switzerland,
  crs_regional,
  crs_proj,
  hmd_codes,
  internal = T,
  overwrite = T
)
