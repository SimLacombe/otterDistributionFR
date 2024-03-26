require(tidyverse, lubridate)


dat.filename <- "data/PNA-data/PaysdelaLoire-LPOsarthe/Export_données_loutre_SFEPM_CEFE_LPO_Sarthe 31_01_2024.xlsx"

dat <- readxl::read_xlsx(dat.filename)

dat <- dat %>% 
  mutate(Remarques = toupper(Remarques),
         tr_len = as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\dKM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\dKM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d\\d KM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(Remarques, "\\d KM")))*1000, tr_len))%>%
  mutate(PA = grepl("PNA", Remarques)|grepl("PRA", Remarques)|grepl("UICN", Remarques)|grepl("POINT", Remarques),
         PA = PA|tr_len >= 300,
         PA = ifelse(is.na(PA), FALSE, PA),
         PA.protocole = ifelse(PA, "transect", NA))%>%
  mutate(collision = FALSE,
         date = as.Date(Date),
         year = year(date),
         presence = sign(Nombre),
         data.provider = "LPO-PdL",
         loc = paste(Commune, `Lieu-dit`, sep = ".")) %>%
  rename(lon.l93 = `X Lambert93 [m]`,
         lat.l93 = `Y Lambert93 [m]`,
         grid.cell = Maille) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
