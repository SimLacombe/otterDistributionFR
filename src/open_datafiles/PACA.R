require(tidyverse, lubridate)


dat.filename <- "data/PNA-data/PACA-LPOPACA/Copie de Données LPO PACA.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT),
         tr_len = as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d.\\dKM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\dKM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d.\\d KM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(COMMENT, "\\d KM")))*1000, tr_len))%>%
  mutate(PA = grepl("E\\d\\d\\dN\\d\\d\\d", COMMENT)|grepl("E\\d\\dN\\d\\d\\d", COMMENT)|
           grepl("EO\\d\\dN\\d\\d\\d", COMMENT)|grepl("EN\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|
           grepl("E\\d\\d\\dN\\d\\d\\d", PRIVATE_COMMENT)|grepl("PNA", COMMENT),
         PA = PA|tr_len >= 300,
         PA = ifelse(is.na(PA), FALSE, PA)) %>%
  mutate(PA.protocole = ifelse(PA, "transect", NA),
         collision = FALSE,
         date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         data.provider = "LPO-PACA",
         loc = paste(MUNICIPALITY, PLACE, sep = ".")) %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  select(data.provider, PA, PA.protocole, collision, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
