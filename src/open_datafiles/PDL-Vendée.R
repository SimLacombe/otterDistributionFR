require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT))%>%
  mutate(PNA.protocole = NAME == "Marais Breton"|TRA_NAME == "Marais Breton"|TRA_SURNAME == "Rnrvacherie"|
           grepl("Rnrvacherie", COMMENT)|grepl("RNNBA", COMMENT)|
           grepl("LOUTRE LITTORAL", COMMENT)|TRA_NAME == "Dupé"|TRA_NAME == "Dupé (lpo)",
         date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         data.provider = "LPO-PdL",
         loc = paste(MUNICIPALITY, PLACE, sep = ".")) %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  select(data.provider, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
