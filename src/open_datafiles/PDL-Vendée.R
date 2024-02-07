require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/PaysdelaLoire-LPOvendée/Copie de Donnees_Loutre_Vendee.xlsx"

dat <- readxl::read_xlsx(dat.filename, skip = 1)
names(dat) <- names(readxl::read_xlsx(dat.filename, n_max = 1))

dat <- dat %>% 
  mutate(COMMENT = toupper(COMMENT))%>%
  mutate(PNA.protocole = NAME == "Marais Breton"|TRA_NAME == "Marais Breton", 
         date = as.Date(DATE),
         year = year(date),
         presence = sign(TOTAL_COUNT),
         region = "Vendée",
         data.provider = "LPO",
         loc = paste(MUNICIPALITY, PLACE, sep = ".")) %>%
  rename(lon.l93 = COORD_LON_L93,
         lat.l93 = COORD_LAT_L93,
         grid.cell = GRID_NAME) %>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
