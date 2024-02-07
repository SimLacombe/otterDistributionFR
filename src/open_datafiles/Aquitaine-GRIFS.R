require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/Aquitaine-GRIFS/GRIFS_Point_Loutre_202112-202301.csv"

dat <- read.csv(dat.filename, sep = "\t")
dat <- dat %>% 
  mutate(PNA.protocole = FALSE, 
         date = as.Date(DateDebut),
         loc = NomCom,
         presence = as.numeric(StatPresen=="présent"),
         region = "Aquitaine",
         data.provider = "GRIFS",
         grid.cell = Maille10,
         year = year(DateDebut)) %>%
  rowwise() %>%
  mutate(lon.l93 = as.numeric(strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][1]),
         lat.l93 = as.numeric(strsplit(str_extract(string = GeomWkt, pattern = "(?<=\\().*(?=\\))"), " ")[[1]][2])) %>% 
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
