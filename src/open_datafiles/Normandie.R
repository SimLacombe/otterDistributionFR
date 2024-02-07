
dat1.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20210518.xlsx"
dat2.filename <- "data/PNA-data/Normandie-GMN/LUTLUT_GMN_20231010.xlsx"

dat1 <- readxl::read_xlsx(dat1.filename) %>%
  select(-`N° de parcelle`)

dat2 <- readxl::read_xlsx(dat2.filename) %>%
  rename(ORGANISME = `Nom Observateur`)

dat <- rbind(dat1,dat2) 

dat <- dat %>% 
  mutate(PNA.protocole = TRUE,
         date = as.Date(`Date\r\nobservation`),
         year = year(date),
         presence = as.numeric(`Comportement\r\nN°1` != "0001/Absence d'indice"),
         region = "Normandie",
         data.provider = "GMN") %>%
  rename(loc = `Nom INSEE`,
         lon.l93 = `Longitude Lambert 93`,
         lat.l93 = `Latitude Lambert 93`) %>%
  mutate(grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))%>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
