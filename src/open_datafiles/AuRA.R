require(tidyverse, lubridate)

dat.filename <- "data/PNA-data/RhônesAlpes-LPOAURA&GMA/data_loutre_ORB_AURA.gpkg"

dat <- read_sf(dat.filename)

dat[, c("lon.l93", "lat.l93")] <- dat %>%
  st_coordinates()

dat <- dat %>% 
  as_data_frame() %>% 
  filter(desc_source == "[LPO] visionature") %>% 
  mutate(year = year(date),
         loc = place,
         presence = as.numeric(is_present),
         region = "Auvergne Rhône-Alpes",
         data.provider = "LPO",
         grid.cell = ifelse(lon.l93 >= 1000000,
                            paste0("E", substr(lon.l93,1,3),"N",substr(lat.l93,1,3)),
                            paste0("E0", substr(lon.l93,1,2),"N",substr(lat.l93,1,3))))

dat <- dat %>% 
  mutate(comment = toupper(comment), comment_priv = toupper(comment_priv)) %>%
  mutate(tr_len = as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\d\\d\\dM"))),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\d\\d M"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\d\\dM"))), tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\dKM")))*100, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\dKM")))*10000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d\\d KM")))*1000, tr_len),
         tr_len = ifelse(is.na(tr_len), as.numeric(gsub("\\D", "", str_extract(comment, "\\d KM")))*1000, tr_len)) %>% 
  mutate(PNA.protocole = ((grepl("IUCN", comment)|grepl("UICN", comment)|grepl("PROTOCOL", comment_priv)|grepl("PROTOCOL", comment)|grepl("PNA", comment))&
                            (!grepl("FRAPNA", comment)|!grepl("NON PROTOCOL", comment)|!grepl("NON PROTOCOL", comment_priv)|
                               !grepl("NON-PROTOCOL", comment_priv)|!grepl("NON-PROTOCOL", comment))),
         PNA.protocole = PNA.protocole|tr_len >= 300,
         PNA.protocole = ifelse(is.na(PNA.protocole), FALSE, PNA.protocole))

dat <- dat%>%
  select(data.provider, region, PNA.protocole, year, date, loc, lon.l93, lat.l93, grid.cell, presence)
