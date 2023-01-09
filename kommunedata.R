



install.packages("PxWebApiData")

library(PxWebApiData)
library(tidyverse)

bef <- ApiData("http://data.ssb.no/api/v0/en/table/04861", 
        ContentsCode = "Bosatte", Tid = c(1, 2, -2, -1)) 

colnames <- tolower(names(bef[[1]]))
bef <- map(bef, ~ rename_with(., ~ colnames))

str(bef[[2]])

bef <- bef %>% 
  bind_rows() %>% 
  mutate(year = as.numeric(year))
  

str(bef)

ggplot(bef, aes(x = year, y = value, group = region, col = region)) +
  geom_line()


ApiData("http://data.ssb.no/api/v0/no/table/07459",  returnMetaFrames = TRUE)


## Anmeldte lovbrudd
meta<- ApiData("http://data.ssb.no/api/v0/en/table/08487",  returnMetaFrames = TRUE)

kommuner <- meta$Gjerningssted %>% 
  filter(nchar(values) == 4 & !(values %in% c("Ialt", "0000"))) %>% 
  pull(values)

grupper <- meta$LovbruddKrim %>% 
  filter(nchar(values) > 5 & valueTexts != "All groups of offences" ) %>% 
  pull(values)

## Pull data from API
anm_list <- ApiData("http://data.ssb.no/api/v0/en/table/08487", 
                    ContentsCode = "AnmLovbrPer1000", 
                    Gjerningssted = kommuner, 
                    LovbruddKrim = grupper, 
                    Tid = TRUE,
                    makeNAstatus = FALSE)

# rename
names(anm_list[[1]]) <- c("kommune", "lovbruddsgruppe", "content", "year", "lovbrudd_per1000")


# Combine and tidy up
anm <- cbind(anm_list[[2]][1], anm_list[[1]]) %>% 
  select(-content) %>% 
  mutate(lovbruddsgruppe = str_sub(lovbruddsgruppe, 3, nchar(lovbruddsgruppe))) %>% 
  mutate(year = as.numeric(str_sub(year,1,4))) %>% 
  filter(!is.na(lovbrudd_per1000)) %>% 
  mutate(lovbruddsgruppe = case_when(str_sub(lovbruddsgruppe, 1,4) == "Prop" ~ "vinningskriminalitet", 
                                     str_sub(lovbruddsgruppe, 1,4) == "Viol" ~ "voldskriminalitet",
                                     str_sub(lovbruddsgruppe, 1,4) == "Drug" ~ "nark_alko_kriminalitet",
                                     str_sub(lovbruddsgruppe, 1,4) == "Publ" ~ "ordenslovbrudd",
                                     str_sub(lovbruddsgruppe, 1,4) == "Traf" ~ "trafikklovbrudd",
                                     str_sub(lovbruddsgruppe, 1,4) == "Othe" ~ "andre_lovbrudd")) %>% 
  pivot_wider(values_from = lovbrudd_per1000, names_from = lovbruddsgruppe) %>% 
  rename(kommune_nr = Gjerningssted) %>% 
  select(-kommune)

glimpse(anm)

#Check
ggplot( filter(anm, Gjerningssted == "0301"), 
        aes(x = year_n, y = lovbrudd_per1000, group = lovbruddsgruppe, linetype = lovbruddsgruppe))+
  geom_line()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  guides(linetype = guide_legend(ncol = 1))+
  xlab("")





# Befolkning
meta<- ApiData("http://data.ssb.no/api/v0/en/table/07459",  returnMetaFrames = TRUE)
names(meta)

meta$Tid
meta$Alder
meta$Kjonn
meta$ContentsCode
meta$Region %>% head()

kommuner_bef <- meta$Region %>% 
  filter(nchar(values) == 4 ) %>% 
  pull(values)
head(kommuner_bef)


befolk <- list(length(meta$Tid$values))
for(i in 1:length(meta$Tid$values)){ 
  #print(i)
  bef_list <- ApiData("http://data.ssb.no/api/v0/en/table/07459", 
                      ContentsCode = TRUE, 
                      Region = kommuner_bef, 
                      Kjonn = TRUE,  
                      Alder = TRUE,
                      Tid = i)
  befolk[[i]] <- cbind(bef_list[[2]][1], bef_list[[1]])
  
  #rm(bef_list)
  }


befolkning <- bind_rows(befolk)

# rename
names(befolkning) <- c("kommune_nr", "kommune", "kjonn", "alder", "contents", "year", "bef_antall")

# Combine and tidy up
befolkning2 <- befolkning %>% 
  select(-contents) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(age = str_extract(alder, "(\\d)+")) %>% 
  mutate(age_gr = case_when(age < 18 ~ "under 18", 
                            age <= 25 ~ "18-25",
                            age <= 35 ~ "26-35", 
                            age <= 67 ~ "36-67", 
                            age > 67 ~ "over 67")) 

bef_gruppe <- befolkning2 %>% 
  group_by(kommune_nr, kommune, year, age_gr, kjonn) %>% 
  summarise(n = sum(bef_antall)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = n, names_from = age_gr) %>% 
  rename(bef_18_25 = `18-25`, bef_26_35 = `26-35`, bef_36_67 = `36-67`, 
         bef_67plus = `over 67`, bef_18min = `under 18`) %>% 
  filter(year >= 2010) 
  
bef_menn <- filter(bef_gruppe, kjonn == "Males") %>% 
  rename_all(str_replace_all, "bef_", "menn_") %>% 
  select(-kjonn, -kommune)

bef_kvinner <- filter(bef_gruppe, kjonn == "Females") %>% 
  rename_all(str_replace_all, "bef_", "kvinner_") %>% 
  select(-kjonn, -kommune)

head(bef_kvinner)


bef_tot <- befolkning2 %>% 
  group_by(kommune_nr, kommune, year, age_gr) %>% 
  summarise(n = sum(bef_antall)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = n, names_from = age_gr) %>% 
  rename(bef_18_25 = `18-25`, bef_26_35 = `26-35`, bef_36_67 = `36-67`, 
         bef_67plus = `over 67`, bef_18min = `under 18`) %>% 
  filter(year >= 2010) %>% 
  rowwise() %>% 
  mutate(bef_totalt = sum( across(bef_18_25:bef_18min))) %>% 
  select(1:3, 8, 4, 5, 9)

glimpse(bef_tot)

# bef_agg <- befolkning2 %>% 
#   group_by( year, age_gr) %>% 
#   summarise(n = sum(bef_antall)) %>% 
#   ungroup()
# 
# glimpse(bef_agg)
# 
# ggplot(bef_agg, aes(x = year, y = n, linetype = age_gr, group = age_gr))+
#   geom_line()+
#   ylim(0,NA)
# 
  

## Inntekt

meta<- ApiData("http://data.ssb.no/api/v0/en/table/06944",  returnMetaFrames = TRUE)
names(meta)
meta$Tid
meta$HusholdType
meta$ContentsCode

kommuner_innt <- meta$Region %>% 
  filter(nchar(values) == 4 ) %>% 
  pull(values)
head(kommuner_innt)


innt_list <- ApiData("http://data.ssb.no/api/v0/en/table/06944", 
                    ContentsCode = TRUE, 
                    Region = kommuner_innt, 
                    HusholdType = TRUE,  
                    Tid = TRUE)

glimpse(innt_list[[1]])

inntekt <- cbind(innt_list[[2]][1], innt_list[[1]]) %>% 
  mutate(year = as.numeric(year)) %>% 
  select(-NAstatus) %>% 
  filter(!is.na(value)) %>% 
  rename(kommune = region, kommune_nr = Region,
         hushold = `type of household`) %>% 
  mutate(contents = case_when(str_sub(contents,1,5) == "Total" ~ "inntekt_totalt_median",
                             str_sub(contents,1,6) == "Income" ~ "inntekt_eskatt_median",
                             str_sub(contents,1,6) == "Number" ~ "ant_husholdninger")) %>%
  pivot_wider(values_from = value, names_from = contents)
  
glimpse(inntekt)

hh_inntekt <- filter(inntekt, hushold == "All households") %>% 
  select(-hushold, -kommune)
head(hh_inntekt)








## SOSIALHJLEP

meta<- ApiData("http://data.ssb.no/api/v0/en/table/12210",  returnMetaFrames = TRUE)
names(meta)
meta$ContentsCode
kokk <- meta$KOKkommuneregion0000 %>% 
  filter(!(values %in% c("EAK", "EAKUO")) ) %>% 
  pull(values)

glimpse(kokk)  


shj_list <- ApiData("http://data.ssb.no/api/v0/en/table/12210", 
                     ContentsCode = TRUE, 
                     KOKkommuneregion0000 = kokk, 
                     Tid = TRUE)

shj <- cbind(shj_list[[2]], shj_list[[1]][1]) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(values_from = value, names_from = ContentsCode) %>% 
  rename(kommune_nr = KOKkommuneregion0000, 
          year = Tid,
          shj_klienter = KOSsosantkliente0000 ,
          shj_unge = KOSant18240000) %>% 
  select(kommune_nr, year, shj_klienter, shj_unge) %>% 
  mutate(year = as.numeric(year))
  

glimpse(shj)



## Samle 


samlet <- left_join(bef_tot, bef_menn, by = c("kommune_nr", "year")) %>% 
  left_join(bef_kvinner, by = c("kommune_nr", "year")) %>% 
  left_join(hh_inntekt, by = c("kommune_nr", "year")) %>% 
  left_join( shj, by = c("kommune_nr", "year")) %>% 
  left_join( anm, by = c("kommune_nr", "year")) %>% 
  filter(year >= 2015) %>% 
  data.frame() %>% 
  filter(complete.cases(.)) 

glimpse(samlet)
  

saveRDS(samlet , "data/kommunedata.rds")

