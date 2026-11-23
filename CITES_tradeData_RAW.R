library(tidyverse)

########################################################################
########################################################################
direc <- '../../Desktop/FullCITES_data_040725/'

readFun <- function(filename) {
  
  # read in the data
  data <- read.csv(paste0(direc, filename))
  
  # filter only Tridacnidae entries
  sub.data <- data[data$Family == 'Tridacnidae', c(2,4,9:17)]
  
  return(sub.data)
}

tridac_data <- plyr::ldply(.data = list.files(path = direc, pattern = '*.csv'), .fun = readFun)
tridac_data <- arrange(tridac_data, Year)

write.csv(tridac_data, file = 'CITES_Data/CITES_Tridacnidae_FULL.csv', row.names = F)

########################################################################
########################################################################

countryCodes <- read_csv('CITES_Data/CITES_countryCodes.csv')
cites_full <- read.csv('CITES_Data/CITES_Tridacnidae_FULL.csv') %>%
  left_join(countryCodes, by = c('Origin' = 'ISO.Code')) %>%
  rename(Origin_name = Country) %>%
  left_join(countryCodes, by = c('Exporter' = 'ISO.Code')) %>%
  rename(Exporter_name = Country) %>%
  left_join(countryCodes, by = c('Importer' = 'ISO.Code')) %>%
  rename(Importer_name = Country)

for(i in 1:nrow(cites_full)){
  if(cites_full$Unit[i] == 'g'){
    cites_full$Quantity[i] <- cites_full$Quantity[i]/1000
  }
}
cites_full$Unit[cites_full$Unit == 'g'] <- 'kg'

import_report <- filter(cites_full, Reporter.type == 'I')
export_report <- filter(cites_full, Reporter.type == 'E')

###### US Imports/Exports ######
us_imports <- filter(import_report, Importer %in% c('US', 'AS', 'GU', 'MP'))
exports_to_us <- filter(export_report, Importer %in% c('US', 'AS', 'GU', 'MP'))
us_exports <- filter(export_report, Exporter %in% c('US', 'AS', 'GU', 'MP'))
imports_from_us <- filter(import_report, Exporter %in% c('US', 'AS', 'GU', 'MP'))

#########
# MEAT
#########
all_meat <- filter(cites_full, Term == 'meat' & Importer == 'US')
all_meat$Unit[all_meat$Unit == ''] <- 'Number of specimens'

us_reported_meat_imports <- filter(us_imports, Term == 'meat') %>%
  filter(Year > 2015) %>%
  group_by(Year, Taxon, Unit, Importer, Exporter, Source, Purpose) %>%
  summarise(Quantity = sum(Quantity))
us_reported_meat_exports <- filter(us_exports, Term == 'meat') %>%
  filter(Year > 2015) %>%
  group_by(Year, Taxon, Unit, Importer, Exporter, Source, Purpose) %>%
  summarise(Quantity = sum(Quantity))
other_reported_meat_imports <- filter(imports_from_us, Term == 'meat') %>%
  filter(Year > 2015) %>%
  group_by(Year, Taxon, Unit, Importer, Exporter, Source, Purpose) %>%
  summarise(Quantity = sum(Quantity))
other_reported_meat_exports <- filter(exports_to_us, Term == 'meat') %>%
  filter(Year > 2015) %>%
  group_by(Year, Taxon, Unit, Importer, Exporter, Source, Purpose) %>%
  summarise(Quantity = sum(Quantity))

us_meat_transactions_all <- all_meat %>%
  group_by(Year, Source, Exporter_name) %>%
  summarize(trans = n())
us_meat_transactions_byUS <- us_reported_meat %>%
  group_by(Year, Source, Exporter_name) %>%
  summarize(trans = n())
us_meat_transactions_other <- other_reported_meat %>%
  group_by(Year, Source, Exporter_name) %>%
  summarize(trans = n())

us_meat_volume_byUS <- us_reported_meat %>%
  group_by(Year, Unit, Source, Exporter_name) %>%
  summarize(Quantity = sum(Quantity)) %>%
  mutate(Quantity = round(Quantity, digits = 1), Reported_by = 'I')
us_meat_volume_other <- other_reported_meat %>%
  group_by(Year, Unit, Source, Exporter_name) %>%
  summarize(Quantity = sum(Quantity)) %>%
  mutate(Quantity = round(Quantity, digits = 1), Reported_by = 'E')
us_meat_volume_all <- full_join(us_meat_volume_byUS, us_meat_volume_other) %>%
  arrange(Year)

# Marshall Islands, FSM, Palau
meat_MI_to_US <- filter(all_meat, Exporter_name == 'Marshall Islands', Year > 2015 & Year < 2020)
meat_FSM_to_US <- filter(all_meat, Exporter_name == 'Micronesia', Year > 2015 & Year < 2020)
meat_PA_to_US <- filter(all_meat, Exporter_name == 'Palau', Year > 2015 & Year < 2020)

group_by(meat_MI_to_US, Unit) %>% summarize(Quantity = sum(Quantity)/4)
group_by(meat_FSM_to_US, Unit) %>% summarize(Quantity = sum(Quantity)/4)
group_by(meat_PA_to_US, Unit) %>% summarize(Quantity = sum(Quantity)/4)

rbind(meat_MI_to_US, meat_FSM_to_US) %>%
  group_by(Unit) %>%
  summarize(Quantity = sum(Quantity)/4)

MI_meat_transactions <- filter(us_meat_transactions_byUS, Exporter_name == 'Marshall Islands', Year > 2015)
FSM_meat_transactions <- filter(us_meat_transactions_byUS, Exporter_name == 'Micronesia', Year > 2015)
PAL_meat_transactions <- filter(us_meat_transactions_other, Exporter_name == 'Palau', Year > 2015)

####
# Munro (1989) estimated that the flesh weight of giant clams is ~16.5% of total weight
# Estimated weight of meat for average sizes of T.gigas and T.derasa (n = 10,000) provided by Munro (1988):
# T.gigas:
# Age (yr)    Size (cm)     Total weight (kg)     Meat weight (kg)
# 4           26.63         3.95                  0.65
# 8           44.93         20.55                 3.39
# 12          56.96         43.38                 7.16
# T.derasa:
# 4           17.06         0.79                  0.13
# 8           27.53         4.37                  0.72
# 12          34.32         9.61                  1.59
####

#########
# SHELLS
#########
USshellImports_all <- filter(cites_full, Importer %in% c('US', 'AS', 'GU', 'MP') & Term %in% c('shells','carvings','jewellery')) %>%
  mutate(Unit = factor(ifelse(Unit %in% c('Number of specimens', 'sets', 'pairs') | Unit == '', 'num', Unit), levels = c('num','kg')),
         Origin = ifelse(is.na(Origin_name) | Origin_name == 'Unknown', Exporter_name, Origin_name),
         Source = factor(ifelse(Source == '' | Source == 'U', 'U', 
                                ifelse(Source == 'C' | Source == 'F', 'C', Source)), levels = c('W','C','I','O','U'))) %>%
  group_by(across(-c(Purpose, Quantity, Exporter, Importer, Exporter_name, Origin_name))) %>%
  summarize(Quantity = sum(Quantity))

# Pivot wide by Year
USshellImports_wide <- USshellImports_all %>%
  pivot_wider(names_from = Year, values_from = Quantity) %>%
  arrange(Taxon, Term, Origin, Source, Reporter.type, Unit)

# Merging Importer- and Exporter-reported values in the same rows
year_cols <- colnames(USshellImports_wide)[8:47]
USshellImports_importers <- ungroup(USshellImports_wide) %>% 
  filter(Reporter.type == "I") %>% 
  select(-Reporter.type) %>% 
  rename_with(~ paste0(.x, "_I"), year_cols)
USshellImports_exporters <- ungroup(USshellImports_wide) %>% 
  filter(Reporter.type == "E") %>% 
  select(-Reporter.type) %>% 
  rename_with(~ paste0(.x, "_E"), year_cols)
merged <- full_join(USshellImports_importers, USshellImports_exporters, by = c("Taxon", "Term", "Unit", "Origin", "Source", "Importer_name"))
for (year in year_cols) {
  i_col <- paste0(year, "_I")
  e_col <- paste0(year, "_E")
  merged[[year]] <- case_when(
    !is.na(merged[[i_col]]) & !is.na(merged[[e_col]]) ~ paste0(scales::comma(merged[[i_col]]), " (", scales::comma(merged[[e_col]]), ")"),
    !is.na(merged[[i_col]]) ~ scales::comma(merged[[i_col]]),
    !is.na(merged[[e_col]]) ~ paste0("(", scales::comma(merged[[e_col]]), ")"),
    TRUE ~ NA_character_
  )
}
USshellImports_wide_merged <- merged %>% 
  select(c("Taxon", "Term", "Unit", "Origin", "Source", "Importer_name", year_cols)) %>%
  arrange(Taxon, Term, Importer_name, Origin, Source, Unit)
write_csv(USshellImports_wide_merged, file = 'CITES_Data/newAnalysis/Shells-Carvings_USImports.csv', na = '')


carvings <- filter(USshellImports_all, Term != 'shells') %>%
  group_by(across(-c(Quantity, Term, Purpose))) %>%
  summarize(Quantity = sum(Quantity)) %>%
  mutate(Origin = ifelse(is.na(Origin_name) | Origin_name == 'Unknown', Exporter_name, Origin_name)) %>%
  arrange(Taxon, Origin, Year)
carvings.byWt <- filter(carvings, Unit == 'kg')
carvings.byNo <- filter(carvings, Unit %in% c('Number of specimens', 'sets', 'pairs'))
carvings.unk <- filter(carvings, Unit == '')

shells <- filter(USshellImports_all, Term == 'shells') %>%
  group_by(across(-c(Quantity, Term, Purpose))) %>%
  summarize(Quantity = sum(Quantity)) %>%
  mutate(Origin = ifelse(is.na(Origin_name) | Origin_name == 'Unknown', Exporter_name, Origin_name)) %>%
  arrange(Taxon, Origin, Year)
shells.byWt <- filter(shells, Unit == 'kg')
shells.byNo <- filter(shells, Unit %in% c('Number of specimens', 'sets', 'pairs'))
shells.unk <- filter(shells, Unit == '')


########################################################################
########################################################################
# USFWS-verified (LEMIS) data, all species (2016-2020)

countryCodes <- read_csv('CITES_Data/CITES_countryCodes.csv')
lemis <- read_csv('CITES_Data/US-CITES-Trade_ALL_2016-2020.csv') %>%
  mutate(dt_transaction = as.Date(as.character(dt_transaction), format = '%Y%m%d')) %>%
  mutate(yr_transaction = format(dt_transaction, '%Y')) %>%
  left_join(countryCodes, by = c('country_origin' = 'ISO.Code')) %>%
  rename(Origin_name = Country) %>%
  left_join(countryCodes, by = c('country_export' = 'ISO.Code')) %>%
  rename(Exporter_name = Country) %>%
  left_join(countryCodes, by = c('country_import' = 'ISO.Code')) %>%
  rename(Importer_name = Country)

for(i in 1:nrow(lemis)){
  if(lemis$unit_cd[i] == 'GM'){
    lemis$quantity[i] <- lemis$quantity[i]/1000
  }
  if(lemis$unit_cd[i] == 'LB'){
    lemis$quantity[i] <- lemis$quantity[i]/2.205
  }
}
lemis$unit_cd[lemis$unit_cd == 'GM'] <- 'KG'
lemis$unit_cd[lemis$unit_cd == 'LB'] <- 'KG'

# DERIVATIVE PRODUCTS
## MEAT
lemis_meat <- filter(lemis, description == 'MEA')
table(lemis_meat$status_cd)
lemis_meat_imports <- filter(lemis_meat, ie_cd == 'I' & status_cd != 'CL') %>%
  mutate(mon_transaction = as.Date(paste0(format(dt_transaction, "%Y-%m"), '-01'), format = "%Y-%m-%d"),
         Origin_name = factor(Origin_name, levels = c('Samoa','Kiribati','French Polynesia','Tonga','Micronesia','Marshall Islands'))) %>%
  group_by(mon_transaction, unit_cd, Origin_name) %>%
  summarise(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(Year = as.integer(substr(as.character(mon_transaction), 1, 4)))

meat_import_plot_kg <- ggplot(filter(lemis_meat_imports, unit_cd == 'KG'), aes(x = mon_transaction, y = quantity))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Seizures of Giant Clam Meat', subtitle = 'Monthly, 2016-2020')+
  geom_col(aes(fill = Origin_name), position = 'stack')+
  scale_y_continuous(name = 'Quantity (kg)', expand = expansion(mult = c(0,0.05)))+
  scale_fill_manual(name = 'Origin', values = c("#FB9A99","#E31A1C","#B2DF8A","#33A02C","#A6CEE3","#1F78B4"))
meat_import_plot_kg

cites_meat_imports <- filter(us_imports, Term == 'meat' & Year > 2020 & Source == 'I') %>%
#table(meat_imports_cites$Source)
#table(meat_imports_cites$Taxon)
  group_by(Year, Unit, Exporter_name) %>%
  summarise(quantity = sum(Quantity)) %>%
  ungroup() %>%
  rename(Origin_name = Exporter_name) %>%
  mutate(dataset = 'CITES') %>%
  filter(Unit == 'kg') %>% #2022: 1,695 specimens from Marshall Islands, 27 specimens from Fiji
  select(-Unit)

lemis_cites_meat_imports <- lemis_meat_imports %>%
  filter(unit_cd == 'KG') %>%
  select(-unit_cd) %>%
  mutate(dataset = 'LEMIS') %>%
  bind_rows(cites_meat_imports) %>%
  mutate(Origin_name = factor(Origin_name, levels = c('Samoa','Kiribati','Palau','French Polynesia','Tonga','Fiji','Micronesia','Marshall Islands')))

meat_import_plot_kg_combined <- ggplot(lemis_cites_meat_imports, aes(x = Year, y = quantity))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Seizures of Giant Clam Meat', subtitle = 'Annual, 2016-2022')+
  geom_col(aes(fill = Origin_name), position = 'stack')+
  scale_x_continuous(breaks = c(2016:2022))+
  scale_y_continuous(name = 'Quantity (kg)', expand = expansion(mult = c(0,0.05)))+
  scale_fill_manual(name = 'Origin', values = c("#FDBF6F","#FF7F00","#FB9A99","#E31A1C","#B2DF8A","#33A02C","#A6CEE3","#1F78B4"),
                    guide = guide_legend(reverse = TRUE))
meat_import_plot_kg_combined

meat_summ <- group_by(lemis_meat, yr_transaction, Origin_name, unit_cd, purpose_cd, source_cd) %>%
  summarize(quantity = sum(quantity), dollar_value = sum(dollar_value), num_transactions = n())
meat_transactions_MH_FM <- filter(lemis_meat, Origin_name %in% c('Marshall Islands', 'Micronesia')) %>%
  group_by(yr_transaction, unit_cd) %>%
  summarize(num_transactions = n(), quantity = sum(quantity))

## CARVINGS
lemis_carvings <- filter(lemis, description %in% c('SPR','JWL','CAR'))
table(lemis_carvings$status_cd)
lemis_carvings_imports <- filter(lemis_carvings, ie_cd == 'I' & status_cd != 'CL') %>%
  mutate(mon_transaction = as.Date(paste0(format(dt_transaction, "%Y-%m"), '-01'), format = "%Y-%m-%d")) %>%
  group_by(mon_transaction, unit_cd, Exporter_name) %>%
  summarise(quantity = sum(quantity)) %>%
  ungroup() %>%
  mutate(Year = as.integer(substr(as.character(mon_transaction), 1, 4)))

carvings_import_plot <- ggplot(lemis_carvings_imports, aes(x = mon_transaction, y = quantity))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Seizures of Giant Clam Shell Carvings', subtitle = 'Monthly, 2016-2020')+
  geom_col(aes(fill = Exporter_name), position = 'stack')+
  scale_y_continuous(name = 'Quantity (#)', expand = expansion(mult = c(0,0.05)))
carvings_import_plot

cites_carvings_imports <- filter(us_imports, Term %in% c('shells', 'carvings','jewellery') & Year > 2020 & Source == 'I') %>%
  #table(cites_carvings_imports$Source)
  #table(cites_carvings_imports$Taxon)
  group_by(Year, Unit, Exporter_name) %>%
  summarise(quantity = sum(Quantity)) %>%
  ungroup() %>%
  mutate(dataset = 'CITES') %>%
  filter(Unit != 'kg') %>% #2021: 0.524 kg from China, 0.709 kg from Marshall Islands; 2022: 3.72 kg from Marshall Islands
  select(-Unit)

lemis_cites_carvings_imports <- lemis_carvings_imports %>%
  filter(unit_cd == 'NO') %>%
  select(-unit_cd) %>%
  mutate(dataset = 'LEMIS') %>%
  bind_rows(cites_carvings_imports)

carvings_exporters <- lemis_cites_carvings_imports %>%
  group_by(Exporter_name) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange(desc(quantity))
top_carvings_exporters <- carvings_exporters$Exporter_name[1:8]

lemis_cites_carvings_imports_2 <- lemis_cites_carvings_imports %>%
  mutate(Exporter_name = ifelse(Exporter_name %in% top_carvings_exporters, Exporter_name, 'Other')) %>%
  mutate(Exporter_name = factor(Exporter_name, levels = c('Other', rev(top_carvings_exporters))))

carvings_import_plot_kg_combined <- ggplot(lemis_cites_carvings_imports_2, aes(x = Year, y = quantity))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Seizures of Giant Clam Shell Carvings', subtitle = 'Annual, 2016-2022')+
  geom_col(aes(fill = Exporter_name), position = 'stack')+
  scale_x_continuous(breaks = c(2016:2022))+
  scale_y_continuous(name = 'Quantity (#)', expand = expansion(mult = c(0,0.05)))+
  scale_fill_manual(name = 'Exporter', 
                    values = c("grey","#FDBF6F","#FF7F00","#FB9A99","#E31A1C","#B2DF8A","#33A02C","#A6CEE3","#1F78B4"),
                    guide = guide_legend(reverse = TRUE))
carvings_import_plot_kg_combined

carvings_summ <- group_by(shell_carvings, yr_transaction, Origin_name, species_cd, unit_cd, purpose_cd, source_cd) %>%
  summarize(quantity = sum(quantity), dollar_value = sum(dollar_value), num_transactions = n())
carvings_transactions <- shell_carvings %>%
  group_by(yr_transaction) %>%
  summarize(num_transactions = n(), quantity = sum(quantity))


# LIVE SPECIMENS
live <- filter(lemis, description %in% c('LIV','SPE'))
us_live_imports_bySpecies <- filter(live, ie_cd == 'I') %>%
  group_by(species_cd) %>%
  summarise(quantity = sum(quantity), dollar_value = sum(dollar_value)) %>%
  mutate(proportion = format(round(quantity/sum(quantity) * 100, digits = 2), scientific = FALSE))
us_live_imports_byOrigin <- filter(live, ie_cd == 'I') %>%
  group_by(source_cd) %>%
  summarise(quantity = sum(quantity)) %>%
  mutate(proportion = format(round(quantity/sum(quantity) * 100, digits = 2), scientific = FALSE))
us_live_imports_byExporter <- filter(live, ie_cd == 'I') %>%
  group_by(Exporter_name) %>%
  summarise(quantity = sum(quantity)) %>%
  mutate(proportion = format(round(quantity/sum(quantity) * 100, digits = 2), scientific = FALSE))


########################################################################
########################################################################
library(tidyverse)
# Updated LEMIS data (2016-2025)
countryCodes <- read_csv('CITES_Data/CITES_countryCodes.csv')
lemis2 <- read_csv('CITES_Data/DecDetail_NOAA_GiantClams_DiPrizio_20250611_pivotincluded.csv') %>%
  mutate(ie_date = as.Date(`I/E Date`, format = '%m/%d/%Y')) %>%
  mutate(yr_transaction = format(ie_date, '%Y')) %>%
  left_join(countryCodes, by = c('Ctry Org' = 'ISO.Code')) %>%
  rename(Origin_name = Country) %>%
  left_join(countryCodes, by = c('Ctry IE' = 'ISO.Code')) %>%
  rename(IE_name = Country)

for(i in 1:nrow(lemis2)){
  if(lemis2$Unit[i] == 'GM'){
    lemis2$Qty[i] <- lemis2$Qty[i]/1000
  }
  if(lemis2$Unit[i] == 'LB'){
    lemis2$Qty[i] <- lemis2$Qty[i]/2.205
  }
}
lemis2$Unit[lemis2$Unit == 'GM'] <- 'KG'
lemis2$Unit[lemis2$Unit == 'LB'] <- 'KG'

# DERIVATIVE PRODUCTS
## MEAT
lemis2_meat <- filter(lemis2, `Wildlf Desc` == 'MEA')
table(lemis2_meat$Act)
lemis2_meat_imports <- filter(lemis2_meat, IE == 'I' & Act != 'C') %>%
  mutate(import_mon = as.Date(paste0(format(ie_date, "%Y-%m"), '-01'), format = "%Y-%m-%d"),
         IE_name = ifelse(IE_name %in% c('Kiribati','Malaysia','Mongolia','Samoa','Togo','Turkey'), 'Other', IE_name)) %>%
  mutate(IE_name = factor(IE_name, levels = rev(c('Marshall Islands','Micronesia','Fiji','French Polynesia','Tonga','Palau','Other')))) %>%
  group_by(import_mon, Unit, IE_name) %>%
  summarise(Qty = sum(Qty)) %>%
  ungroup() %>%
  mutate(Year = as.integer(substr(as.character(import_mon), 1, 4)))

trans_mode <- group_by(lemis2_meat, `Trans Mode`) %>%
  summarise(n_shipments = n())

# Seizures reported by weight (top) and number (bottom)
meat_import_plot2 <- ggplot()+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Seizures of Giant Clam Meat', subtitle = 'Annual, 2016-2025')+
  geom_col(data = filter(lemis2_meat_imports, Unit == 'KG'), aes(x = Year, y = Qty, fill = IE_name), position = 'stack')+
  geom_col(data = filter(lemis2_meat_imports, Unit == 'NO'), aes(x = Year, y = Qty*-0.5, fill = IE_name), position = 'stack')+
  geom_hline(yintercept = 0)+
  scale_x_continuous(breaks = seq(2016, 2025))+
  scale_y_continuous(name = 'Quantity', limits = c(-4250, 2250))+
  scale_fill_manual(name = 'Exporter', values = c("grey","#FB9A99","#E31A1C","#B2DF8A","#33A02C","#A6CEE3","#1F78B4"),
                    guide = guide_legend(reverse = TRUE))
meat_import_plot2
cowplot::save_plot('plotLEMIS_meat_SZ_2016-2025.png', meat_import_plot2, base_height = 5, base_width = 6)

# Which port is seeing the most giant clam meat from Marshall Islands?
lemis2_meat_ports <- filter(lemis2_meat, IE == 'I') %>%
  mutate(import_yr = format(ie_date, "%Y"),
         IE_name = ifelse(IE_name %in% c('Kiribati','Malaysia','Mongolia','Samoa','Togo','Turkey'), 'Other', IE_name)) %>%
  group_by(Unit, `Pt Cd`) %>%
  summarise(Qty = sum(Qty)) %>%
  ungroup()
# Honolulu: 87.9% reported by weight, 99.9% reported by number (~96% total assuming 1 clam ~ 0.5 kg meat)
# San Francisco: 11.2% reported by weight

## CARVINGS
lemis2_carvings <- filter(lemis2, `Wildlf Desc` %in% c('SPR','JWL','CAR'))
table(lemis2_carvings$Act)
lemis2_carvings_imports <- filter(lemis2_carvings, IE == 'I') %>%
  mutate(import_mon = as.Date(paste0(format(ie_date, "%Y-%m"), '-01'), format = "%Y-%m-%d"),
         IE_name = ifelse(IE_name %in% c('China','Palau','Canada','Viet Nam','Marshall Islands','Mexico','Hong Kong'), IE_name, 'Other')) %>%
  mutate(Year = as.integer(substr(as.character(import_mon), 1, 4)),
         IE_name = factor(IE_name, levels = rev(c('China','Palau','Canada','Viet Nam','Marshall Islands','Mexico','Hong Kong','Other')))) %>%
  group_by(Year, Unit, IE_name) %>%
  summarise(Qty = sum(Qty)) %>%
  ungroup()

trans_mode <- group_by(lemis2_carvings, `Trans Mode`) %>%
  summarise(n_shipments = n())

carvings_import_plot2 <- ggplot(lemis2_carvings_imports, aes(x = Year, y = Qty))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        plot.subtitle = element_text(face = 'italic', color = 'grey50'))+
  ggtitle('U.S. Imports of Giant Clam Shell Carvings', subtitle = 'Annual, 2016-2025')+
  geom_col(aes(fill = IE_name), position = 'stack')+
  scale_x_continuous(breaks = seq(2016, 2025))+
  scale_y_continuous(name = 'Quantity (#)', expand = expansion(mult = c(0,0.05)))+
  scale_fill_manual(name = 'Exporter', values = c("grey","#FF7F00","#FB9A99","#E31A1C","#B2DF8A","#33A02C","#A6CEE3","#1F78B4"),
                    guide = guide_legend(reverse = TRUE))
carvings_import_plot2











  