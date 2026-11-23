library(tidyverse)
library(scales)

countryCodes <- read_csv('CITES_Data/CITES_countryCodes.csv')
cites_trade <- read_csv('CITES_Data/CITES_Tridacnidae.csv') %>%
  mutate(two_sided = !is.na(`Importer reported quantity`) & !is.na(`Exporter reported quantity`),
         Quantity_import = NA,
         Quantity_export = NA,
         Origin = ifelse(!is.na(Origin), Origin, Exporter)) %>%
  left_join(countryCodes, by = c('Origin' = 'ISO.Code')) %>%
  rename(Origin_name = Country) %>%
  left_join(countryCodes, by = c('Exporter' = 'ISO.Code')) %>%
  rename(Exporter_name = Country) %>%
  left_join(countryCodes, by = c('Importer' = 'ISO.Code')) %>%
  rename(Importer_name = Country) %>%
  mutate(spp_code = ifelse(Taxon == 'Tridacna gigas', 'tg', 
                           ifelse(Taxon == 'Tridacna derasa', 'td', 
                                  ifelse(Taxon == 'Tridacna squamosa', 'ts', 
                                         ifelse(Taxon == 'Tridacna tevoroa', 'tmb',
                                                ifelse(Taxon == 'Tridacna squamosina', 'tsi',
                                                       ifelse(Taxon == 'Hippopus hippopus', 'hh',
                                                              ifelse(Taxon == 'Hippopus porcellanus', 'hp',
                                                                     ifelse(Taxon == 'Hippopus spp.', 'hipp',
                                                                            ifelse(Taxon %in% c('Tridacna spp.', 'Tridacnidae spp.'), 'unknown', 'other')))))))))) %>%
#  filter(spp_code != 'other') %>%
  mutate(Source = ifelse(Source == 'U', NA, Source))

for(i in 1:nrow(cites_trade)){
  if(cites_trade$two_sided[i]){
    cites_trade$Quantity_import[i] <- cites_trade$`Importer reported quantity`[i]
    cites_trade$Quantity_export[i] <- cites_trade$`Exporter reported quantity`[i]
  } else{
    cites_trade$Quantity_import[i] <- sum(cites_trade$`Importer reported quantity`[i], cites_trade$`Exporter reported quantity`[i], na.rm = T)
    cites_trade$Quantity_export[i] <- sum(cites_trade$`Importer reported quantity`[i], cites_trade$`Exporter reported quantity`[i], na.rm = T)
  }
}
cites_trade$Origin_name[cites_trade$Origin_name == 'Viet Nam'] <- 'Vietnam'

# Peek at different TERM entries
unique(cites_trade$Term)
table(cites_trade$Term)

# Meat-related transactions per year grouped by country of origin and species
meat.byWeight <- filter(cites_trade, Term == 'meat' & Unit %in% c('kg', 'g'))
for(i in 1:nrow(meat.byWeight)){
  if(meat.byWeight$Unit[i] == 'g'){
    meat.byWeight$Quantity_export[i] <- meat.byWeight$Quantity_export[i]/1000
    meat.byWeight$Quantity_import[i] <- meat.byWeight$Quantity_import[i]/1000
  }
}
meat.rank.byWeight <- meat.byWeight %>%
  group_by(Origin_name) %>%
  summarize(total_import = round(sum(Quantity_import), 0), 
            total_export = round(sum(Quantity_export), 0)) %>%
  arrange(desc(total_import))

####
meat.since2010 <- filter(cites_trade, Term == 'meat' & spp_code == 'ts') %>%
  group_by(Unit, Term, Source, Year, Origin_name, Importer_name, spp_code) %>%
  summarize(import = sum(Quantity_import)) %>%
  ungroup()
####

top10.meat.byWeight <- head(meat.rank.byWeight$Origin_name, 10)
top10.meat.byWeight.summ <- filter(meat.byWeight, Origin_name %in% top10.meat.byWeight) %>%
  group_by(Origin_name, Year, spp_code, Source) %>%
  summarize(annual_import = round(sum(Quantity_import), 0), 
            annual_export = round(sum(Quantity_export), 0)) %>%
  left_join(meat.rank.byWeight[,1:2], by = 'Origin_name')
top10.meat.byWeight.summ$new_name <- factor(paste0(as.character(top10.meat.byWeight.summ$Origin_name), '\n(Total: ', as.character(top10.meat.byWeight.summ$total_import), ' kg)'))
top10.meat.byWeight.summ$new_name <- factor(top10.meat.byWeight.summ$new_name, levels = levels(top10.meat.byWeight.summ$new_name)[c(10,7,8,9,1,2,3,6,4,5)])

spp.labs <- c('T. derasa', 'T. gigas', 'T. squamosa', 'Tridacninae spp.')
names(spp.labs) <- c('td', 'tg', 'ts', 'unknown')

plot.meat.weight <- ggplot(top10.meat.byWeight.summ, aes(x = Year, y = annual_import, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (kg / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1982)+
  geom_col(position = 'stack')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(4), labels = label_comma())+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Born in captivity (F1, F2, ...)',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Wild-caught'), 
                    values = c(RColorBrewer::brewer.pal(4,'Set1'), '#FF9933'), na.value = 'grey80')+
  facet_grid(new_name~spp_code, labeller = labeller(spp_code = spp.labs), scales = 'free_y')

plot.meat.weight
cowplot::save_plot('plotMeat_Importer.png', plot.meat.weight, base_height = 10, base_width = 8)


seized <- filter(cites_trade, Term == 'meat' & Unit == 'kg' & Source == 'I')
table(seized$Importer_name)
all.meat.imports <- filter(cites_trade, Term == 'meat' & Unit == 'kg') %>%
  mutate(transaction = paste(Exporter_name, Importer_name, sep = '-'))
table(all.meat.imports$Importer_name)
table(all.meat.imports$transaction)

main.meat.exporters <- filter(all.meat, Origin_name %in% c('Cook Islands', 'Kiribati', 'Marshall Islands', 'Micronesia', 'Tonga')) %>%
  filter(Year >= 2005) %>%
  group_by(Origin_name, Year) %>%
  summarise(all_import = sum(total_import)) %>%
  ungroup()
expand.df <- add_row(main.meat.exporters, Year = 2006, Origin_name = 'Cook Islands') %>%
  expand(Origin_name, Year)
main.meat.exporters.2005 <- expand.df %>%
  left_join(main.meat.exporters) %>%
  mutate(all_import = replace_na(all_import, 0)) %>%
  group_by(Origin_name) %>%
  summarise(mean_exp = mean(all_import), median_exp = median(all_import))

filter(cites_trade, Origin_name == 'Cook Islands')


# Shell-related transactions per year grouped by country of origin and species
all.shells <- filter(cites_trade, Term %in% c('shells', 'carvings','jewellery'))

unique(all.shells$Unit)

table(all.shells$Term)

summ.shells.all.imports <- all.shells %>%
  group_by(Importer_name, Term, Unit) %>%
  summarize(import = sum(Quantity_import)) %>%
  ungroup() %>%
  group_by(Term, Unit) %>%
  mutate(total_import = sum(import)) %>%
  mutate(pct_import = round(100*import/total_import, 1)) %>%
  arrange(desc(pct_import), .by_group = T) %>%
  mutate(pct_import_format = paste0(pct_import, '%')) %>%
  ungroup() %>%
  mutate(import_quant_perc = paste0(import, " (", pct_import_format, ")"),
         term_group = paste0(stringr::str_to_title(Term), " (", Unit, ")")) %>%
  select(Importer_name, term_group, import_quant_perc) %>%
  spread(term_group, import_quant_perc)
write_csv(summ.shells.all.imports, file = 'shellImporters.csv')

summ.shells.all.exports <- all.shells %>%
  group_by(Origin_name, Term, Unit) %>%
  summarize(export = sum(Quantity_import)) %>%
  ungroup() %>%
  group_by(Term, Unit) %>%
  mutate(total_export = sum(export)) %>%
  mutate(pct_export = round(100*export/total_export, 1)) %>%
  arrange(desc(pct_export), .by_group = T) %>%
  mutate(pct_export_format = paste0(pct_export, '%')) %>%
  ungroup()
write_csv(filter(summ.shells.all.exports, Origin_name == 'Philippines'), file = 'shellExports_Philippines.csv')

summ.shells <- all.shells %>%
  group_by(Importer_name, spp_code, Term, Unit) %>%
  summarize(import = sum(Quantity_import)) %>%
  ungroup() %>%
  group_by(spp_code, Term, Unit) %>%
  mutate(total_import = sum(import)) %>%
  mutate(pct_import = round(100*import/total_import, 1)) %>%
  arrange(desc(pct_import), .by_group = T) %>%
  mutate(pct_import_format = paste0(pct_import, '%')) %>%
  ungroup()

recentTrade <- filter(all.shells, Year > 2015) %>%
  group_by(Importer_name, Term, Unit, Source) %>%
  summarize(import = sum(Quantity_import)) %>%
  filter(Term == 'shells' & is.na(Unit) & Importer_name == 'China')

shells.hh <- filter(summ.shells, spp_code == 'hh' & pct_import >= 5)
shells.hp <- filter(summ.shells, spp_code == 'hp' & pct_import >= 5)
shells.tg <- filter(summ.shells, spp_code == 'tg' & pct_import >= 5)
shells.td <- filter(summ.shells, spp_code == 'td' & pct_import >= 5)
shells.ts <- filter(summ.shells, spp_code == 'ts' & pct_import >= 5)
shells.trid <- filter(summ.shells, spp_code == 'unknown' & pct_import >= 5)
shells.hipp <- filter(summ.shells, spp_code == 'hipp' & pct_import >= 5)
  

summ.shells.list <- summ.shells %>%
  split(f = as.factor(.$spp_code))
write_csv(summ.shells.list$hh, file = 'shellImports_HH.csv')

# Top 10 exporters by number of shells
top10 <- tail(sort(table(all.shells$Origin_name)), 11)[-10]
top10.df <- filter(all.shells, Origin_name %in% names(top10))

#~~~ Number of transactions
top10.n <- top10.df %>%
  group_by(Year, spp_code, Origin_name, Source) %>%
  summarize(n = n())

spp.labs2 <- c('H. hippopus', 'Hippopus spp.', 'H. porcellanus', 'T. derasa', 'T. gigas', 'T.mbalavuana', 'T. squamosa', 'Tridacninae spp.')
names(spp.labs2) <- c('hh', 'hipp', 'hp', 'td', 'tg', 'tmb', 'ts', 'unknown')
plot.shells.n <- ggplot(top10.n, aes(x = Year, y = n, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (# transactions / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1982)+
  geom_col(position = 'stack')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(3))+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Born in captivity (F1, F2, ...)',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Ranched',
                               'Wild-caught'), 
                    values = c(RColorBrewer::brewer.pal(4,'Set1'), '#000000', '#FF9933'), na.value = 'grey80')+
  facet_grid(Origin_name~spp_code, labeller = labeller(spp_code = spp.labs2), scales = 'free_y')

plot.shells.n
cowplot::save_plot('plotShells_byN.png', plot.shells.n, base_height = 8, base_width = 14)

#~~~ Count of shells
shells.byCount <- filter(all.shells, Term == 'shells' & (Unit %in% c('sets', 'pairs', 'Number of specimens') | is.na(Unit)))
rank.byCount <- shells.byCount %>%
  group_by(Origin_name) %>%
  summarize(total_import = round(sum(Quantity_import), 0), 
            total_export = round(sum(Quantity_export), 0)) %>%
  arrange(desc(total_import))
top5.byCount <- head(rank.byCount$Origin_name, 5)
top5.byCount.summ <- filter(shells.byCount, Origin_name %in% top5.byCount) %>%
  group_by(Origin_name, Year, spp_code, Source) %>%
  summarize(annual_import = round(sum(Quantity_import), 0), 
            annual_export = round(sum(Quantity_export), 0))
top5.byCount.summ.noTmb <- filter(top5.byCount.summ, spp_code != 'tmb') %>%
  left_join(rank.byCount[,1:2], by = 'Origin_name')
top5.byCount.summ.noTmb$new_name <- factor(paste0(as.character(top5.byCount.summ.noTmb$Origin_name), '\n(Total: ', as.character(top5.byCount.summ.noTmb$total_import), ')'))
top5.byCount.summ.noTmb$new_name <- factor(top5.byCount.summ.noTmb$new_name, levels = levels(top5.byCount.summ.noTmb$new_name)[c(4,2,3,1,5)])

spp.labs3 <- c('H. hippopus', 'Hippopus spp.', 'H. porcellanus', 'T. derasa', 'T. gigas', 'T. squamosa', 'Tridacninae spp.')
names(spp.labs3) <- c('hh', 'hipp', 'hp', 'td', 'tg', 'ts', 'unknown')
plot.shells.count <- ggplot(top5.byCount.summ.noTmb, aes(x = Year, y = annual_import, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (# clams / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1982)+
  # geom_vline(data = data.frame(xint = 1996, Origin_name = 'Philippines'), aes(xintercept = xint), linetype = 'dashed')+
  geom_col(position = 'stack')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(4), labels = label_comma())+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Born in captivity (F1, F2, ...)',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Ranched',
                               'Wild-caught'),
                    values = c(RColorBrewer::brewer.pal(4,'Set1'), '#000000', '#FF9933'), na.value = 'grey80')+
  facet_grid(new_name~spp_code, labeller = labeller(spp_code = spp.labs3), scales = 'free_y')

plot.shells.count
cowplot::save_plot('plotShells_byCount_Importer.png', plot.shells.count, base_height = 6, base_width = 11)

#~~~ Weight of shells
shells.byWeight <- filter(all.shells, Term == 'shells' & (Unit %in% c('kg', 'g')))
for(i in 1:nrow(shells.byWeight)){
  if(shells.byWeight$Unit[i] == 'g'){
    shells.byWeight$Quantity_export[i] <- shells.byWeight$Quantity_export[i]/1000
    shells.byWeight$Quantity_import[i] <- shells.byWeight$Quantity_import[i]/1000
  }
}
rank.byWeight <- shells.byWeight %>%
  group_by(Origin_name) %>%
  summarize(total_import = round(sum(Quantity_import), 0), 
            total_export = round(sum(Quantity_export), 0)) %>%
  arrange(desc(total_import))
top5.byWeight <- head(rank.byWeight$Origin_name, 5)
top5.byWeight.summ <- filter(shells.byWeight, Origin_name %in% top5.byWeight) %>%
  group_by(Origin_name, Year, spp_code, Source) %>%
  summarize(annual_import = round(sum(Quantity_import), 0), 
            annual_export = round(sum(Quantity_export), 0)) %>%
  left_join(rank.byWeight[,1:2], by = 'Origin_name')
top5.byWeight.summ$new_name <- factor(paste0(as.character(top5.byWeight.summ$Origin_name), '\n(Total: ', as.character(top5.byWeight.summ$total_import), ' kg)'))
top5.byWeight.summ$new_name <- factor(top5.byWeight.summ$new_name, levels = levels(top5.byWeight.summ$new_name)[c(3,5,2,1,4)])


plot.shells.weight <- ggplot(top5.byWeight.summ, aes(x = Year, y = annual_import, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (kg / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1982)+
  # geom_vline(data = data.frame(xint = 1992, Origin_name = 'Philippines'), aes(xintercept = xint), linetype = 'dashed')+
  geom_col(position = 'stack')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(4), labels = label_comma())+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Wild-caught'),
                    values = c('#E41A1C', '#4DAF4A', '#984EA3', '#FF9933'), na.value = 'grey80')+
  facet_grid(new_name~spp_code, labeller = labeller(spp_code = spp.labs3), scales = 'free_y')

plot.shells.weight
cowplot::save_plot('plotShells_byWeight_Importer.png', plot.shells.weight, base_height = 6, base_width = 11)

####
shells.since2010 <- filter(all.shells, spp_code == 'ts' & Year > 2009 & Origin_name %in% c('Fiji', 'Solomon Islands','Tonga')) %>%
  group_by(Term, Unit, Source, Origin_name) %>%
  summarize(import = sum(Quantity_import)) %>%
  ungroup()
####


#~~~~ LIVE SPECIMENS
all.live <- filter(cites_trade, Term == 'live' & Purpose != 'S' & (is.na(Unit) | Unit == 'Number of specimens'))
all.live$Origin_name[all.live$Origin_name == 'Viet Nam'] <- 'Vietnam'
rank.live <- all.live %>%
  group_by(Origin_name) %>%
  summarize(total_import = round(sum(Quantity_import), 0), 
            total_export = round(sum(Quantity_export), 0)) %>%
  arrange(desc(total_import))
top10.live <- head(rank.live$Origin_name, 10)
top10.live.summ <- filter(all.live, Origin_name %in% top10.live) %>%
  group_by(Origin_name, Year, spp_code, Source) %>%
  summarize(annual_import = round(sum(Quantity_import), 0), 
            annual_export = round(sum(Quantity_export), 0))
top10.live.summ$Source[top10.live.summ$Source == 'D'] <- 'C'
top10.live.summ <- filter(top10.live.summ, !(spp_code %in% c('hipp','hp','tmb'))) %>%
  left_join(rank.live[,1:2], by = 'Origin_name')
top10.live.summ$new_name <- factor(paste0(as.character(top10.live.summ$Origin_name), '\n(Total: ', as.character(top10.live.summ$total_import), ')'))
top10.live.summ$new_name <- factor(top10.live.summ$new_name, levels = levels(top10.live.summ$new_name)[c(1,6,10,8,4,9,5,7,2,3)])

spp.labs.live <- c('H. hippopus', 'T. derasa', 'T. gigas', 'T. squamosa', 'Tridacninae spp.')
names(spp.labs.live) <- c('hh', 'td', 'tg', 'ts', 'unknown')
plot.live <- ggplot(top10.live.summ, aes(x = Year, y = annual_import, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (# clams / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1982)+
  geom_col(position = 'stack')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(3), labels = label_comma())+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Born in captivity (F1, F2, ...)',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Ranched',
                               'Wild-caught'),
                    values = c(RColorBrewer::brewer.pal(4,'Set1'), '#000000', '#FF9933'), na.value = 'grey80')+
  facet_grid(new_name~spp_code, labeller = labeller(spp_code = spp.labs.live), scales = 'free_y')

plot.live
cowplot::save_plot('plotLive_Importer.png', plot.live, base_height = 10, base_width = 10)

all.live2 <- filter(all.live, is.na(Unit) | Unit == 'Number of specimens')
summ.live <- filter(all.live2, !(Importer_name %in% c('Samoa', 'Cook Islands'))) %>%
  group_by(Importer_name, spp_code) %>%
  summarize(import = sum(Quantity_import)) %>%
  ungroup() %>%
  group_by(spp_code) %>%
  mutate(total_import = sum(import)) %>%
  mutate(pct_import = round(100*import/total_import, 1)) %>%
  arrange(desc(pct_import), .by_group = T) %>%
  mutate(pct_import_format = paste0(pct_import, '%')) %>%
  ungroup() %>%
  mutate(import_quant_perc = paste0(import, " (", pct_import_format, ")")) %>%
  select(Importer_name, spp_code, import_quant_perc) %>%
  spread(spp_code, import_quant_perc)

write_csv(summ.live, file = 'liveImporters.csv')

select.live <- filter(all.live2, Importer_name %in% c('Samoa', 'Cook Islands', 'Viet Nam'))
select.live.us <- filter(all.live2, Importer_name == 'United States' & spp_code == 'ts' & Year > 2009) %>%
  group_by(Source, Origin_name) %>%
  summarise(import = sum(Quantity_import)) %>%
  mutate(percent = import / 490387 * 100)

recentTrade.live <- filter(all.live2, Year > 2009 & spp_code == 'ts' & Origin_name %in% c('Cambodia', 'Indonesia', 'Palau', 'Marshall Islands', 'Vietnam', 'Egypt', 'Vanuatu', 'Australia', 'Tonga', 'Micronesia')) %>%
  group_by(Origin_name, Source, Year) %>%
  summarize(import = sum(Quantity_import)) %>%
  mutate(Year = as.Date(paste(Year, 1, 1, sep = "-")))
recentTrade.live$Source[recentTrade.live$Source == 'D'] <- 'C'
recentTrade.live$Origin_name_f = factor(recentTrade.live$Origin_name, 
                                        levels = c('Cambodia', 'Indonesia', 'Palau', 'Marshall Islands', 'Vietnam', 'Egypt', 'Vanuatu', 'Australia', 'Tonga', 'Micronesia'))

plot.recent.live <- ggplot(recentTrade.live, aes(x = Year, y = import, fill = Source))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0),
        legend.position = 'bottom')+
  labs(y = 'Annual Trade (# clams / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = as.Date('2009-01-01'))+
  geom_col(position = 'stack')+
  scale_x_date(expand = c(0,0), limits = as.Date(c('2009-01-01', '2021-01-01')))+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), breaks = breaks_extended(3), labels = label_comma())+
  scale_fill_manual(labels = c('Bred in captivity',
                               'Born in captivity (F1, F2, ...)',
                               'Confiscated/seized',
                               'Pre-Convention',
                               'Wild-caught'),
                    values = c(RColorBrewer::brewer.pal(4,'Set1'), '#FF9933'), na.value = 'grey80')+
  facet_wrap(.~Origin_name_f, nrow = 2)

plot.recent.live
cowplot::save_plot('plotLive_Importer_recent.png', plot.recent.live, base_height = 10, base_width = 10)



# US Trade Data -----------------------------------------------------------

country_coords <- read_csv('Abundance_Data/globalAbundance_Neo2017.csv')[,1:4]

us_cites_trade_raw <- read_csv('CITES_Data/US-CITES-Trade_Tridacna_Hippopus_2016-2020.csv')
us_cites_trade <- us_cites_trade_raw %>%
  mutate(yr_transaction = as.integer(substr(dt_transaction, 1, 4))) %>%
  group_by(yr_transaction, species_cd, country_origin, unit_cd, description, purpose_cd, source_cd, status_cd) %>%
  summarise(quantity_yr = sum(quantity))

count.seized <- us_cites_trade %>%
  group_by(description, status_cd) %>%
  summarise(n = n(), count = sum(quantity_yr))

us.meat <- us_cites_trade %>%
  filter(description == 'MEA') %>%
  left_join(countryCodes, by = c('country_origin' = 'ISO.Code'))

for(i in 1:nrow(us.meat)) {
  if(us.meat$unit_cd[i] == 'GM') {
    us.meat$quantity_yr[i] <- us.meat$quantity_yr[i] / 1000
    us.meat$unit_cd[i] <- 'KG'
    }
  if(us.meat$unit_cd[i] == 'LB') {
    us.meat$quantity_yr[i] <- us.meat$quantity_yr[i] / 2.205
    us.meat$unit_cd[i] <- 'KG'
    }
}

us.meat.summ <- us.meat %>%
  group_by(yr_transaction, Country, purpose_cd, source_cd, status_cd, unit_cd) %>%
  summarise(total = sum(quantity_yr))

write_csv(us.meat.summ, file = 'us_meat_summary.csv', quote = 'none')

plot.us.meat <- ggplot(us.meat, aes(x = yr_transaction, y = quantity_yr, fill = source_cd))+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.text.x = element_text(face = 'italic'),
        strip.text.y = element_text(angle = 0))+
  labs(y = 'Annual Trade (kg / yr)')+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 2015)+
  geom_col(position = 'dodge')+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  facet_grid(country_origin ~ .)
plot.us.meat

meat.seized <- filter(us.meat, status_cd %in% c('RE', 'SZ')) %>%
  group_by(yr_transaction, species_cd, unit_cd) %>%
  summarize(total = sum(quantity_yr))


#~~~ SHELLS
us.shells <- us_cites_trade %>%
  filter(description == 'SHE' & unit_cd == 'NO') %>%
  left_join(countryCodes, by = c('country_origin' = 'ISO.Code')) %>%
  group_by(Country, source_cd) %>%
  summarise(total_import = sum(quantity_yr))


#~~~ LIVE
us.live <- us_cites_trade %>%
  filter(description == 'LIV' & !(species_cd %in% c('TCRO', 'TSXM', 'TSXC', 'TINA'))) %>%
  left_join(countryCodes, by = c('country_origin' = 'ISO.Code')) %>%
  group_by(Country, species_cd, source_cd) %>%
  summarise(total_import = sum(quantity_yr))

us.live.source <- us.live %>%
  group_by(source_cd) %>%
  summarise(import = sum(total_import))

us.live.plot <- ggplot(us.live, aes(x = Country, y = total_import, fill = source_cd))+
  theme_bw()+
  theme(legend.position = 'bottom')+
  labs(y = 'Total export (2016-2020')+
  # geom_hline(yintercept = 0)+
  # geom_vline(xintercept = 1982)+
  geom_col(position = 'stack')+
  scale_y_continuous(expand = expansion(mult = c(0, .1)), labels = label_comma())+
  # scale_fill_manual(labels = c('Bred in captivity',
  #                              'Born in captivity (F1, F2, ...)',
  #                              'Confiscated/seized',
  #                              'Pre-Convention',
  #                              'Ranched',
  #                              'Wild-caught'),
  #                   values = c(RColorBrewer::brewer.pal(4,'Set1'), '#000000', '#FF9933'), na.value = 'grey80')
  facet_grid(.~spp_code, labeller = labeller(spp_code = spp.labs.live))


us.live.plot
cowplot::save_plot('plotLive_Importer.png', plot.live, base_height = 10, base_width = 10)




