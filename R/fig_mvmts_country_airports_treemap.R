
mvts_country_apts1 <- filter(TFC_DF, substr(AIRPORT, 1, 2)==substr(Airport, 1, 2) & YEAR==Last_complete_year)  %>%
  select(FLT_TOT, AIRPORT) %>%
  group_by(AIRPORT) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  arrange(-FLT_TOT) %>% 
  ungroup()

if (nrow(mvts_country_apts1)>6) {
  
  mvts_country_apts_other=mvts_country_apts1[7:nrow(mvts_country_apts1),]
  
  mvts_country_apts=rbind(head(mvts_country_apts1, 6), 
                          cbind(AIRPORT="Other", FLT_TOT=sum(mvts_country_apts_other$FLT_TOT)))
  
} else {
  
  mvts_country_apts=mvts_country_apts1
  
}

mvts_country_apts=mvts_country_apts %>% 
  mutate(FLT_TOT=as.numeric(FLT_TOT),
         fraction=FLT_TOT/sum(FLT_TOT),
         label = paste0(AIRPORT, "\n", round(fraction*100), "%"),
         AIRPORT=factor(AIRPORT, levels=mvts_country_apts$AIRPORT))

if (nrow(mvts_country_apts)>1) {
  mvts_country_apts_fig=ggplot(mvts_country_apts, aes(area = FLT_TOT, fill = AIRPORT, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15,
                      grow = TRUE) +
    theme_factsheet() +
    theme(legend.position = "none")
} else {
  mvts_country_apts_fig = ggplot(data=mvts_country_apts, aes(x=3, y=FLT_TOT, fill=AIRPORT)) +
    geom_col() +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    geom_text(aes(x=3.4, label=label), position = position_stack(vjust = 0.5), size=60) +
    xlim(c(0.2, 3.5)) +
    theme_factsheet() +
    theme_void() +
    theme(legend.position = "none")
}

ggsave(here("media", "factsheet", paste0("Mvmts_country_treemap_", Airport, ".pdf")), plot=mvts_country_apts_fig, 
       width = Traffic_layout2[1]*Page_width, height = Traffic_height2, units = "cm", dpi=100, limitsize = FALSE)
