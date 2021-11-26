
mvts_country_apts1 <- filter(TOP_APTs, substr(AIRPORT, 1, 2)==substr(Airport, 1, 2) & YEAR==Last_complete_year)  %>%
  select(NBR_MVMTS, AIRPORT) %>%
  group_by(AIRPORT) %>%
  summarise(NBR_MVMTS = sum(NBR_MVMTS, na.rm = TRUE)) %>%
  arrange(-NBR_MVMTS) %>% 
  ungroup()

if (nrow(mvts_country_apts1)>6) {
  
  mvts_country_apts_other=mvts_country_apts1[7:nrow(mvts_country_apts1),]
  
  mvts_country_apts=rbind(head(mvts_country_apts1, 6), 
                          cbind(AIRPORT="Other", NBR_MVMTS=sum(mvts_country_apts_other$NBR_MVMTS)))
  
} else {
  
  mvts_country_apts=mvts_country_apts1
  
}

mvts_country_apts=mvts_country_apts%>% 
  mutate(NBR_MVMTS=as.numeric(NBR_MVMTS),
         fraction=NBR_MVMTS/sum(NBR_MVMTS),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin)/2,
         label = paste0(AIRPORT, "\n", round(fraction*100), "%"),
         AIRPORT=factor(AIRPORT, levels=mvts_country_apts$AIRPORT))

mvts_country_apts_fig = ggplot(data=mvts_country_apts, aes(x=3, y=NBR_MVMTS, fill=AIRPORT)) +
  geom_col() +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  geom_text(aes(x=4, label=label), position = position_stack(vjust = 0.5), size=60) +
  xlim(c(0.2, 3.5)) +
  theme_void() +
  theme(legend.position = "none")

ggsave(here("R", "Factsheet", "Figures", paste0("Mvmts_country_", Airport, ".png")), plot=mvts_country_apts_fig, 
       width = Traffic_layout2[1]*Page_width, height = Traffic_height2, units = "cm", dpi=100, limitsize = FALSE)
