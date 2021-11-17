
mvts_pa <- filter(TFC_DF, AIRPORT==Airport)  %>%
  select(YEAR, FLT_TOT) %>%
  group_by(YEAR) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(YEAR = as.character(YEAR))

mvts_pa_fig = ggplot(data=mvts_pa) +
  geom_bar(aes(x=YEAR, y=FLT_TOT/1000), position = "dodge", stat="identity") +
  theme_bw() +
  theme(plot.title = element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=36),
        legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=38),
        axis.title.x = element_blank(),
        plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="Total movements (thousands)\n") +
  scale_y_continuous(labels = label_number(suffix = "k"))
ggsave(here("R", "Factsheet", "Figures", paste0("Tot_mvmts_", Airport, ".png")), 
       width = Traffic_layout1[1]*Page_width, height = Traffic_height1, units = "cm", dpi=200)
