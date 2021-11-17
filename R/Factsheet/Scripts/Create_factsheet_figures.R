
# Total movements per year
source(here("R", "Factsheet", "Scripts", "Total_mvmts_airport.R"), encoding = "UTF8")

source(here("R","apt_dshbd_create_df.R"), encoding = "UTF8")
source(here("R","apt_dshbd_utils.R"), encoding = "UTF8")




# Monthly traffic

mvts_pm <- filter(TFC_DF, AIRPORT==Airport) %>%
  select(YEAR, MONTH_NUM, FLT_TOT) %>%
  group_by(YEAR, MONTH_NUM) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  ungroup()%>%
  mutate(YEAR = as.character(YEAR),
         MONTH_MON=toupper(month.abb[MONTH_NUM]),
         MONTH_MON=factor(MONTH_MON, levels = toupper(month.abb)))

mvts_pm_fig = ggplot(mvts_pm) +
  geom_line(aes(x=MONTH_MON, y = FLT_TOT, group = YEAR, color = YEAR), size=2) +
  theme_bw() +
  theme(plot.title = element_text(size=40, face="bold", hjust=0.5),
        legend.title=element_blank(),
        legend.text=element_text(size=36),
        legend.position = "bottom",
        axis.text=element_text(size=30),
        axis.title=element_text(size=38),
        axis.title.x = element_blank(),
        plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="", title="Monthly movements")
ggsave(here("R", "Factsheet", "Figures", paste0("Monthly_mvmts_", Airport, ".png")), 
       width = 50, height = 30, units = "cm", dpi=200)
