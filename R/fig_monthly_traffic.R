
library("ggrepel")

mvts_pm <- params$tfc %>%
  select(YEAR, MONTH_NUM, FLT_TOT) %>%
  group_by(YEAR, MONTH_NUM) %>%
  summarise(FLT_TOT = sum(FLT_TOT, na.rm = TRUE)) %>%
  ungroup()

mvts_pm_max <- mvts_pm %>% filter(FLT_TOT == max(FLT_TOT))

msg <- paste0(
  "<b>Busiest month <br>(last 5 years): <br>",
  mvts_pm_max$MONTH_NUM, "-",
  mvts_pm_max$YEAR,
  "<br>", mvts_pm_max$FLT_TOT, "</b>"
)

xax <- tick_yr_no_title

yax <- list(
  title = "Number of flights",
  rangemode = "tozero",
  titlefont = list(size = 11)
)

mvts_pm <- mvts_pm %>%
  mutate(YEAR = as.character(YEAR))

mvts_pm %>%
  plot_ly(
    x = ~MONTH_NUM,
    y = ~FLT_TOT,
    group = ~YEAR,
    color = ~YEAR
  ) %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines",
    hovertemplate = "%{y:.r}"
  ) %>%
  add_annotations(
    text = msg,
    x = mvts_pm_max$MONTH_NUM,
    y = mvts_pm_max$FLT_TOT,
    showarrow = TRUE,
    ax = 50,
    ay = 100
  ) %>%
  layout(
    xaxis = xax,
    yaxis = yax,
    hovermode = "x unified"
    ##############################################################################
    # , annotations = list(
    #   x = 1,
    #   y = -0.1,
    #   text = "Source: Network Manager",
    #   showarrow = FALSE,
    #   xref = "paper",
    #   yref = "paper",
    #   xanchor = "right",
    #   yanchor = "auto",
    #   xshift = 0,
    #   yshift = -5,
    #   font = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

mvts_pm_fig = ggplot(mutate(mvts_pm,
                            MONTH_MON=toupper(month.abb[MONTH_NUM]),
                            MONTH_MON=factor(MONTH_MON, levels = toupper(month.abb)))) +
  geom_line(aes(x=MONTH_MON, y = FLT_TOT, group = YEAR, color = YEAR), size=linesize_factsheet) +
  geom_text_repel(data=mvts_pm_max, aes(x=factor(toupper(month.abb[MONTH_NUM]), levels = toupper(month.abb)), y=FLT_TOT, 
                                        label = paste0("Busiest month\n(last 5 years):\n", 
                                                       MONTH_NUM, "-", YEAR, "\n", FLT_TOT)), 
                  size = 50, nudge_y = -mvts_pm_max$FLT_TOT/4, segment.color = 'black', segment.size = 5,
                  arrow = arrow(length = unit(0.015, "npc")))+ 
  theme_factsheet() +
  theme(legend.position = "right") +
  # theme_bw() +
  # theme(plot.title = element_text(size=100, face="bold", hjust=0.5),
  #       legend.title=element_blank(),
  #       legend.text=element_text(size=36),
  #       legend.position = "bottom",
  #       axis.text=element_text(size=100),
  #       axis.title=element_text(size=100),
  #       axis.title.x = element_blank(),
  #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="", title="")
ggsave(here("media", "factsheet", paste0("Monthly_mvmts_", params$icao, ".png")), 
       width = Traffic_layout1[2]*Page_width, height = Traffic_height1, units = "cm", dpi=100, limitsize = FALSE)

