
punc_dep_mm_plot <- params$punc_dep_mm

#punc_dep_mm_plot$PUNCT_CAT <- factor(
#  punc_dep_mm_plot$PUNCT_CAT,
#  levels = c(
#    "Early >30",
#    "Early 16-30",
#    "Early 5-15",
#    "On Time -+4",
#    "Late 5-15",
#    "Late 16-30",
#    "Late 31-60",
#    "Late >60"))


punc_dep_mm_plot$PUNCT_CAT <- factor(
  punc_dep_mm_plot$PUNCT_CAT,
  levels = c(
    "[<-30]",
    "[-16, -30]",
    "[- 5, -15]",
    "[- 4, + 4]",
    "[+ 5, +15]",
    "[+16, +30]",
    "[+31, +60]",
    "[>+60]"
  )
)



filter_years <- punc_dep_mm_plot %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

button_list <- lapply(
  1:length(filter_years),
  function(x) {
    list(
      method = "restyle",
      args = list("transforms[0].value", filter_years[x]),
      label = filter_years[x]
    )
  }
)

button_type_list <- list(
  buttons   = button_list,
  type      = "buttons",
  bgcolor   = "#c3c1e8",
  active    = length(filter_years) - 1,
  direction = "right",
  xref      = "paper",
  x         = 0.3,
  xanchor   = "left",
  yref      = "paper",
  y         = 1.2,
  yanchor   = "bottom"
)

punc_dep_mm_plot %>%
  plot_ly(
    x          = ~MONTH_NUM,
    y          = ~AVG_PER_CATEG,
    customdata = ~YEAR,
    color      = ~PUNCT_CAT,
    colors     = pun_col,
    type       = "bar",
    transforms = list(
      list(
        type      = "filter",
        target    = "customdata",
        operation = "=",
        value     = max_year
      )
    )
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = tick_yr_no_title,
    yaxis     = list(
      title       = "Departure Punctuality repartition (%)",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
    ),
    showlegend = TRUE,
    updatemenus = list(button_type_list)#,
    ##############################################################################
    #    annotations = list(
    #      x         = 1,
    #      y         = -0.1,
    #      text      = "Source: APDF",
    #      showarrow = FALSE,
    #      xref      = "paper",
    #      yref      = "paper",
    #      xanchor   = "right",
    #      yanchor   = "auto",
    #      xshift    = 0,
    #      yshift    = -10,
    #      font      = list(size = 12)
    #    )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure
punc_dep_mm_plot_curr_year=filter(punc_dep_mm_plot, YEAR == max_year) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb))

punc_dep_mm_plot_fig = ggplot(data=punc_dep_mm_plot_curr_year) +
  geom_bar(aes(x=Month, y=AVG_PER_CATEG, fill=PUNCT_CAT), stat="identity", position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values=pun_col) +
  theme_factsheet() +
  # theme_bw() +
  # theme(plot.title = element_blank(),
  #       legend.title=element_blank(),
  #       legend.text=element_text(size=36),
  #       legend.position = "bottom",
  #       axis.text=element_text(size=100),
  #       axis.title=element_text(size=100),
  #       axis.title.x = element_blank(),
  #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="Departure Punctuality repartition (%)\n")
ggsave(here("media", "factsheet", paste0("Punc_Dep_Monthly_", params$icao, ".pdf")), plot=punc_dep_mm_plot_fig, 
       width = Punctuality_layout1[2]*Page_width, height = Punctuality_height1, units = "cm", dpi=100, limitsize = FALSE)

