
punc_arr_yy_plot <- params$punc_arr_yy

#punc_arr_yy_plot$PUNCT_CAT <- factor(
#  punc_arr_yy_plot$PUNCT_CAT,
#  levels = c(
#    "Early >30",
#    "Early 16-30",
#    "Early 5-15",
#    "On Time -+4",
#    "Late 5-15",
#    "Late 16-30",
#    "Late 31-60",
#    "Late >60"
#  )
#)


punc_arr_yy_plot$PUNCT_CAT <- factor(
  punc_arr_yy_plot$PUNCT_CAT,
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



punc_arr_yy_plot %>%
  plot_ly(
    x      = ~YEAR,
    y      = ~AVG_PER_CATEG,
    color  = ~PUNCT_CAT,
    colors = pun_col,
    type   = "bar"
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title       = "Arrival Punctuality repartition (%)",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
    )#,
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
    #      yshift    = -5,
    #      font      = list(size = 12)
    #    )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

punc_arr_yy_plot_fig = ggplot(data=punc_arr_yy_plot) +
  geom_bar(aes(x=YEAR, y=AVG_PER_CATEG, fill=PUNCT_CAT), stat="identity", position = position_stack(reverse = TRUE)) +
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
  labs(x="", y="Arrival Punctuality repartition (%)\n")
ggsave(here("media", "factsheet", paste0("Punc_Arr_Yearly_", params$icao, ".pdf")), plot=punc_arr_yy_plot_fig, 
       width = Punctuality_layout2[1]*Page_width, height = Punctuality_height2, units = "cm", dpi=100, limitsize = FALSE)

