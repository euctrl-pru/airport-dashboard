
pun_col <- c(
  "#1f4e79", # EARLY >30		#rgb 31,78,121
  "#8497b0", # EARLY 16-30	#rgb 132,151,176
  "#c5e0b4", # EARLY 5-15	  #rgb 197,224,180
  "#70ad47", # ON TIME -+4	#rgb 112,173,71
  "#ffe699", # LATE 5-15		#rgb 255,230,153
  "#e1c000", # LATE 16-30	  #rgb 225,192,0
  "#ff0000", # LATE 31-60	  #rgb 255,0,0
  "#c00000"  # LATE >60		  #rgb 192,0,0
)           

punc_dep_yy_plot <- params$punc_dep_yy

#punc_dep_yy_plot$PUNCT_CAT <- factor(punc_dep_yy_plot$PUNCT_CAT,
#                                     levels = c(
#                                       "Early >30",
#                                       "Early 16-30",
#                                       "Early 5-15",
#                                       "On Time -+4",
#                                       "Late 5-15",
#                                       "Late 16-30",
#                                       "Late 31-60",
#                                       "Late >60"
#                                     )
#)

punc_dep_yy_plot$PUNCT_CAT <- factor(punc_dep_yy_plot$PUNCT_CAT,
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


punc_dep_yy_plot %>%
  plot_ly(
    x      = ~YEAR,
    y      = ~AVG_PER_CATEG,
    type   = "bar",
    color  = ~PUNCT_CAT,
    colors = pun_col
  ) %>%
  layout(
    barmode = "stack",
    hovermode = "x unified",
    xaxis = list(title = ""),
    yaxis = list(
      title = "Departure Punctuality repartition (%)",
      titlefont = list(size = 11),
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

punc_dep_yy_plot_fig = ggplot(data=punc_dep_yy_plot) +
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
  labs(x="", y="Departure Punctuality repartition (%)\n")
ggsave(here("media", "factsheet", paste0("Punc_Dep_Yearly_", params$icao, ".pdf")), plot=punc_dep_yy_plot_fig, 
       width = Punctuality_layout1[1]*Page_width, height = Punctuality_height1, units = "cm", dpi=100, limitsize = FALSE)

