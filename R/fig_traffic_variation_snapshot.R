

if (nrow(params$tfcvar) < 1) {
  cat("<p><b>Traffic evolution for this airport under development!</b>")
} else {
  fig1 <- params$tfcvar %>%
    plot_ly() %>%
    add_lines(
      x = ~DAY,
      y = ~FLTS,
      name = "Flights"
    ) %>%
    add_lines(
      x = ~DAY,
      y = ~FLTS_2019,
      name = "Flights 2019 (Reference)",
      line = list(
        dash = "dot",
        color = "red"
      )
    ) %>%
    layout(yaxis = list(title = "movements"))
  
  fig2 <- params$tfcvar %>%
    plot_ly() %>%
    add_lines(
      x = ~DAY,
      y = ~MOV_AVG_WK,
      name = " <br> % vs 2019 <br> (7-day Moving Average)"
    ) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(
        title = "% change from 2019",
        titlefont = list(size = 11),
        tickformat = ".0%"
      )
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
      #   yshift = -20,
      #   font = list(size = 12)
      # )
      ##############################################################################
    )
  
  fig <- subplot(fig1, fig2, nrows = 2, shareX = TRUE) %>%
    layout(hovermode = "x unified") %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = config_bar_remove_buttons
    )
  
  fig
}





# Factsheet figure

library(reshape2)
library(patchwork)

Tfc_var1=melt(params$tfcvar, id.vars = "DAY", measure.vars = c("FLTS", "FLTS_2019", "MOV_AVG_WK")) %>%
  mutate(variable2 = ifelse(variable=="FLTS",
                           "Flights",
                           ifelse(variable=="FLTS_2019",
                                  "Flights 2019\n(Reference)",
                                  ifelse(variable=="MOV_AVG_WK",
                                         "% vs 2019\n(7-day Moving\nAverage)",
                                         variable))))

Tfc_var_fig1 = ggplot(data=filter(Tfc_var1, variable %in% c("FLTS", "FLTS_2019"))) +
  geom_line(aes(x=DAY, y=value, group=variable2, colour=variable2), size=linesize_factsheet) +
  scale_color_manual(values = c("blue", "red")) +
  theme_factsheet() +
  theme(legend.position = "right",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(100, 20, 30, 60), "pt")) +
  labs(x="", y="")
Tfc_var_fig2 = ggplot(data=filter(Tfc_var1, variable == "MOV_AVG_WK")) +
  geom_line(aes(x=DAY, y=value, linetype='% vs 2019\n(7-day Moving\nAverage)', group=variable2),
            colour="green", size=linesize_factsheet)+
  scale_y_continuous(labels = scales::percent, breaks = seq(-2, 2, by = 0.2)) +
  scale_x_date(date_labels = "%b %Y", breaks = seq(as.Date("2020-01-01"), max(Tfc_var1$DAY), "4 months")) +
  theme_factsheet() +
  theme(legend.position = "right") +
  labs(x="", y="")
Tfc_var_fig=Tfc_var_fig1/Tfc_var_fig2 + plot_layout(guides = "collect")
ggsave(here("media", "factsheet", paste0("Tfc_var_snapshot_", params$icao, ".png")), plot=Tfc_var_fig,
       width = Traffic_layout2[2]*Page_width, height = Traffic_height2, units = "cm", dpi=100, limitsize = FALSE)

