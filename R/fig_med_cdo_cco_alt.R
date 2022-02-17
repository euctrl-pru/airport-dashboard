
MED_CDO_CCO_ALT_PLOT <- params$cdo_cco %>% 
  select(YEAR, MONTH_NUM, MEDIAN_CDO_ALT, MEDIAN_CCO_ALT)

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

plot_ly(MED_CDO_CCO_ALT_PLOT, 
        x = ~MONTH_NUM, 
        y = ~MEDIAN_CDO_ALT, 
        customdata = ~YEAR, 
        line = list(color = 'rgb(40, 120, 181)'),
        hovertemplate = "%{y:.0f}",
        type = 'scatter',
        mode = 'lines',
        name = 'Descent (Fuel CDO)',
        transforms = list(list(type      = 'filter',
                               target    = "customdata",
                               operation = '=',
                               value     = max_year)
        )) %>% 
  add_trace(y = ~MEDIAN_CCO_ALT, 
            line = list(color = 'rgb(106, 168, 82)'),
            name = 'Climb (Fuel CCO)') %>%
  layout(barmode = 'group',
         hovermode   = "x unified",
         xaxis       = tick_yr_no_title,
         yaxis       = list( title="Median CDO/CCO altitude (feet)",
                             titlefont = list(size = 11),
                             range = list(0, 40000)),
         showlegend  = TRUE,
         updatemenus = list( button_type_list )
         ##############################################################################
         # ,annotations = list(
         #   x         = 1, 
         #   y         = -0.1, 
         #   text      = "Source: AIU analysis", 
         #   showarrow = F, 
         #   xref      = 'paper', 
         #   yref      = 'paper', 
         #   xanchor   = 'right', 
         #   yanchor   = 'auto', 
         #   xshift    = 0, 
         #   yshift    = -10,
         #   font      =list(size=12)
         # )          
         ##############################################################################
  ) %>% 
  config( displaylogo = FALSE,
          modeBarButtonsToRemove = config_bar_remove_buttons)





# Factsheet figure

MED_CDO_CCO_ALT_PLOT_cols=c(rgb(40, 120, 181, maxColorValue = 255), rgb(106, 168, 82, maxColorValue = 255))
MED_CDO_CCO_ALT_PLOT_curr_year=filter(MED_CDO_CCO_ALT_PLOT, YEAR==max_year) %>% 
  tidyr::pivot_longer(
    cols      = starts_with("MEDIAN_C"),
    names_to  = "Metric",
    values_to = "Value"
  ) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb)) %>% 
  arrange(YEAR, Month, Metric)


MED_CDO_CCO_ALT_PLOT_fig = ggplot(data=MED_CDO_CCO_ALT_PLOT_curr_year) +
  geom_line(aes(x=Month, y = Value, group=Metric, colour=Metric), size=linesize_factsheet) +
  scale_colour_manual(name ="", values = MED_CDO_CCO_ALT_PLOT_cols,
                      labels=c("Descent (Fuel CDO)", "Climb (Fuel CCO)")) +
  theme_factsheet() +
  # theme_bw() +
  # theme(plot.title = element_text(size=100, face="bold", hjust=0.5),
  #       legend.title=element_blank(),
  #       legend.text=element_text(size=36),
  #       legend.position = "bottom",
  #       axis.text=element_text(size=100),
  #       axis.title=element_text(size=100),
  #       axis.title.x = element_blank(),
  #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="Median CDO/CCO altitude (feet)\n", title="")

ggsave(here("R", "Factsheet", "Figures", paste0("Med_CDO_CCO_alt_", params$icao, ".png")), 
       plot=MED_CDO_CCO_ALT_PLOT_fig, width = VFE_layout1[2]*Page_width, height = VFE_height1, 
       units = "cm", dpi=100, limitsize = FALSE)


