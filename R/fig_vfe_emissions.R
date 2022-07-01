
VFE_EMISSIONS_PLOT <- params$cdo_cco %>% 
  select(YEAR, MONTH_NUM, NBR_FLIGHTS_DESCENT, TOT_DELTA_CO2_KG_DESCENT, TOT_DELTA_CO2_KG_DESCENT_BELOW_7000,
         NBR_FLIGHTS_CLIMB, TOT_DELTA_CO2_KG_CLIMB, TOT_DELTA_CO2_KG_CLIMB_BELOW_10000)

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

if (nrow(filter(VFE_EMISSIONS_PLOT, YEAR==max_year))>1) {
  
  vfe_emissions_fig = plot_ly(VFE_EMISSIONS_PLOT, 
                              x = ~MONTH_NUM, 
                              y = ~TOT_DELTA_CO2_KG_DESCENT/1000, 
                              customdata = ~YEAR, 
                              line = list(color = 'rgb(40, 120, 181)'),
                              hovertemplate = "%{y:.1f}%",
                              type = 'scatter',
                              mode = 'lines', 
                              name = 'Descent (Fuel CDO)',
                              transforms = list(list(type      = 'filter',
                                                     target    = "customdata",
                                                     operation = '=',
                                                     value     = max_year)
                              )) %>% 
    add_trace(y = ~TOT_DELTA_CO2_KG_CLIMB/1000, 
              line = list(color = 'rgb(106, 168, 82)'),
              name = 'Climb (Fuel CCO)') %>%
    add_trace(y = ~TOT_DELTA_CO2_KG_DESCENT_BELOW_7000/1000, 
              line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
              name = 'Descent (Noise CDO)') %>% 
    add_trace(y = ~TOT_DELTA_CO2_KG_CLIMB_BELOW_10000/1000, 
              line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
              name = 'Climb (Noise CCO)')
  
} else {
  
  vfe_emissions_fig = plot_ly(VFE_EMISSIONS_PLOT, 
                              x = ~MONTH_NUM, 
                              y = ~TOT_DELTA_CO2_KG_DESCENT/1000, 
                              customdata = ~YEAR, 
                              line = list(color = 'rgb(40, 120, 181)'),
                              marker = list(color = 'rgb(40, 120, 181)'),
                              symbol = ~max_year,
                              symbols = 'circle',
                              hovertemplate = "%{y:.1f}%",
                              type = 'scatter',
                              mode = 'lines', 
                              name = 'Descent (Fuel CDO)',
                              transforms = list(list(type      = 'filter',
                                                     target    = "customdata",
                                                     operation = '=',
                                                     value     = max_year)
                              )) %>% 
    add_trace(y = ~TOT_DELTA_CO2_KG_CLIMB/1000, 
              line = list(color = 'rgb(106, 168, 82)'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year,
              symbols = 'circle',
              name = 'Climb (Fuel CCO)') %>%
    add_trace(y = ~TOT_DELTA_CO2_KG_DESCENT_BELOW_7000/1000, 
              line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
              marker = list(color = 'rgb(40, 120, 181)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Descent (Noise CDO)') %>% 
    add_trace(y = ~TOT_DELTA_CO2_KG_CLIMB_BELOW_10000/1000, 
              line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Climb (Noise CCO)')
  
}

vfe_emissions_fig = vfe_emissions_fig  %>% 
  layout(barmode = 'group',
         hovermode   = "x unified",
         xaxis       = tick_yr_no_title,
         yaxis       = list( title="Total delta CO<sub>2</sub> emissions (tonnes)",
                             titlefont = list(size = 11)),
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

vfe_emissions_fig



# Factsheet figure

VFE_EMISSIONS_PLOT_cols=c(rgb(40, 120, 181, maxColorValue = 255), rgb(40, 120, 181, maxColorValue = 255),
                          rgb(106, 168, 82, maxColorValue = 255), rgb(106, 168, 82, maxColorValue = 255))
VFE_EMISSIONS_PLOT_curr_year=filter(VFE_EMISSIONS_PLOT, YEAR==max_year) %>% 
  tidyr::pivot_longer(
    cols      = starts_with("TOT_DELTA_CO2_KG_"),
    names_to  = "Metric",
    values_to = "Value"
  ) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb)) %>% 
  arrange(YEAR, Month, Metric)


VFE_EMISSIONS_PLOT_fig = ggplot(data=VFE_EMISSIONS_PLOT_curr_year) +
  geom_line(aes(x=Month, y = Value/1000, group=Metric, colour=Metric, linetype=Metric), size=linesize_factsheet) +
  scale_linetype_manual(name="", values = c("solid", "dashed", "solid", "dashed"),
                        labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)")) +
  scale_colour_manual(name ="", values = VFE_EMISSIONS_PLOT_cols,
                      labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)")) +
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
  labs(x="", y="Average time flown level per flight (min.)\n", title="")

if (nrow(filter(VFE_EMISSIONS_PLOT, YEAR==max_year))==1) {
  
  VFE_EMISSIONS_PLOT_fig = VFE_EMISSIONS_PLOT_fig +
    geom_point(aes(x=Month, y = Value/1000, group=Metric, colour=Metric, shape=Metric), size=20) +
    scale_shape_manual(name="", values = c(16, 17, 16, 17),
                       labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)")) +
    scale_colour_manual(name ="", values = AVG_TIME_LVL_PLOT_cols,
                        labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)"))
}

ggsave(here("media", "factsheet", paste0("vfe_emissions_", params$icao, ".png")), 
       plot=VFE_EMISSIONS_PLOT_fig, width = VFE_layout1[1]*Page_width, height = VFE_height1, 
       units = "cm", dpi=100, limitsize = FALSE)

