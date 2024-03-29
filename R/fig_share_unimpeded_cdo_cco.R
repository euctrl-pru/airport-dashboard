
SHARE_CDO_CCO_FLIGHTS_PLOT <- params$cdo_cco %>% 
  select(YEAR, MONTH_NUM, NBR_FLIGHTS_DESCENT, NBR_CDO_FLIGHTS, SHARE_CDO_FLIGHTS, SHARE_CDO_FLIGHTS_BLW_70, 
         NBR_FLIGHTS_CLIMB, NBR_CCO_FLIGHTS, SHARE_CCO_FLIGHTS, SHARE_CCO_FLIGHTS_BLW_100)

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

if (nrow(filter(SHARE_CDO_CCO_FLIGHTS_PLOT, YEAR==max_year))>1) {
  
  share_cdo_cdo_fig = plot_ly(SHARE_CDO_CCO_FLIGHTS_PLOT, 
        x = ~MONTH_NUM, 
        y = ~SHARE_CDO_FLIGHTS*100, 
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
  add_trace(y = ~SHARE_CCO_FLIGHTS*100, 
            line = list(color = 'rgb(106, 168, 82)'),
            name = 'Climb (Fuel CCO)') %>%
  add_trace(y = ~SHARE_CDO_FLIGHTS_BLW_70*100, 
            line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
            name = 'Descent (Noise CDO)') %>% 
  add_trace(y = ~SHARE_CCO_FLIGHTS_BLW_100*100, 
            line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
            name = 'Climb (Noise CCO)')
  
} else {
  
  share_cdo_cdo_fig = plot_ly(SHARE_CDO_CCO_FLIGHTS_PLOT, 
                              x = ~MONTH_NUM, 
                              y = ~SHARE_CDO_FLIGHTS*100, 
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
    add_trace(y = ~SHARE_CCO_FLIGHTS*100, 
              line = list(color = 'rgb(106, 168, 82)'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year,
              symbols = 'circle',
              name = 'Climb (Fuel CCO)') %>%
    add_trace(y = ~SHARE_CDO_FLIGHTS_BLW_70*100, 
              line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
              marker = list(color = 'rgb(40, 120, 181)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Descent (Noise CDO)') %>% 
    add_trace(y = ~SHARE_CCO_FLIGHTS_BLW_100*100, 
              line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Climb (Noise CCO)')
  
}

share_cdo_cdo_fig = share_cdo_cdo_fig  %>% 
  layout(barmode = 'group',
         hovermode   = "x unified",
         xaxis       = tick_yr_no_title,
         yaxis       = list( title="Share of CDO/CCO flights (%)",
                             titlefont = list(size = 11),
                             ticksuffix = "%",  
                             range = c(0, 100)),
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

share_cdo_cdo_fig



# Factsheet figure

SHARE_CDO_CCO_FLIGHTS_PLOT_curr_year=filter(SHARE_CDO_CCO_FLIGHTS_PLOT, YEAR==max_year) %>% 
  ungroup() %>% 
  group_by(YEAR) %>% 
  summarise(TOT_NBR_FLIGHTS_DESCENT=sum(NBR_FLIGHTS_DESCENT),
            TOT_NBR_FLIGHTS_CLIMB=sum(NBR_FLIGHTS_CLIMB),
            SHARE_CDO_FLIGHTS=sum(NBR_CDO_FLIGHTS)/sum(NBR_FLIGHTS_DESCENT),
            SHARE_CCO_FLIGHTS=sum(NBR_CCO_FLIGHTS)/sum(NBR_FLIGHTS_CLIMB))

SHARE_CDO_CCO_FLIGHTS_PLOT_curr_year_fig = ggplot(data=SHARE_CDO_CCO_FLIGHTS_PLOT_curr_year) +
  geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=2), fill=rgb(40, 120, 181, maxColorValue = 255)) +
  geom_text(aes(x=0.5, y=1.5,
                label=paste0(round(SHARE_CDO_FLIGHTS*100, 1), "% CDO\n(", TOT_NBR_FLIGHTS_DESCENT, " flights)")), 
            size=60, colour="black") +
  geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill=rgb(106, 168, 82, maxColorValue = 255)) +
  geom_text(aes(x=0.5, y=0.5,
                label=paste0(round(SHARE_CCO_FLIGHTS*100, 1), "% CCO\n(", TOT_NBR_FLIGHTS_CLIMB, " flights)")), 
            size=60, colour="black") +
  theme_factsheet() +
  theme_void()

ggsave(here("media", "factsheet", paste0("Share_CDO_CCO_flights_", params$icao, ".pdf")), 
       plot=SHARE_CDO_CCO_FLIGHTS_PLOT_curr_year_fig, width = VFE_layout1[3]*Page_width, height = VFE_height1, 
       units = "cm", dpi=100, limitsize = FALSE)


