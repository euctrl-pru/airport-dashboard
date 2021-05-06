
annotations = list()
for (i in 1:nrow(AVG_TIME_LVL_min)) {
  annotation <- list(x = AVG_TIME_LVL_min$MONTH_NUM[i],
                     y = AVG_TIME_LVL_min$AVG_TIME_LVL_DESCENT[i]/60,
                     xref='x',
                     yref='y',
                     text = paste0(
                       "<b>Lowest value (descent):<br>",
                       AVG_TIME_LVL_min$MONTH_NUM[i], "-",
                       AVG_TIME_LVL_min$YEAR[i],
                       "<br>", round(AVG_TIME_LVL_min$AVG_TIME_LVL_DESCENT[i]/60, 1), " min.</b>"),
                     ax=0,
                     ay=-40,
                     showarrow = FALSE)
  annotations[[i]] <- annotation
}

button_list=list()
vis_vector=rep(FALSE, length(filter_years))
annot_list=vector(mode = "list", length = length(filter_years))
for(x in 1:length(filter_years)){
  vis_vector_temp=vis_vector
  vis_vector_temp[x]=TRUE
  annot_list_temp=annot_list
  annot_list_temp[[x]]=annotations[[x]]
  bl=list(
    label  = filter_years[x],
    method = "update",
    args   = list(list(annotations = annot_list_temp))
  )
  button_list[[x]]=bl
}


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

plot_ly(AVG_TIME_LVL_PLOT, 
        x = ~MONTH_NUM, 
        y = ~AVG_TIME_LVL_DESCENT/60, 
        customdata = ~YEAR, 
        marker = list(color = 'rgb(40, 120, 181)'),
        hovertemplate = "%{y:.1f}",
        type = 'bar', 
        name = 'Descent',
        transforms = list(list(type      = 'filter',
                               target    = "customdata",
                               operation = '=',
                               value     = max_year)
        )) %>% 
  add_trace(y = ~AVG_TIME_LVL_CLIMB/60, 
            marker = list(color = 'rgb(106, 168, 82)'),
            name = 'Climb') %>%
  add_annotations(x         = 1, 
                  y         = -0.1, 
                  text      = "Source: AIU analysis", 
                  showarrow = F, 
                  xref      = 'paper', 
                  yref      = 'paper', 
                  xanchor   = 'right', 
                  yanchor   = 'auto', 
                  xshift    = 0, 
                  yshift    = -10,
                  font      = list(size=12)
  ) %>%
  layout(barmode = 'group',
         hovermode   = "x unified",
         xaxis       = tick_yr_no_title,
         yaxis       = list( title="Average time flown level per flight (min.)",
                             titlefont = list(size = 11)),
         showlegend  = TRUE,
         ##############################################################################
         # annotations = annotations,
         ##############################################################################
         updatemenus = list( button_type_list )
         
  ) %>% 
  config( displaylogo = FALSE,
          modeBarButtonsToRemove = config_bar_remove_buttons)
