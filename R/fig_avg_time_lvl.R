
AVG_TIME_LVL_PLOT <- params$cdo_cco %>%
  select(YEAR, MONTH_NUM, AVG_TIME_LVL_DESCENT, AVG_TIME_LVL_DESCENT_BLW_70, AVG_TIME_LVL_CLIMB, AVG_TIME_LVL_CLIMB_BLW_100)

filter_years <- AVG_TIME_LVL_PLOT %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

AVG_TIME_LVL_min=group_by(AVG_TIME_LVL_PLOT, YEAR) %>% 
  filter(AVG_TIME_LVL_DESCENT==min(AVG_TIME_LVL_DESCENT)) %>% 
  arrange(YEAR)

annotations = vector(mode = "list", length = length(filter_years))
for (i in 1:length(filter_years)) {
  AVG_TIME_LVL_min_temp=filter(AVG_TIME_LVL_min, YEAR==filter_years[i])
  if (nrow(AVG_TIME_LVL_min_temp)>0) {
    annotation <- list(x = AVG_TIME_LVL_min_temp$MONTH_NUM,
                       y = AVG_TIME_LVL_min_temp$AVG_TIME_LVL_DESCENT/60,
                       xref='x',
                       yref='y',
                       text = paste0(
                         "<b>Lowest value<br>(descent - Fuel CDO):<br>",
                         round(AVG_TIME_LVL_min_temp$AVG_TIME_LVL_DESCENT/60, 1), " min.</b>"),
                       yshift=40,
                       xshift = ifelse(AVG_TIME_LVL_min_temp$MONTH_NUM==1,
                                       70,
                                       ifelse(AVG_TIME_LVL_min_temp$MONTH_NUM==12, -70, 0)),
                       align = ifelse(AVG_TIME_LVL_min_temp$MONTH_NUM==1,
                                      "left",
                                      ifelse(AVG_TIME_LVL_min_temp$MONTH_NUM==12, "right", "center")),
                       showarrow = FALSE)
  } else {
    annotation <- list(x = 6,
                       y = 1,
                       xref='x',
                       yref='y',
                       text = "",
                       yshift=0,
                       xshift = 0,
                       align = "center",
                       showarrow = FALSE)
  }
  
  annotations[[i]] <- annotation
}

# source_annotation=list(x         = 1, 
#                        y         = -0.1, 
#                        text      = "Source: AIU analysis", 
#                        showarrow = F, 
#                        xref      = 'paper', 
#                        yref      = 'paper', 
#                        xanchor   = 'right', 
#                        yanchor   = 'auto', 
#                        xshift    = 0, 
#                        yshift    = -10,
#                        font      = list(size=12))

button_list=vector(mode = "list", length = length(filter_years))
annot_list=vector(mode = "list", length = length(filter_years))
for(x in 1:length(filter_years)){
  annot_list_temp=annot_list
  annot_list_temp[[x]]=annotations[[x]]
  if (!is.null(annotations[[x]])) {
    bl=list(
      label  = filter_years[x],
      method = "update",
      args   = list(list("transforms[0].value"=filter_years[x]),
                    list(annotations=list(annot_list_temp[[x]]
                                          # , source_annotation
                    )))
    )
  } else {
    bl=list(
      label  = filter_years[x],
      method = "update",
      args   = 
        # list(
        list("transforms[0].value"=filter_years[x])
      # ,list(annotations=list(source_annotation))
      # )
    )
  }
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


# AVG_TIME_LVL_PLOT=mutate(AVG_TIME_LVL_PLOT, YEAR = as.character(YEAR))

if (nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))>1) {
  
  avg_time_lvl_fig=plot_ly(AVG_TIME_LVL_PLOT, 
                           x = ~MONTH_NUM, 
                           y = ~AVG_TIME_LVL_DESCENT/60, 
                           customdata = ~YEAR, 
                           line = list(color = 'rgb(40, 120, 181)'),
                           hovertemplate = "%{y:.1f}",
                           type = 'scatter',
                           mode = 'lines',
                           name = 'Descent (Fuel CDO)',
                           transforms = list(list(type      = 'filter',
                                                  target    = "customdata",
                                                  operation = '=',
                                                  value     = max_year)
                           )) %>% 
    add_trace(y = ~AVG_TIME_LVL_CLIMB/60, 
              mode = 'lines',
              line = list(color = 'rgb(106, 168, 82)'),
              name = 'Climb (Fuel CCO)') %>%
    add_trace(y = ~AVG_TIME_LVL_DESCENT_BLW_70/60, 
              mode = 'lines',
              line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
              name = 'Descent (Noise CDO)') %>% 
    add_trace(y = ~AVG_TIME_LVL_CLIMB_BLW_100/60, 
              mode = 'lines',
              line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
              name = 'Climb (Noise CCO)')
  
} else {
  
  avg_time_lvl_fig=plot_ly(AVG_TIME_LVL_PLOT, 
                           x = ~MONTH_NUM, 
                           y = ~AVG_TIME_LVL_DESCENT/60, 
                           customdata = ~YEAR, 
                           line = list(color = 'rgb(40, 120, 181)'),
                           marker = list(color = 'rgb(40, 120, 181)'),
                           symbol = ~max_year,
                           symbols = 'circle',
                           hovertemplate = "%{y:.1f}",
                           type = 'scatter',
                           mode = ifelse(nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))>1, 'lines', 'lines+markers'),
                           name = 'Descent (Fuel CDO)',
                           transforms = list(list(type      = 'filter',
                                                  target    = "customdata",
                                                  operation = '=',
                                                  value     = max_year)
                           )) %>% 
    add_trace(y = ~AVG_TIME_LVL_CLIMB/60, 
              mode = ifelse(nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))>1, 'lines', 'lines+markers'),
              line = list(color = 'rgb(106, 168, 82)'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year,
              symbols = 'circle',
              name = 'Climb (Fuel CCO)') %>%
    add_trace(y = ~AVG_TIME_LVL_DESCENT_BLW_70/60, 
              mode = ifelse(nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))>1, 'lines', 'lines+markers'),
              line = list(color = 'rgb(40, 120, 181)', dash = 'dash'),
              marker = list(color = 'rgb(40, 120, 181)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Descent (Noise CDO)') %>% 
    add_trace(y = ~AVG_TIME_LVL_CLIMB_BLW_100/60, 
              mode = ifelse(nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))>1, 'lines', 'lines+markers'),
              line = list(color = 'rgb(106, 168, 82)', dash = 'dash'),
              marker = list(color = 'rgb(106, 168, 82)'),
              symbol = ~max_year+1,
              symbols = 'triangle-up',
              name = 'Climb (Noise CCO)')
}

# %>% 
# add_annotations(x         = 1, 
#                 y         = -0.1, 
#                 text      = "Source: AIU analysis", 
#                 showarrow = F, 
#                 xref      = 'paper', 
#                 yref      = 'paper', 
#                 xanchor   = 'right', 
#                 yanchor   = 'auto', 
#                 xshift    = 0, 
#                 yshift    = -10,
#                 font      = list(size=12)
# ) 

avg_time_lvl_fig=avg_time_lvl_fig %>%
  layout(barmode = 'group',
         hovermode   = "x unified",
         xaxis       = tick_yr_no_title,
         yaxis       = list( title="Average time flown level per flight (min.)",
                             titlefont = list(size = 11)),
         showlegend  = TRUE,
         updatemenus = list( button_type_list )
         
  ) %>% 
  config( displaylogo = FALSE,
          modeBarButtonsToRemove = config_bar_remove_buttons) %>% 
  add_download_button(
    AVG_TIME_LVL_PLOT %>%
      mutate(
        AVG_TIME_LVL_DESCENT = AVG_TIME_LVL_DESCENT/60,
        AVG_TIME_LVL_CLIMB = AVG_TIME_LVL_CLIMB/60, 
        AVG_TIME_LVL_DESCENT_BLW_70 = AVG_TIME_LVL_DESCENT_BLW_70/60, 
        AVG_TIME_LVL_CLIMB_BLW_100 = AVG_TIME_LVL_CLIMB_BLW_100/60) %>%
      select(
        YEAR,
        MONTH = MONTH_NUM, 
        AVG_TIME_LVL_DESCENT_FUEL_CDO = AVG_TIME_LVL_DESCENT, 
        AVG_TIME_LVL_CLIMB_FUEL_CCO = AVG_TIME_LVL_CLIMB, 
        AVG_TIME_LVL_DESCENT_NOISE_CDO = AVG_TIME_LVL_DESCENT_BLW_70,
        AVG_TIME_LVL_CLIMB_NOISE_CCO = AVG_TIME_LVL_CLIMB_BLW_100), 
    "AVG_TIME_LVL_MM")

if (!is.null(annotations[[length(filter_years)]])) {
  avg_time_lvl_fig=avg_time_lvl_fig %>% 
    add_annotations(x         = annotations[[length(filter_years)]]$x,
                    y         = annotations[[length(filter_years)]]$y,
                    text      = annotations[[length(filter_years)]]$text,
                    showarrow = annotations[[length(filter_years)]]$showarrow,
                    xref      = annotations[[length(filter_years)]]$xref,
                    yref      = annotations[[length(filter_years)]]$yref,
                    yshift    = annotations[[length(filter_years)]]$yshift,
                    xshift    = annotations[[length(filter_years)]]$xshift,
                    align     = annotations[[length(filter_years)]]$align
    )
}

avg_time_lvl_fig





# Factsheet figure

AVG_TIME_LVL_PLOT_cols=c(rgb(40, 120, 181, maxColorValue = 255), rgb(40, 120, 181, maxColorValue = 255),
                         rgb(106, 168, 82, maxColorValue = 255), rgb(106, 168, 82, maxColorValue = 255))
AVG_TIME_LVL_PLOT_curr_year=filter(AVG_TIME_LVL_PLOT, YEAR==max_year) %>% 
  tidyr::pivot_longer(
    cols      = starts_with("AVG_TIME_LVL_"),
    names_to  = "Metric",
    values_to = "Value"
  ) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb)) %>% 
  arrange(YEAR, Month, Metric)


AVG_TIME_LVL_PLOT_fig = ggplot(data=AVG_TIME_LVL_PLOT_curr_year) +
  geom_line(aes(x=Month, y = Value/60, group=Metric, colour=Metric, linetype=Metric), size=linesize_factsheet) +
  scale_linetype_manual(name="", values = c("solid", "dashed", "solid", "dashed"),
                        labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)")) +
  scale_colour_manual(name ="", values = AVG_TIME_LVL_PLOT_cols,
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

if (nrow(filter(AVG_TIME_LVL_PLOT, YEAR==max_year))==1) {
  
  AVG_TIME_LVL_PLOT_fig = AVG_TIME_LVL_PLOT_fig +
    geom_point(aes(x=Month, y = Value/60, group=Metric, colour=Metric, shape=Metric), size=20) +
    scale_shape_manual(name="", values = c(16, 17, 16, 17),
                       labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)")) +
    scale_colour_manual(name ="", values = AVG_TIME_LVL_PLOT_cols,
                        labels=c("Descent (Fuel CDO)", "Descent (Noise CDO)", "Climb (Fuel CCO)", "Climb (Noise CCO)"))
}

ggsave(here("media", "factsheet", paste0("Avg_time_lvl_", params$icao, ".pdf")), 
       plot=AVG_TIME_LVL_PLOT_fig, width = VFE_layout1[1]*Page_width, height = VFE_height1, 
       units = "cm", dpi=100, limitsize = FALSE)
