
ASMA_MM <- params$asma_mm %>%
  tidyr::pivot_longer(
    cols      = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    names_to  = "TYPE",
    values_to = "TIME"
  )

ASMA_MM <- ASMA_MM %>%
  mutate(
    TYPE   = factor(TYPE,
                    levels = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
                    labels = c("Avg. Reference time", "Avg. Additional time")
    ))

filter_years <- ASMA_MM %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

MONTHLY_HIGH_ASMA = ASMA_MM %>% 
  filter(TYPE=="Avg. Additional time") %>% 
  mutate(AVG_TIME=(TOT_UNIMP_TIME + TOT_ADD_TIME)/TOT_FLT) %>% 
  select(YEAR, MONTH_NUM, TIME, AVG_TIME) %>% 
  group_by(YEAR) %>% 
  filter(TIME !=0) %>% 
  filter(TIME==max(TIME)) %>% 
  arrange(YEAR)

annotations = vector(mode = "list", length = length(filter_years))
for (i in 1:length(filter_years)) {
  MONTHLY_HIGH_ASMA_temp=filter(MONTHLY_HIGH_ASMA, YEAR==filter_years[i])
  if (nrow(MONTHLY_HIGH_ASMA_temp)>0) {
    annotation <- list(x = MONTHLY_HIGH_ASMA_temp$MONTH_NUM,
                       y = MONTHLY_HIGH_ASMA_temp$AVG_TIME,
                       xref='x',
                       yref='y',
                       text = paste0(
                         "<b>Highest average<br>additional ASMA time:<br>",
                         round(MONTHLY_HIGH_ASMA_temp$TIME, 2), " min/arr</b>"),
                       yshift=40,
                       xshift = ifelse(MONTHLY_HIGH_ASMA_temp$MONTH_NUM==1,
                                       45,
                                       ifelse(MONTHLY_HIGH_ASMA_temp$MONTH_NUM==12, -45, 0)),
                       align = ifelse(MONTHLY_HIGH_ASMA_temp$MONTH_NUM==1,
                                      "left",
                                      ifelse(MONTHLY_HIGH_ASMA_temp$MONTH_NUM==12, "right", "center")),
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
#                        text      = "Source: APDF", 
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
  type      = "buttons",
  bgcolor   = "#c3c1e8",
  active    = length(filter_years) - 1,
  direction = "right",
  xref      = "paper",
  x         = 0.3,
  xanchor   = "left",
  yref      = "paper",
  y         = 1.2,
  yanchor   = "bottom",
  buttons   = button_list
)

asma_mm_fig = ASMA_MM %>%
  plot_ly(
    x          = ~MONTH_NUM,
    y          = ~TIME,
    customdata = ~YEAR,
    color      = ~TYPE,
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
  # add_annotations(x         = 1, 
  #                 y         = -0.1, 
  #                 text      = "Source: APDF", 
  #                 showarrow = F, 
  #                 xref      = 'paper', 
  #                 yref      = 'paper', 
  #                 xanchor   = 'right', 
  #                 yanchor   = 'auto', 
  #                 xshift    = 0, 
  #                 yshift    = -10,
  #                 font      = list(size=12)
# ) %>%
layout(
  barmode   = "stack",
  hovermode = "x unified",
  xaxis     = tick_yr_no_title,
  yaxis     = list(
    title       = "Average ASMA time [min/arr]",
    titlefont   = list(size = 11),
    hoverformat = ".2f"
  ),
  showlegend = TRUE,
  legend = list(
    orientation = "h",
    xanchor="center",
    x = 0.5
  ),
  updatemenus = list(button_type_list)
) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(
    ASMA_MM %>% 
      select(
        AIRPORT, 
        YEAR, 
        MONTH = MONTH_NUM,
        TOT_REF_TIME = TOT_UNIMP_TIME,
        TOT_ADD_TIME, 
        TOT_FLT,  
        TYPE, 
        AVG_ASMA_TIME = TIME), 
    "ASMA_MM")


if (!is.null(annotations[[length(filter_years)]])) {
  asma_mm_fig=asma_mm_fig %>% 
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

asma_mm_fig





# Factsheet figure

ASMA_MM_curr_year=filter(ASMA_MM, YEAR==max_year) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb))

ASMA_MM_fig = ggplot(data=filter(ASMA_MM_curr_year, YEAR==max_year)) +
  geom_bar(aes(x=Month, y=TIME, fill=TYPE), stat="identity", position = position_stack(reverse = TRUE)) +
  # scale_fill_manual(values=slot_col) +
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
  labs(x="", y="Average ASMA time (min/arr)\n")
ggsave(here("media", "factsheet", paste0("ASMA_Monthly_", params$icao, ".pdf")), plot=ASMA_MM_fig, 
       width = ASMA_layout1[2]*Page_width, height = ASMA_height1, units = "cm", dpi=100, limitsize = FALSE)

