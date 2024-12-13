
library(purrr)

TXIN_RWY_MM <- params$txinrwy_mm %>%
  tidyr::pivot_longer(
    cols      = c("AVG_REF_TIME", "AVG_ADD_TIME"),
    names_to  = "TYPE",
    values_to = "TIME"
  )

TXIN_RWY_MM <- TXIN_RWY_MM %>%
  mutate(
    TYPE = factor(TYPE,
                  levels = c("AVG_REF_TIME", "AVG_ADD_TIME"),
                  labels = c("Avg. Reference time", "Avg. Additional time")
    ))

ALL_YEAR <- TXIN_RWY_MM %>%
  filter(TIME != "NaN") %>%
  pull(YEAR) %>%
  unique()

ALL_RWY <- TXIN_RWY_MM %>%
  filter(TIME != "NaN") %>%
  arrange(RUNWAY) %>% 
  pull(RUNWAY) %>%
  unique()

filter_years <- TXIN_RWY_MM %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

TXIN_RWY_MM <- TXIN_RWY_MM %>%
  mutate(YEAR = factor(YEAR, levels =  ALL_YEAR),
         RUNWAY=factor(RUNWAY, levels = ALL_RWY)) %>%
  arrange(YEAR, RUNWAY)

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

subplot(
  map(seq(1, 12), function(.x){
    
    dat=TXIN_RWY_MM %>%
      filter(MONTH_NUM==.x) %>%
      group_by(TYPE) %>%
      arrange(YEAR, MONTH_NUM, RUNWAY)
    
    x_title <- dat$MONTH_NUM %>% unique() %>% month.abb[.]
    show_legend_once = ifelse(x_title == "Jan", TRUE, FALSE)
    
    plotly::plot_ly(data = dat,
                    x          = ~RUNWAY,
                    y          = ~TIME,
                    customdata = ~YEAR,
                    color      = ~TYPE,
                    type       = "bar",
                    legendgroup= ~TYPE,
                    showlegend = show_legend_once,
                    transforms = list(
                      list(
                        type      = "filter",
                        target    = "customdata",
                        operation = "=",
                        value     = max_year
                      ))
    ) %>%
      layout(
        # barmode   = "stack",
        hovermode = "x unified",
        xaxis     = list(
          title       = x_title,
          titlefont   = list(size = 11),
          hoverformat = ".2f"
        ),
        yaxis     = list(
          title       = "Average Taxi-in time [min/arr]",
          titlefont   = list(size = 11),
          hoverformat = ".2f"
        ),
        updatemenus = list(button_type_list)
      )
  }),
  titleX = TRUE,
  shareY = T) %>% 
  layout(barmode = 'stack', 
         showlegend = TRUE,
         legend = list(
           orientation = "h",
           xanchor="center",
           x = 0.5,
           y = 1
         )) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(
    TXIN_RWY_MM %>%
      select(
        AIRPORT, 
        YEAR, 
        MONTH = MONTH_NUM,
        RUNWAY,
        TOT_REF_TIME,
        TOT_ADD_TIME, 
        TOT_FLT,  
        TYPE, 
        AVG_TXIT_TIME = TIME) %>%
      filter(!is.na(RUNWAY)), 
    "TXIT_RWY_MM")
  








# Factsheet figure

# TXIN_MM_curr_year=filter(TXIN_MM, YEAR==max_year) %>% 
#   mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb))
# 
# TXIN_MM_fig = ggplot(data=filter(TXIN_MM_curr_year, YEAR==max_year)) +
#   geom_bar(aes(x=Month, y=TIME, fill=TYPE), stat="identity", position = position_stack(reverse = TRUE)) +
#   # scale_fill_manual(values=slot_col) +
#   theme_factsheet() +
#   # theme_bw() +
#   # theme(plot.title = element_blank(),
#   #       legend.title=element_blank(),
#   #       legend.text=element_text(size=36),
#   #       legend.position = "bottom",
#   #       axis.text=element_text(size=100),
#   #       axis.title=element_text(size=100),
#   #       axis.title.x = element_blank(),
#   #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
#   labs(x="", y="Average Taxi-in Time (min/arr)\n")
# ggsave(here("media", "factsheet", paste0("Taxi_In_Times_Monthly_", params$icao, ".png")), plot=TXIN_MM_fig, 
#        width = Taxi_in_times_layout1[2]*Page_width, height = Taxi_in_times_height1, units = "cm", dpi=100, limitsize = FALSE)
# 


