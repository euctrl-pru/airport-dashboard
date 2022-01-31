
Deps_outside_STW <- params$slot_yy %>%
  select(YEAR, TOT_FLT_DEP_OUT_LATE_1, TOT_FLT_DEP_OUT_EARLY_1, TOT_FLT_DEP_IN_1) %>%
  tidyr::pivot_longer(
    cols = c(TOT_FLT_DEP_OUT_LATE_1, TOT_FLT_DEP_OUT_EARLY_1, TOT_FLT_DEP_IN_1),
    names_to  = "SLOT_CAT",
    values_to = "NB_SLOT"
  ) %>% 
  filter(YEAR==Last_complete_year) %>% 
  mutate(Share=NB_SLOT/sum(NB_SLOT),
         SLOT_CAT=factor(SLOT_CAT,
                         levels = c(
                           "TOT_FLT_DEP_OUT_EARLY_1",
                           "TOT_FLT_DEP_IN_1",
                           "TOT_FLT_DEP_OUT_LATE_1"
                         )
         )) %>% 
  arrange(SLOT_CAT)

Deps_outside_STW_fig = ggplot(Deps_outside_STW) +
  geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=Share[1]), fill=slot_col[1]) +
  geom_text(aes(x=0.5, y=Share[1]/2, label=paste0(round(Share[1]*100, 1), "%")), 
            position = position_stack(vjust = 0.5), size=60, colour="black") +
  geom_rect(aes(xmin=1, xmax=2, ymin=0, ymax=Share[3]), fill=slot_col[3]) +
  geom_text(aes(x=1.5, y=Share[3]/2, label=paste0(round(Share[3]*100, 1), "%")), 
            position = position_stack(vjust = 0.5), size=60, colour="black") +
  theme_void()


ggsave(here("R", "Factsheet", "Figures", paste0("Departures_outside_STW_", Airport, ".png")), 
       plot=Deps_outside_STW_fig,width = ATFM_Slot_Adherence_layout1[3]*Page_width,
       height = ATFM_Slot_Adherence_height1, units = "cm", dpi=100, limitsize = FALSE)