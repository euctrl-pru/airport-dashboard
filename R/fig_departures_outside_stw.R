
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

slot_col <- c(
  "#BDD7EE", # blue
  "#C6E0B4", # green
  "#F8CBAD"
) # orange





# Factsheet figure

Deps_outside_STW_fig = ggplot() +
  geom_rect(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_EARLY_1"), 
            aes(xmin=0, xmax=1, ymin=0, ymax=Share), fill=slot_col[1]) +
  geom_text(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_EARLY_1"), 
            aes(x=0.5, y=Share, vjust=0, label=paste0(round(Share*100, 1), "%")), 
            size=60, colour="black") +
  geom_text(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_EARLY_1"), 
            aes(x=0.5, y=Share/2, vjust=0, label=NB_SLOT), 
            size=60, colour="black") +
  geom_rect(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_LATE_1"), 
            aes(xmin=1, xmax=2, ymin=0, ymax=Share), fill=slot_col[3]) +
  geom_text(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_LATE_1"), 
            aes(x=1.5, y=Share, vjust=0, label=paste0(round(Share*100, 1), "%")), 
            size=60, colour="black") +
  geom_text(data=filter(Deps_outside_STW, SLOT_CAT=="TOT_FLT_DEP_OUT_LATE_1"), 
            aes(x=1.5, y=Share/2, vjust=0, label=NB_SLOT), 
            size=60, colour="black") +
  theme_factsheet() +
  theme_void()


ggsave(here("media", "factsheet", paste0("Departures_outside_STW_", Airport, ".pdf")), 
       plot=Deps_outside_STW_fig,width = ATFM_Slot_Adherence_layout1[3]*Page_width,
       height = ATFM_Slot_Adherence_height1, units = "cm", dpi=100, limitsize = FALSE)

