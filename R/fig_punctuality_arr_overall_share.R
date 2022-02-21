
punc_arr_yy <- params$punc_arr_yy %>% 
  mutate(PUNCT_CAT=factor(PUNCT_CAT,
                          levels = c(
                            "[<-30]",
                            "[-16, -30]",
                            "[- 5, -15]",
                            "[- 4, + 4]",
                            "[+ 5, +15]",
                            "[+16, +30]",
                            "[+31, +60]",
                            "[>+60]"
                          )
  ))

filter_years <- punc_arr_yy %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)





# Factsheet figure
punc_arr_yy_share_curr_year=filter(punc_arr_yy, YEAR == max_year)
punc_arr_within_15_min=sum(filter(punc_arr_yy_share_curr_year, 
                                  PUNCT_CAT %in% c("[- 5, -15]", "[- 4, + 4]", "[+ 5, +15]"))$AVG_PER_CATEG)
punc_arr_OTP=sum(filter(punc_arr_yy_share_curr_year, 
                        PUNCT_CAT %in% c("[<-30]", "[-16, -30]", "[- 5, -15]", "[- 4, + 4]", "[+ 5, +15]"))$AVG_PER_CATEG)

punc_arr_yy_share_plot_fig = ggplot() +
  geom_rect(aes(xmin=0, xmax=1, ymin=1, ymax=2), colour='black', fill='lightblue') +
  geom_text(aes(x=0.5, y=1.5,
                label=paste0(round(punc_arr_within_15_min, 0), "%\nFlights arriving within +/- 15\n",
                             "minutes of their schedule")), 
            size=60, colour="black") +
  geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), colour='black', fill='lightblue') +
  geom_text(aes(x=0.5, y=0.5,
                label=paste0(round(punc_arr_OTP, 0), "%\nOn-Time Performance\n(flights arriving not later than\n",
                             "15 minutes from scheduled)")), 
            size=60, colour="black") +
  theme_factsheet() +
  theme_void()
ggsave(here("media", "factsheet", paste0("Punc_Arr_Overall_share_", params$icao, ".png")),
       plot=punc_arr_yy_share_plot_fig, width = Punctuality_layout2[3]*Page_width, height = Punctuality_height2, 
       units = "cm", dpi=100, limitsize = FALSE)

