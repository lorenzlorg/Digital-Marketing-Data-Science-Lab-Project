# The data contains info about campaigns:
# ID_CAMP: identify email campaign (key)
# TYP_CAMP: type email campaign
# CHANNEL_CAMP: channel campaign (only email)

#### INSIGHTS ####
# Il canale utilizzato per la campagna è quello email
# Il tipo di campagna più utilizzato risulta essere "PRODUCT", seguito da "PERSONALIZED" e "NATIONAL"


#### FIRST LOOK of df_5 ####

str(df_5_camp_cat)
summary(df_5_camp_cat)
# in questo caso il canale della campagna è sempre EMAIL

#### START CLEANING df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat

#### CLEANING LOW VARIANCE in df_5 ####

df_5_camp_cat_clean <- df_5_camp_cat_clean %>%
  select(-CHANNEL_CAMP)

# essendo channel_camp = EMAIL per tutte le osservazioni si procede ad eliminare tale colonna

#### FINAL REVIEW df_5_clean ####

str(df_5_camp_cat_clean)
summary(df_5_camp_cat_clean)

# si procede nell'espolarazione di TYPE_CAMP

df_5_dist_type_camp <- df_5_camp_cat_clean %>%
  group_by(TYP_CAMP) %>%
  summarize(TOT_CAMPs = n_distinct(ID_CAMP)) %>%
  mutate(PERCENT = TOT_CAMPs/sum(TOT_CAMPs)) %>% 
  arrange(desc(PERCENT)) %>%
  as.data.frame()


plot_df5_dist_typ_camp <- (
  ggplot(data=df_5_dist_type_camp
         , aes(x=TYP_CAMP, y=TOT_CAMPs)) +
    geom_bar(stat="identity"
             , fill="turquoise3") +
    theme_classic() + 
    xlab("Type campaign") + ylab("Number of campaigns") + 
    geom_text(aes(label=TOT_CAMPs), position=position_dodge(width=0.9), vjust=-0.25))

plot_df5_dist_typ_camp

# il tipo di campagna più utilizzato risulta essere "PRODUCT"
