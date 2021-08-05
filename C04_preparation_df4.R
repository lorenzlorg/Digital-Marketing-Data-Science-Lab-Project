# The dataset contains info on the privacy policies accepted by each customer:
# ID_CLI: identify client (foreign key)
# FLAG_PRIVACY_1: flag privacy (1/0)
# FLAG_PRIVACY_2: flag profiling (1/0)
# FLAG_DIRECT_MKT: flag direct marketing (1/0)


#### INSIGHTS ####
# 242251 (66%) clienti hanno accettato la flag privacy mentre 127221 (34%) no
# 345682 (94%) hanno accettato la flag profiling policy mentre 23790 (6%) no 
# 247790 (67%) clienti hanno accettato la flag direct marketing mentre 121682 (33%) no

#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarize(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

# ci sono un totale di 369472 valori unici per l'id cliente, pari al numero di osservazioni 
# del dataset. Dunque non si osservano duplicati

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarize(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

# compute distribution: FLAG_PRIVACY_1 (privacy) 

df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

# plot distribution :FLAG_PRIVACY_1

df_4_cli_privacy_clean %>% 
  count(FLAG_PRIVACY_1 = factor(FLAG_PRIVACY_1 )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = FLAG_PRIVACY_1, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("1" = "accepted", "0" = "not accepted"))+
  theme_classic()+
  labs(x = 'Flag privacy policy', y = 'Percentage')

# 242251 (66%) clienti hanno accettato la flag privacy mentre 127221 (34%) no

# compute distribution: FLAG_PRIVACY2 (profiling)

df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_2) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

#plot distribution

df_4_cli_privacy_clean %>% 
  count(FLAG_PRIVACY_2= factor(FLAG_PRIVACY_2 )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = FLAG_PRIVACY_2, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("1" = "accepted", "0" = "not accepted"))+
  theme_classic()+
  labs(x = 'Flag profiling policy', y = 'Percentage')

# 345682 (94%) hanno accettato la flag profiling policy mentre 23790 (6%) no 


# compute distribution: FLAG_DIRECT_MKT (direct marketing)

df_4_cli_privacy_clean %>%
  group_by(FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))%>%
  as.data.frame()

# plot distribution

df_4_cli_privacy_clean %>% 
  count(FLAG_DIRECT_MKT = factor(FLAG_DIRECT_MKT )) %>% 
  mutate(count = prop.table(n)) %>% 
  ggplot(aes(x = FLAG_DIRECT_MKT, y = count, label = scales::percent(count))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("1" = "accepted", "0" = "not accepted"))+
  theme_classic()+
  labs(x = 'Flag direct marketing privacy', y = 'Percentage')

# 247790 (67%) clienti hanno accettato la flag direct marketing mentre 121682 (33%) no

# si analizza in quali percentuali i clienti accentano una sola privacy policy, due, tre oppure nessuna

# compute distribution

num_flag_cli<-df_4_cli_privacy_clean %>%
  group_by(FLAG_PRIVACY_1,FLAG_PRIVACY_2,FLAG_DIRECT_MKT) %>%
  summarize(TOT_CLIs = n_distinct(ID_CLI), .groups="drop") %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))%>%
  as.data.frame()

num_flag_cli


# plot distribution

num_flag_cli$ACCEPTED<-as.factor(c(3,1,2,2,2,0,1,1)) 

num_flag_cli_data <- num_flag_cli[,c("PERCENT", "TOT_CLIs", "ACCEPTED")]


num_flag_cli_data <- num_flag_cli_data %>%       
  group_by(ACCEPTED) %>%                         
  summarise_at(vars(PERCENT, TOT_CLIs),          
               list(name = sum)) 


colnames(num_flag_cli_data) <- c( "ACCEPTED", "PERCENT_aggregate", "TOT_CLIs_aggregate")

num_flag_cli_data%>%
ggplot(aes(x = ACCEPTED, y = PERCENT_aggregate, label = scales::percent(PERCENT_aggregate))) +
  geom_col(position = 'dodge', fill="turquoise3", colour="turquoise3") +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, 
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("0" = "None", "1" = "Accepted One"
                            ,"2"="Accepted Two","3"="Accepted All"))+
  theme_classic()+
  labs(x = 'Flag policies accepted', y = 'Percentage')

# più della metà dei clienti (57%) ha accettato tutte le spunte


#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)
