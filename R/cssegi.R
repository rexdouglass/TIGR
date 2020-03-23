
library(tidyverse)
confirmed <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
deaths <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
recovered <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

confirmed_long <- pivot_longer(confirmed, names_to = "date", cols = ends_with("20"), values_to = "confirmed")
deaths_long <- pivot_longer(deaths, names_to = "date", cols = ends_with("20"), values_to = "deaths")
recovered_long <- pivot_longer(recovered, names_to = "date", cols = ends_with("20"), values_to = "recovered")

#library(tibbletime)
#mean_roll_5 <- rollify(mean, window = 3)

#mean_rolling <- function(x,window=5){
#  if(length(x)>=5){
#    try({
##      return(c(NA,NA,rollapplyr(x, window, mean), NA,NA))
#    })
#  }
#  return(rep(NA,length(x)))
#}


library(lubridate)

all_long <- deaths_long %>% 
  full_join(confirmed_long) %>% 
  full_join(recovered_long) %>%
  filter(confirmed>0) %>% 
  
  mutate(date_asdate = mdy(str_replace(date,"20$","2020"))) %>% 
  rename(country=`Country/Region`, state=`Province/State`) %>%
  #filter(!state %in% c("From Diamond Princess","Diamond Princess")) %>%
  mutate(state=ifelse(is.na(state),"none",state)) %>%   
  mutate(country_state=paste0(country, "___", state) %>% as.factor()  ) %>%
  
  arrange(country_state, date_asdate) %>%
  filter(confirmed>0) %>% #subset to only places/time periods that have had confirmed
  group_by(country_state) %>%
  mutate(days_since_1_confirmed=cumsum(confirmed>0)) %>%
  #mutate(days_since_1_confirmed_log= log(days_since_1_confirmed)) %>%
  
  mutate(confirmed_fd=confirmed-lag(confirmed)) %>%
  #mutate(confirmed_fd_fd=confirmed_fd-lag(confirmed_fd)) %>%
  
  #mutate(confirmed_pf= confirmed/lag(confirmed)) %>%

  #mutate(confirmed_mean_window=mean_rolling(confirmed)   ) %>%
  #mutate(confirmed_mean_window_fd=confirmed_mean_window-lag(confirmed_mean_window)) %>%
  #mutate(confirmed_mean_window_pd= (confirmed_mean_window/lag(confirmed_mean_window)-1)) %>%
  
  #mutate(confirmed_log=log(confirmed) ) %>%
  #mutate(confirmed_log_mean_window=mean_rolling(confirmed_log)   ) %>%
  #mutate(confirmed_log_mean_window_fd= confirmed_log_mean_window-lag(confirmed_log_mean_window) ) %>%
  #mutate(confirmed_log_mean_window_pd= (confirmed_log_mean_window/lag(confirmed_log_mean_window))-1) %>%
  ungroup() %>%
  filter(days_since_1_confirmed>0) 
    
padded <- all_long %>%
          tidyr::expand(country_state, days_since_1_confirmed) %>% 
          separate(col=country_state, into=c('country','state'), sep = "___", remove = T) 

all_long <- padded %>% 
            left_join(all_long) %>% 
            arrange(country, state, days_since_1_confirmed) %>%
            mutate(country_state=paste0(country, "___", state) %>% as.factor()  )

fromscratch=F
if(fromscratch){
  #We need to grab populations
  places <- all_long %>% dplyr::select(country,state) %>% distinct() %>% mutate(search=paste0(state,", ", country ))  %>% mutate(search= ifelse(is.na(state) | state=="none", country, state ) )
  library(WikidataR)
  wiki_searches <- list()
  for(q in places$search){
    print(q)
    try({
      #wiki <- find_item("Ramsey County")
      wiki <- find_item(q)
      temp <- as.data.frame(wiki[[1]])
      temp$search <- q
      wiki_searches[[q]] <- temp
    })
  }
  wiki_searches_df <- bind_rows(wiki_searches)
  
  item_df <- list()
  for(q in wiki_searches_df$id){
    print(q)
    try({
      item_df[[q]] <- get_item(id = q)
    })
  }
  wiki <- find_item("New York")
  
  latest_pop_list <- list()
  for(q in names(item_df)){
    print(q)
    try({
      latest_pop_list[[q]] <- data.frame(wikidata_id=q, P1082= rev(item_df[[q]][[1]]$claims$P1082$mainsnak$datavalue$value$amount)[1])
    })
  }
  latest_pop_df <- places %>% left_join(wiki_searches_df %>% dplyr::select(search,wikidata_id=id), by=c('search'='search')) %>% left_join( bind_rows(latest_pop_list) ) %>% 
    mutate(population= str_replace(P1082,"\\+","") %>% as.numeric()    ) %>%
    mutate(population_log= log(population)) %>%
    mutate(state=ifelse(is.na(state),"none",state)) %>% distinct() %>% arrange(country, state) 
  
  saveRDS(latest_pop_df,"/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/data_temp/latest_pop_df.RDS")

}

latest_pop_df <- readRDS("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/data_temp/latest_pop_df.RDS")

all_long_covariates <- all_long %>% 
                       left_join(latest_pop_df) %>% 
                       arrange(country, state, days_since_1_confirmed) 

locations <- all_long_covariates$country_state
r_list <- list()
for(q in locations %>% unique() ){
  print(q)
  try({
    temp <- all_long_covariates %>% filter(country_state %in% q) %>% filter(!is.na(confirmed))
    temp$confirmed_fd[1]<-temp$confirmed[1]
    library(R0)
    mGT<-generation.time("gamma", c(7.5, 3.4)) #mean and sd of the disease?
    incidents <- as.vector(temp$confirmed_fd )
    pop <- temp$population[1]
    estR0<-estimate.R(incidents, 
                      mGT, 
                      #begin=1,
                      #end=length(incidents), 
                      methods="ML", #c("EG", "ML", "TD", "AR", "SB"),
                      pop.size=pop,
                      nsim=100)
    attributes(estR0)
    estR0
    r_list[[q]] <- data.frame(country_state=q, r_ml=estR0$estimates$ML$R, pop=pop)
  })
}
r_df <- bind_rows(r_list)
r_df %>% filter(!is.na(pop)) %>% pull(r_ml) %>% summary()
r_df %>% filter(!is.na(pop)) %>% ggplot(aes(x=r_ml)) + geom_density()



p_italy <- all_long %>% ggplot()  %>% 
  filter(country_state %in% "China___Hubei" ) %>% + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  facet_wrap(~ country + state ) + theme_bw()
p_italy
#install.packages('SDSFoundations')

p_Hubei <- all_long %>%  filter(confirmed>25) %>% 
  filter(country_state %in% "China___Hubei" ) %>%
  ggplot() +
  geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + facet_wrap(~country_state, scales="free") + theme_bw()
p_Hubei

temp <- all_long_covariates %>% filter(country_state %in% "China___Hubei") %>% filter(!is.na(confirmed))
temp$confirmed_fd[1]<-temp$confirmed[1]
library(R0)
mGT<-generation.time("gamma", c(7.5, 3.4)) #mean and sd of the disease?
incidents <- as.vector(temp$confirmed_fd )
pop <- temp$population[1]
estR0<-estimate.R(incidents, 
                  mGT, 
                  #begin=1,
                  #end=length(incidents), 
                  methods=c("EG", "ML", "TD", "AR", "SB"), #"ML",
                  pop.size=pop,
                  nsim=100)
attributes(estR0)
estR0


p_us <- all_long %>%  filter(confirmed>25) %>% 
  filter(country %in% "US" ) %>%
  ggplot() +
  geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + facet_wrap(~country_state, scales="free") + theme_bw()
p_us

p_notus <- all_long %>%  filter(confirmed>25) %>% 
  filter(!country %in% "US" ) %>%
  ggplot() +
  geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + facet_wrap(~country_state, scales="free") + theme_bw()
p_notus


all_long_italy <- all_long %>%  filter(days_since_1_confirmed>0) %>%   filter(country %in% "Italy" & state %in% "none")

#Early R0 is failing me
#i <- all_long_italy %>% select(days_since_1_confirmed,confirmed) %>% na.omit() %>% group_by(days_since_1_confirmed) %>% mutate(all= list(rep(days_since_1_confirmed,confirmed))) %>% pull(all) %>% unlist() %>%
#     incidence()
#plot(i, border = "white")
#mu <- 7.5  # https://www.ijidonline.com/article/S1201-9712(20)30091-6/fulltext, Li et al., 2020
#sigma <- 3.4 # standard deviation in days
#library(earlyR)
#res <- get_R(i, si_mean = mu, si_sd = sigma)
#plot(res)

library(R0)
mGT<-generation.time("gamma", c(7.5, 3.4)) #mean and sd of the disease?
incidents <- as.vector(all_long_italy$confirmed_fd %>% na.omit())
estR0<-estimate.R(incidents, 
                  mGT, 
                  begin=1,
                  #end=length(incidents), 
                  methods=c("EG", "ML", "TD", "AR", "SB"),
                  pop.size=60480000,
                  nsim=100)
attributes(estR0)
estR0

all_long_Hubei <- all_long_covariates %>%  filter(days_since_1_confirmed>0) %>%   filter(country %in% "China" & state %in% "Hubei") 
mGT<-generation.time("gamma", c(7.5, 3.4)) #mean and sd of the disease?
incidents <- as.vector(all_long_Hubei$confirmed_fd %>% na.omit())
pop <- all_long_Hubei$population[1]
estR0<-estimate.R(incidents[1:60], 
                  mGT, 
                  #begin=2,
                  #end=10, 
                  #methods=c("EG", "ML", "TD", "AR", "SB"),
                  methods=c( "ML", "TD" ),
                  pop.size=pop,
                  nsim=100)
attributes(estR0)
estR0




#https://rpubs.com/jaelison/200149
#Linear fit
lm_italy <- lm(confirmed ~ days_since_1_confirmed, data=all_long_italy)
all_long_italy$y_hat_lm <- predict(lm_italy, all_long_italy)
#exponentialy fit
exp_italy = lm(log(confirmed) ~ days_since_1_confirmed, data=all_long_italy)
all_long_italy$y_hat_exp <- exp(predict(exp_italy, all_long_italy))
#logistic fit
#logit(mass/100)~days.since.birth
library(car)
c <- 100000 * 0.75
logistic_italy = lm(car::logit(confirmed/c) ~ days_since_1_confirmed, data=all_long_italy)
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
all_long_italy$y_hat_logistic <- logit2prob(predict(logistic_italy, all_long_italy))*c
logisticModel <- nls(population~K/(1+exp(Po+r*time)), start=list(Po=0, r=-0.211, K=30), data=all_long_italy)
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_italy)
summary(logisticModelSS)
coef(logisticModelSS)
plot(logisticModelSS)

#install.packages("earlyR")
library(earlyR)


library(nlsr) #install.packages("nlsr")
nlsrModelSS <- nlxb(population~K/(1+exp(Po+r*time)), start=c(population=100000, Po=25, r=-1, K=30) , data=all_long_italy )
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_italy , control = nls.control(minFactor = 1/4096) )
all_long_italy$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_italy)
p_italy <- all_long_italy %>% ggplot() + 
          geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
          geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
          facet_wrap(~ country + state ) + theme_bw()
p_italy  #+ scale_y_log10()

all_long_Hubei <- all_long %>%  filter(days_since_1_confirmed>0) %>%   filter(country %in% "China" & state %in% "Hubei")
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_Hubei , control = nls.control(minFactor = 1/4096) )
all_long_Hubei$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_Hubei)
p_Hubei <- all_long_Hubei %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
  facet_wrap(~ country + state ) + theme_bw()
p_Hubei  

all_long_texas <- all_long %>%filter(days_since_1_confirmed>0) %>% filter(country %in% "US" & state %in% "Texas")
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_texas , control = nls.control(minFactor = 1/4096) )
all_long_texas$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_texas)
p_texas <- all_long_texas %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
  facet_wrap(~ country + state ) + theme_bw()
p_texas

all_long_newyork <- all_long %>%filter(days_since_1_confirmed>0) %>% filter(country %in% "US" & state %in% "New York")
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_newyork , control = nls.control(minFactor = 1/4096) )
all_long_newyork$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_newyork)
p_newyork <- all_long_newyork %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
  facet_wrap(~ country + state ) + theme_bw()
p_newyork


all_long_california <- all_long %>%filter(days_since_1_confirmed>0) %>% filter(country %in% "US" & state %in% "California")
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_california , control = nls.control(minFactor = 1/4096) )
all_long_california$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_california)
p_california <- all_long_california %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
  facet_wrap(~ country + state ) + theme_bw()
p_california


all_long_florida <- all_long %>%filter(days_since_1_confirmed>0) %>% filter(country %in% "US" & state %in% "Florida")
logisticModelSS <- nls(confirmed~SSlogis(days_since_1_confirmed, Asym, xmid, scal), data=all_long_florida , control = nls.control(minFactor = 1/4096) )
all_long_florida$y_hat_logisticModelSS <- predict(logisticModelSS, all_long_florida)
p_florida <- all_long_florida %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_logisticModelSS), col="pink") +
  facet_wrap(~ country + state ) + theme_bw()
p_florida + xlim(0,20)


install.packages('R0')


all_long$days_since_1_confirmed_2 <- all_long$days_since_1_confirmed^2
all_long$days_since_1_confirmed_3 <- all_long$days_since_1_confirmed^3
#Won't work #Error: no valid set of coefficients has been found: please supply starting values
library(MASS)
lm1 <- glm.nb(confirmed ~
                     country_state + 
                     days_since_1_confirmed +
                     days_since_1_confirmed_2 +
                     #days_since_1_confirmed_3 +
                     country_state:days_since_1_confirmed +
                     country_state:days_since_1_confirmed_2 #+
                     #country_state:days_since_1_confirmed_3 
            ,
            data = all_long %>% filter(country %in% "US" ),
          na.action = na.exclude 
)

predictions <- all_long %>% dplyr::select(country_state, country, state,days_since_1_confirmed, days_since_1_confirmed_2, days_since_1_confirmed_3,confirmed ) %>% distinct() %>% filter(country_state %in% lm1$xlevels$country_state)
predictions$y_hat_lm1 <- round(predict(lm1, newdata=predictions, type="response" ))
predictions$y_hat_lm1[!is.finite(predictions$y_hat_lm1)] <- NA
p_us <- predictions %>%  filter(y_hat_lm1>0) %>% 
          filter(country %in% "US" ) %>%
          ggplot() +
          geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + 
          geom_line(aes(x=days_since_1_confirmed,y=y_hat_lm1), color="blue") + facet_wrap(~country_state, scales="free_y") + #
          theme_bw()
p_us

temp <- all_long %>%  filter(confirmed>25) %>%   filter(country %in% "US" )



#replace na with zero
temp <- all_long %>% filter(country %in% "Italy" & state %in% "none") %>% select(country,state,days_since_1_confirmed,confirmed,confirmed_fd)
lm1 <- lm(confirmed_fd~days_since_1_confirmed, data=temp)

p_all <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_pd)) + facet_wrap(~country_state) + theme_bw()
p_all

p_all <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd)) + facet_wrap(~country_state, scales="free") + theme_bw()
p_all


p_all <- all_long %>% filter(confirmed>0, days_since_1_confirmed>=1) %>% ggplot() + 
         geom_point(aes(x=days_since_1_confirmed,y=confirmed_log)) +
         geom_line(aes(x=days_since_1_confirmed,y=confirmed_log_mean_window), color="red") +
         facet_wrap(~country_state) + theme_bw()
p_all

xy_all <- all_long %>% 
          select(confirmed_mean_window_fd, country, state, days_since_1_confirmed, population_log) %>%  
          filter(is.finite(confirmed_mean_window_fd), days_since_1_confirmed>=1, is.finite(population_log)) %>% 
          distinct() %>% 
          group_by(country, state) %>%
          mutate(sample_weight=n()) %>%
          ungroup() %>%
          filter(!is.na(confirmed_mean_window_fd)) %>% #killing off any obs without confirmed
          mutate(random = runif(n() )) %>% # we have to assign upsample to here so that we don't accidentally stick upsampled into both training and test sets
          mutate(confirmed_mean_window_fd=log(confirmed_mean_window_fd+1)) #log the fd

xy_all$sample_weight <- round(1/ (xy_all$sample_weight/max(xy_all$sample_weight)))

p_all <- xy_all %>% mutate(country_state=paste(country, state)) %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd)) + facet_wrap(~country_state, scales="free") + theme_bw()
p_all

xy_all_for_prediction <- xy_all %>%
                          mutate(country_state=paste0(country, "___", state)) %>% 
                          tidyr::expand(country_state, days_since_1_confirmed) %>% 
                          separate(col=country_state, into=c('country','state'), sep = "___", remove = TRUE) %>% 
                          left_join(xy_all) %>%
                          fastDummies::dummy_cols(remove_selected_columns=F,select_columns = c("country","state"), ignore_na =F)

x_predict <-  xy_all_for_prediction %>% 
              select(-country, -state, -confirmed_mean_window_fd, -sample_weight, -random ) %>%  data.matrix()
x_predict[!is.finite(x_predict)]<-0

xy_all_for_prediction_upsampled <- xy_all_for_prediction %>% 
                                   filter(!is.na(sample_weight)) %>%
                                   mutate(obs=1:n())
dim(xy_all_for_prediction_upsampled)
ids <- unlist(sapply(xy_all_for_prediction_upsampled$obs, FUN=function(x) rep(x,xy_all_for_prediction_upsampled$sample_weight[x])))
xy_all_for_prediction_upsampled <- xy_all_for_prediction_upsampled[ids,]
dim(xy_all_for_prediction_upsampled) #[1] 7223    6

x_train <- xy_all_for_prediction_upsampled %>% filter(random<.9) %>% select(-country, -state, -confirmed_mean_window_fd,  -obs, -random,-sample_weight) %>% data.matrix()
x_test <- xy_all_for_prediction_upsampled %>% filter(random>=.9) %>% select(-country, -state, -confirmed_mean_window_fd,  -obs, -random,-sample_weight) %>% data.matrix()
x_train[!is.finite(x_train)]<-0
x_test[!is.finite(x_test)]<-0

y_train <- xy_all_for_prediction_upsampled %>% filter(random<.9) %>% select(confirmed_mean_window_fd) %>% data.matrix()
y_test <- xy_all_for_prediction_upsampled %>% filter(random>=.9) %>% select(confirmed_mean_window_fd) %>% data.matrix()
y_train[!is.finite(y_train)]<-0
y_test[!is.finite(y_test)]<-0

dim(x_train)
dim(y_train)

table(colnames(x_predict)==colnames(x_train))


library(tictoc)
library(tfprobability)
library(keras)
library(tidyverse)

l2_rate= .000001 #.1^10 #.1^1 #don't use an l2 also, it shrinks the normals to 0 which is bad, and doesn't help generalization. We're already inserting noise
library(keras)
input_shape <- ncol(x_train)
encoded_shape <- ncol(y_train)
model <- keras_model_sequential(
  list(
    layer_input(shape = input_shape),
    layer_batch_normalization(), #batch norm on the inputs drastically reduced the initial loss
    #layer_dropout(rate=.5), #dropout on the inputs is garbage
    layer_dense(units = 512, activity_regularizer=regularizer_l1_l2(l1 = 0, l2 = l2_rate)),
    layer_batch_normalization(),
    layer_activation_relu(),
    layer_dropout(rate=.5),
    layer_dense(units = 256, activity_regularizer=regularizer_l1_l2(l1 = 0, l2 = l2_rate)),
    layer_batch_normalization(),
    layer_activation_relu(),
    layer_dropout(rate=.5),
    #layer_dense(units = 32, activity_regularizer=regularizer_l1_l2(l1 = 0, l2 = l2_rate)),
    #layer_batch_normalization(),
    #layer_activation_relu(),
    #layer_dense(units = 1, activity_regularizer=regularizer_l1_l2(l1 = 0, l2 = l2_rate))
    layer_dense(units = params_size_independent_normal(encoded_shape), activity_regularizer=regularizer_l1_l2(l1 = 0, l2 = l2_rate)),
    layer_independent_normal(event_shape = encoded_shape)
  )
)

model %>% compile(
  optimizer = "adam", #rmsprop works much better than adam
  loss = loss_mean_squared_error, #, #'binary_crossentropy',#,
  metrics=metric_mean_squared_error
  #metrics=list(rex_f1,rex_accuracy, rex_recall, rex_precision) #rex_accuracy,rex_recall,rex_precision, #rex_cross_entropy #,rex_recall  #rex_f1
  #metrics=list(rex_accuracy, rex_precision, rex_recall, rex_f1) #rex_accuracy,rex_recall,rex_precision, #rex_cross_entropy #,rex_recall  #rex_f1
)

callbacks <- list(
  callback_tensorboard(paste0("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/logs/",Sys.time() )) ,
  #callback_csv_logger(filename=paste0(here::here(),'/logs/callback.txt'),separator = ",", append = T),
  callback_early_stopping(monitor = 'val_loss', patience = 100, restore_best_weights = T, mode="min") #max
)
tic()
model %>% fit(
  #Ok I still can't get sample weights to work. Upsample the data.
  #sample_weight=list(sample_weight), #downsample so each sentence is weighted equally no matter how many codings it received
  x=list( x_train   ),  #id_wide_train condition_train
  y=list( y_train   ),  #condition_train
  shuffle = TRUE,
  epochs = 1000,
  batch_size =  128, #still need high batchsize to learn condition
  validation_data = list(
    list( x_test   ) , #id_wide_test #condition_test
    list( y_test   ) #   condition_test
  ),
  callbacks = callbacks,
  workers=1, #Error: You may not specify workers > 1 for R based generator functions (R generators must run on the main thread)
  view_metrics=F,
  verbose=T #turned verbose off because I'm awtching it in tensorboard
)
toc()

samples <- lapply(1:100, 
                  FUN=function(x) predict(model,x_predict)[,1] ) #draw 100 samples , actually takes a little while
length(samples)
length(samples[[1]])
df <- t(as.data.frame(samples)) %>% as.data.frame()
dim(df)
samples_05 <- sapply(df, FUN=function(x) quantile(x, c(.05) )) 
samples_mean <- sapply(df, FUN=function(x) quantile(x, c(.5) )) 
samples_95 <- sapply(df, FUN=function(x) quantile(x, c(.95) )) 

xy_all_for_prediction$y_hat_nn_05 <- samples_05
xy_all_for_prediction$y_hat_nn_50 <- samples_mean
xy_all_for_prediction$y_hat_nn_95 <- samples_95

xy_all_for_prediction %>% ggplot() + geom_point(aes(x=confirmed_mean_window_fd,y=y_hat_nn_50))

p_italy <- xy_all_for_prediction %>%  
            filter(country %in% "Italy" & state %in% "none") %>% ggplot() + 
            geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd )) +
  
            geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_05  ), color="red", linetype = "dashed") +
            geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_50  ), color="blue") +
            geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_95 ), color="red", linetype = "dashed") +
  
            facet_wrap(~ country + state ) + theme_bw()
p_italy

p_texas <- xy_all_for_prediction %>%  
  filter(country %in% "US" & state %in% "Texas") %>% ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd  )) +
  
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_05 ), color="red", linetype = "dashed") +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_50 ), color="blue") +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_95), color="red", linetype = "dashed") +
  
  facet_wrap(~ country + state ) + theme_bw()
p_texas

p_all <- xy_all_for_prediction %>%  ggplot() + 
        geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd)) +
        geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_50), color="blue") +
        facet_wrap(~ country + state ) + theme_bw()
p_all


p_us <- xy_all_for_prediction %>% dplyr::filter(country %in% "US") %>%
  ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd %>%exp()  )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_50 %>%exp()), color="blue") +
  facet_wrap(~ country + state, scales="free" ) + theme_bw()
p_us #+ scale_y_log10() 

#Ok so everything looks good in fit, but if you include population the projections look really strange

p_not_us <- xy_all_for_prediction %>% dplyr::filter(!country %in% "US") %>%
  ggplot() + 
  geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_fd %>%exp()  )) +
  geom_line(aes(x=days_since_1_confirmed,y=y_hat_nn_50 %>%exp()), color="blue") +
  facet_wrap(~ country + state, scales="free" ) + theme_bw()
p_not_us #+ scale_y_log10() 



library(lme4)
library(arm)
all_long <- all_long %>% mutate(days_since_1_confirmed_2=days_since_1_confirmed^2)
all_long <- all_long %>% mutate(days_since_1_confirmed_3=days_since_1_confirmed^3)

lm1 <- lm(confirmed_mean_window_pd ~ country_state +
                                     days_since_1_confirmed + 
                                     days_since_1_confirmed_2 +
                                     #days_since_1_confirmed_3 +
                                     days_since_1_confirmed:country_state +
                                     days_since_1_confirmed_2:country_state #+
                                     #days_since_1_confirmed_3:country_state
                                     ,
                                     data=all_long , na.action = na.exclude)
display(lm1)
all_long$lm1 <- predict(lm1, data=all_long, type="response")

p_all <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() +
         geom_point(aes(x=days_since_1_confirmed,y=confirmed_mean_window_pd)) + 
         geom_line(aes(x=days_since_1_confirmed, y=lm1, color="red"))  + facet_wrap(~country_state) + theme_bw()
p_all


library(relgam)
set.seed(1)
n <- 100; p <- 12
x = matrix(rnorm((n) * p), ncol = p)
f4 = 2 * x[,4]^2 + 4 * x[,4] - 2
f5 = -2 * x[, 5]^2 + 2
f6 = 0.5 * x[, 6]^3
mu = rowSums(x[, 1:3]) + f4 + f5 + f6
y = mu + sqrt(var(mu) / 4) * rnorm(n)
fit <- rgam(x, y, verbose = FALSE)
par(mfrow = c(1, 4))
par(mar = c(4, 2, 2, 2))
plot(fit, x)

require(mgcv)
require(nlme)
b0 <- lme(travel~1,data=Rail,~1|Rail,method="REML") 
b <- gam(travel~s(Rail,bs="re"),data=Rail,method="REML")

xy_all <- all_long %>% 
  filter(!is.na(confirmed_mean_window_pd)) %>%
  filter(days_since_1_confirmed>1) 
  
b1 <- gam(confirmed_mean_window_pd~s(country_state,bs="re"),data=xy_all,method="REML") #random intercept by location

#https://stackoverflow.com/questions/25872488/how-to-add-a-random-intercept-and-random-slope-term-to-a-gamm-model-in-r
library(gamm4)
library(nlme)
library(mgcv)
data(sleepstudy,package='lme4')
# Model via lme()
fm1 <- lme(Reaction ~ Days, random= ~1+Days|Subject, data=sleepstudy, method='REML')
# Model via gamm()
#fm1.gamm <- gamm(Reaction ~ Days, random= list(Subject=~1+Days), data=sleepstudy, method='REML')

b2 <- gamm(confirmed_mean_window_pd ~ days_since_1_confirmed, random= list(country_state=~1+days_since_1_confirmed), data=xy_all, method='REML') #ok this is a linear model with a re slope and intercept
summary(b2)
summary(b2$gam)
xy_all$y_hat_b2 <- predict(b2$gam, xy_all)

p1 <- xy_all %>% 
      filter(!is.na(confirmed_mean_window_pd)) %>%
      ggplot() + 
      geom_point(aes(x=days_since_1_confirmed, y=confirmed_mean_window_pd)) +
      geom_line(aes(x=days_since_1_confirmed, y=y_hat_b1), color="blue") + 
      facet_wrap(~country_state, scales="free") + 
      guides(color = FALSE)   
p1  


b3 <- gamm(confirmed_mean_window_pd ~ s(days_since_1_confirmed), random= list(country_state=~1+days_since_1_confirmed), data=xy_all, method='REML')  #ok this is a smooth model with a re slope and intercept
summary(b3)
summary(b3$gam)
xy_all$y_hat_b3 <- predict(b3$gam, xy_all)

p2 <- xy_all %>% 
      filter(!is.na(confirmed_mean_window_pd)) %>%
      ggplot() + 
      geom_point(aes(x=days_since_1_confirmed, y=confirmed_mean_window_pd)) +
      geom_line(aes(x=days_since_1_confirmed, y=y_hat_b3), color="blue") + 
      facet_wrap(~country_state, scales="free") + 
      guides(color = FALSE)   
p2  


b4 <- gamm(confirmed_mean_window_pd ~ s(country_state, bs=c('re')) + 
              s(days_since_1_confirmed,country_state, bs=c('ps','re')),
           data=xy_all, method='REML')  #ok this is a smooth model with a re slope and intercept

#https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/
#https://stat.ethz.ch/R-manual/R-patched/library/mgcv/html/linear.functional.terms.html
b4 <- gamm(confirmed_mean_window_pd ~  country_state + s(days_since_1_confirmed, by = country_state,bs="re"), #taking a long time to run
           data=xy_all, method='REML')  #ok this is a smooth model with a re slope and intercept

summary(b4)
summary(b4$gam)
xy_all$y_hat_b4 <- predict(b4$gam, xy_all)
library(Metrics)
rmse(xy_all$y_hat_b4, xy_all$confirmed_mean_window_pd)

p2 <- xy_all %>% 
  filter(!is.na(confirmed_mean_window_pd)) %>%
  ggplot() + 
  geom_point(aes(x=days_since_1_confirmed, y=confirmed_mean_window_pd)) +
  geom_line(aes(x=days_since_1_confirmed, y=y_hat_b4), color="blue") + 
  facet_wrap(~country_state, scales="free") + 
  guides(color = FALSE)   
p2  

install.packages('glinternet')
library(glinternet)
# gaussian response, continuous features
Y = rnorm(100)
X = matrix(rnorm(100*10), nrow=100)
numLevels = rep(1, 10)
fit = glinternet(X, Y, numLevels)
#binary response, continuous features
Y = rbinom(100, 1, 0.5)
fit = glinternet(X, Y, numLevels, family="binomial")
#binary response, categorical variables
X = matrix(sample(0:2, 100*10, replace=TRUE), nrow=100)
numLevels = rep(3, 10)
fit = glinternet(X, Y, numLevels, family="binomial")

xy_all <- all_long %>% 
  filter(!is.na(confirmed_mean_window_pd) & confirmed_mean_window_pd>0 ) %>%
  filter(days_since_1_confirmed>1) 
x= xy_all %>% dplyr::select(days_since_1_confirmed, country_state) %>% mutate(country_state=as.numeric(factor(country_state))-1) %>%data.matrix()
numlevels=c(1,max(x[,2]+1))
y= xy_all %>% dplyr::select(confirmed_mean_window_pd) %>%   data.matrix()

fit = glinternet(x, y, numLevels=numlevels, family="gaussian")




xy_all <- all_long %>% 
           filter(!is.na(confirmed_mean_window_pd)) %>% 
           group_by()
           dplyr::select(days_since_1_confirmed,country_state,confirmed_mean_window_pd) %>% distinct() %>%
           fastDummies::dummy_cols(remove_selected_columns=T,select_columns = c("country_state")) %>% distinct()
x= xy_all %>% dplyr::select(-confirmed_mean_window_pd) %>% data.matrix()
y= xy_all %>% dplyr::select(confirmed_mean_window_pd) %>%   data.matrix()
fit <- rgam(x, y, verbose = T)

library(mgcv)

data("tobin", package = "survival")
install.packages('truncreg')
library(truncreg)
tm1 <- truncreg(confirmed_mean_window_pd ~ country_state +
                  days_since_1_confirmed + 
                  days_since_1_confirmed_2 +
                  #days_since_1_confirmed_3 +
                  days_since_1_confirmed:country_state +
                  days_since_1_confirmed_2:country_state #+
                #days_since_1_confirmed_3:country_state
                ,
                data=all_long %>% dplyr::filter(confirmed_mean_window_pd>0) %>% dplyr::select(confirmed_mean_window_pd, country_state, days_since_1_confirmed, days_since_1_confirmed_2) %>% as.data.frame(), 
                 na.action = na.exclude,
                point = 0, direction = "left")


p1 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_log, color=country_state)) + guides(color = FALSE) 
p1 + geom_abline(slope = 1)

p1 <- all_long %>% ggplot() + geom_line(aes(x=date_asdate, y=confirmed, color=country_state)) + guides(color = FALSE) 
p1 + geom_abline(slope = 1)
#p1 + scale_y_log10() 

p1 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed, color=country_state)) + guides(color = FALSE) 
p1  + geom_abline(slope = 1000)

p1 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed, color=country_state)) + guides(color = FALSE) 
p1 + geom_abline(slope = 4) + scale_y_log10() + scale_x_log10() 

install.packages("genlogis")

p2 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_fd, color=country_state)) + guides(color = FALSE) 
p2 

p3 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_log_mean3, color=country_state)) + guides(color = FALSE) 
p3 + ylim(3,12) + scale_x_log10() 


p3 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_pd, color=country_state)) + guides(color = FALSE) 
p3 + ylim(1,5)


p5 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_log_mean3_fd, color=country_state)) + guides(color = FALSE) 
p5 

p6 <- all_long %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_log_mean3_pd, color=country_state)) + guides(color = FALSE) 
p6 + ylim(1,1.5) + scale_x_log10()



p1 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed, color=country_state)) + guides(color = FALSE) 
p1 + scale_y_log10() + scale_x_log10() 

temp <- all_long %>% filter(days_since_1_confirmed>=1)
p2 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed, color=country_state)) + guides(color = FALSE) 
p2

p2a <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed, color=country_state)) + guides(color = FALSE) = 
p2a


p3 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=deaths, color=country_state)) + guides(color = FALSE) 
p3


p4 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_fd, color=country_state)) + guides(color = FALSE) 
p4

p4 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot() + geom_line(aes(x=days_since_1_confirmed, y=confirmed_pd, color=country_state)) + guides(color = FALSE) 
p4


p5 <- all_long %>% filter(days_since_1_confirmed>=1) %>% ggplot(aes(x=days_since_1_confirmed, y=confirmed_fd, color=country_state)) + geom_smooth(se = F) + guides(color = FALSE) 
p5

all_long_100 <- all_long %>% filter(days_since_1_confirmed>=1)

library(lme4)
library(arm)
lm1 <- glm(confirmed ~ days_since_1_confirmed + country_state +  days_since_1_confirmed:country_state, data=all_long_100 )
display(lm1)
all_long_100$y_hat_lm1 <- round(predict(lm1, all_long_100))
all_long_100$e_hat_lm1 <- all_long_100$y_hat_lm1-all_long_100$confirmed

all_long_100$days_since_1_confirmed_2 <- all_long_100$days_since_1_confirmed^2
lm2 <- glm(confirmed ~ country_state + days_since_1_confirmed + days_since_1_confirmed_2 +  days_since_1_confirmed:country_state +  days_since_1_confirmed_2:country_state, data=all_long_100 )
display(lm2)
all_long_100$y_hat_lm2 <- round(predict(lm2, all_long_100))
all_long_100$e_hat_lm2 <- all_long_100$y_hat_lm2-all_long_100$confirmed

all_long_100$days_since_1_confirmed_3 <- all_long_100$days_since_1_confirmed^3
lm3 <- glm(confirmed ~ country_state + 
                        days_since_1_confirmed +
                        days_since_1_confirmed_2 + 
                        days_since_1_confirmed_3 + 
                        days_since_1_confirmed:country_state +  
                        days_since_1_confirmed_2:country_state + 
                        days_since_1_confirmed_3:country_state,
               data=all_long_100 )
display(lm3)
all_long_100$y_hat_lm3 <- round(predict(lm3, all_long_100))
all_long_100$e_hat_lm3 <- all_long_100$y_hat_lm3-all_long_100$confirmed

#glm1 <- glmer.nb(confirmed ~ country_state + (1+days_since_1_confirmed|country_state), data=all_long_100 ) #actually does take a while
#display(glm1)
#all_long_100$y_hat_glm1 <- round(exp(predict(glm1, all_long_100)))
#all_long_100$e_hat_glm1 <- all_long_100$y_hat_glm1-all_long_100$confirmed

all_long_100$confirmed_log <- log(all_long_100$confirmed)

lm4 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed +
             days_since_1_confirmed:country_state, 
           data=all_long_100 )
display(lm4)
all_long_100$y_hat_lm4 <- round(exp(predict(lm4, all_long_100)))
all_long_100$e_hat_lm4 <- all_long_100$y_hat_lm4-all_long_100$confirmed

lm5 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed +
             days_since_1_confirmed_2 + 
             days_since_1_confirmed:country_state +  
             days_since_1_confirmed_2:country_state ,
           data=all_long_100 )
display(lm5)
all_long_100$y_hat_lm5 <- round(exp(predict(lm5, all_long_100)))
all_long_100$e_hat_lm5 <- all_long_100$y_hat_lm5-all_long_100$confirmed


lm6 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed +
             days_since_1_confirmed_2 + 
             days_since_1_confirmed_3 + 
             days_since_1_confirmed:country_state +  
             days_since_1_confirmed_2:country_state + 
             days_since_1_confirmed_3:country_state,
           data=all_long_100 )
display(lm6)
all_long_100$y_hat_lm6 <- round(exp(predict(lm6, all_long_100)))
all_long_100$e_hat_lm6 <- all_long_100$y_hat_lm6-all_long_100$confirmed

all_long_100$days_since_1_confirmed_log <- log(all_long_100$days_since_1_confirmed)
all_long_100$days_since_1_confirmed_log_2 <- all_long_100$days_since_1_confirmed_log^2
all_long_100$days_since_1_confirmed_log_3 <- all_long_100$days_since_1_confirmed_log^3

lm7 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed_log +
             days_since_1_confirmed:country_state , 
           data=all_long_100 )
display(lm7)
all_long_100$y_hat_lm7 <- round(exp(predict(lm7, all_long_100)))
all_long_100$e_hat_lm7 <- all_long_100$y_hat_lm7-all_long_100$confirmed

lm8 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed_log +
             days_since_1_confirmed_log_2 +
             days_since_1_confirmed:country_state + 
             days_since_1_confirmed_log_2:country_state , 
           data=all_long_100 )
display(lm8)
all_long_100$y_hat_lm8 <- round(exp(predict(lm8, all_long_100)))
all_long_100$e_hat_lm8 <- all_long_100$y_hat_lm8-all_long_100$confirmed

lm9 <- glm(confirmed_log ~ country_state + 
             days_since_1_confirmed_log +
             days_since_1_confirmed_log_2 + 
             days_since_1_confirmed_log_3 +
             days_since_1_confirmed:country_state + 
             days_since_1_confirmed_log_2:country_state +
             days_since_1_confirmed_log_3:country_state , 
           data=all_long_100 )
display(lm9)
all_long_100$y_hat_lm9 <- round(exp(predict(lm9, all_long_100)))
all_long_100$e_hat_lm9 <- all_long_100$y_hat_lm9-all_long_100$confirmed


library(Metrics)
rmse(all_long_100$y_hat_lm1,all_long_100$confirmed) #2015.659
rmse(all_long_100$y_hat_lm2,all_long_100$confirmed) #1035.882
rmse(all_long_100$y_hat_lm3,all_long_100$confirmed) #889
rmse(all_long_100$y_hat_lm4,all_long_100$confirmed) #7310
rmse(all_long_100$y_hat_lm5,all_long_100$confirmed) #2737
rmse(all_long_100$y_hat_lm6,all_long_100$confirmed) #612 #so far this is the winner

rmse(all_long_100$y_hat_lm7,all_long_100$confirmed) #5157.076
rmse(all_long_100$y_hat_lm8,all_long_100$confirmed) #881.1098
rmse(all_long_100$y_hat_lm9,all_long_100$confirmed) #969.2783

nb1 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed,
              data = all_long_100)
summary(nb1)
all_long_100$y_hat_nb1 <- round(predict(nb1, all_long_100, type="response"))
all_long_100$e_hat_nb1 <- all_long_100$y_hat_nb1-all_long_100$confirmed

nb2 <- glm.nb(confirmed ~ country_state + 
                          days_since_1_confirmed +
                          country_state:days_since_1_confirmed,
                          data = all_long_100
              )
summary(nb2)
all_long_100$y_hat_nb2 <- round(predict(nb2, all_long_100, type="response"))
all_long_100$e_hat_nb2 <- all_long_100$y_hat_nb2-all_long_100$confirmed

nb3 <- glm.nb(confirmed ~ country_state + 
                          days_since_1_confirmed +
                          days_since_1_confirmed_2 +
                          country_state:days_since_1_confirmed +
                          country_state:days_since_1_confirmed_2 ,
                        data = all_long_100
)
all_long_100$y_hat_nb3 <- round(predict(nb3, all_long_100, type="response"))
all_long_100$e_hat_nb3 <- all_long_100$y_hat_nb3-all_long_100$confirmed

nb4 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed +
                days_since_1_confirmed_2 +
                days_since_1_confirmed_3 +
                country_state:days_since_1_confirmed +
                country_state:days_since_1_confirmed_2 +
                country_state:days_since_1_confirmed_3 ,
              data = all_long_100
)
all_long_100$y_hat_nb4 <- round(predict(nb4, all_long_100, type="response"))
all_long_100$e_hat_nb4 <- all_long_100$y_hat_nb4-all_long_100$confirmed



rmse(all_long_100$y_hat_nb1,all_long_100$confirmed) #3252.802
rmse(all_long_100$y_hat_nb2,all_long_100$confirmed) #6356.23
rmse(all_long_100$y_hat_nb3,all_long_100$confirmed) #1502.571
rmse(all_long_100$y_hat_nb4,all_long_100$confirmed) #1557.028 #so we start to overfit here

#negative binomial logged time

nb_log_1 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed_log,
              data = all_long_100)
summary(nb_log_1)
all_long_100$y_hat_nb_log_1 <- round(predict(nb_log_1, all_long_100, type="response"))
all_long_100$e_hat_nb_log_1 <- all_long_100$y_hat_nb_log_1-all_long_100$confirmed

nb_log_2 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed_log +
                country_state:days_since_1_confirmed_log,
              data = all_long_100
)
summary(nb_log_2)
all_long_100$y_hat_nb_log_2 <- round(predict(nb_log_2, all_long_100, type="response"))
all_long_100$e_hat_nb_log_2 <- all_long_100$y_hat_nb_log_2-all_long_100$confirmed

#Won't work #Error: no valid set of coefficients has been found: please supply starting values
nb_log_3 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed_log +
                days_since_1_confirmed_log_2 +
                country_state:days_since_1_confirmed_log +
                country_state:days_since_1_confirmed_log_2 ,
              data = all_long_100
)
all_long_100$y_hat_nb_log_3 <- round(predict(nb_log_3, all_long_100, type="response"))
all_long_100$e_hat_nb_log_3 <- all_long_100$y_hat_nb_log_3-all_long_100$confirmed

#But this one does
nb_log_4 <- glm.nb(confirmed ~ country_state + 
                days_since_1_confirmed_log +
                days_since_1_confirmed_log_2 +
                days_since_1_confirmed_log_3 +
                country_state:days_since_1_confirmed_log +
                country_state:days_since_1_confirmed_log_2 +
                country_state:days_since_1_confirmed_log_3 ,
              data = all_long_100
)
all_long_100$y_hat_nb_log_4 <- round(predict(nb_log_4, all_long_100, type="response"))
all_long_100$e_hat_nb_log_4 <- all_long_100$y_hat_nb_log_4-all_long_100$confirmed

nb_log_5 <- glm.nb(confirmed ~  
                     date_asdate +
                     country_state + 
                     days_since_1_confirmed_log +
                     days_since_1_confirmed_log_2 +
                     days_since_1_confirmed_log_3 +
                     country_state:days_since_1_confirmed_log +
                     country_state:days_since_1_confirmed_log_2 +
                     country_state:days_since_1_confirmed_log_3 ,
                   data = all_long_100
)
all_long_100$y_hat_nb_log_5 <- round(predict(nb_log_5, all_long_100, type="response"))
all_long_100$e_hat_nb_log_5 <- all_long_100$y_hat_nb_log_5-all_long_100$confirmed

all_long_100$date_asdate_log <- log(as.numeric(all_long_100$date_asdate))
all_long_100$date_asdate_log_2 <- all_long_100$date_asdate_log ^2

nb_log_6 <- glm.nb(confirmed ~  
                     date_asdate +
                     date_asdate_log +
                     date_asdate_log_2 +
                     country_state + 
                     days_since_1_confirmed_log +
                     days_since_1_confirmed_log_2 +
                     days_since_1_confirmed_log_3 +
                     country_state:days_since_1_confirmed_log +
                     country_state:days_since_1_confirmed_log_2 +
                     country_state:days_since_1_confirmed_log_3 ,
                   data = all_long_100
)
all_long_100$y_hat_nb_log_6 <- round(predict(nb_log_6, all_long_100, type="response"))
all_long_100$e_hat_nb_log_6 <- all_long_100$y_hat_nb_log_6-all_long_100$confirmed

all_long_100$days_since_1_confirmed_log_4 <- all_long_100$days_since_1_confirmed_log^4
nb_log_7 <- glm.nb(confirmed ~  
                     date_asdate +
                     date_asdate_log +
                     country_state + 
                     days_since_1_confirmed_log +
                     days_since_1_confirmed_log_2 +
                     days_since_1_confirmed_log_3 +
                     days_since_1_confirmed_log_4 + 
                     country_state:days_since_1_confirmed_log +
                     country_state:days_since_1_confirmed_log_2 +
                     country_state:days_since_1_confirmed_log_3 +
                     country_state:days_since_1_confirmed_log_4 
                   ,
                   data = all_long_100
)
all_long_100$y_hat_nb_log_7 <- round(predict(nb_log_7, all_long_100, type="response"))
all_long_100$e_hat_nb_log_7 <- all_long_100$y_hat_nb_log_7-all_long_100$confirmed

all_long_100$days_since_1_confirmed_log_5 <- all_long_100$days_since_1_confirmed_log^5
nb_log_8 <- glm.nb(confirmed ~  
                     date_asdate +
                     date_asdate_log +
                     country_state + 
                     days_since_1_confirmed_log +
                     days_since_1_confirmed_log_2 +
                     days_since_1_confirmed_log_3 +
                     days_since_1_confirmed_log_4 + 
                     days_since_1_confirmed_log_5 + 
                     country_state:days_since_1_confirmed_log +
                     country_state:days_since_1_confirmed_log_2 +
                     country_state:days_since_1_confirmed_log_3 +
                     country_state:days_since_1_confirmed_log_4 +
                     country_state:days_since_1_confirmed_log_5 
                     
                   ,
                   data = all_long_100
)
all_long_100$y_hat_nb_log_8 <- round(predict(nb_log_8, all_long_100, type="response"))
all_long_100$e_hat_nb_log_8 <- all_long_100$y_hat_nb_log_8-all_long_100$confirmed


rmsle(all_long_100$y_hat_nb_log_1,all_long_100$confirmed) #0.4616091
rmsle(all_long_100$y_hat_nb_log_2,all_long_100$confirmed) #0.05348043
rmsle(all_long_100$y_hat_nb_log_3,all_long_100$confirmed) #
rmsle(all_long_100$y_hat_nb_log_4,all_long_100$confirmed) #659.2006 
rmsle(all_long_100$y_hat_nb_log_5,all_long_100$confirmed) #595.1177 
rmsle(all_long_100$y_hat_nb_log_6,all_long_100$confirmed) #496.5488 #still prefered
rmsle(all_long_100$y_hat_nb_log_7,all_long_100$confirmed) #535.0781
rmsle(all_long_100$y_hat_nb_log_8,all_long_100$confirmed) #462.3122 #tighter but now it's no longer realistic

temp <- all_long_100 %>% dplyr::select(country_state, date_asdate, days_since_1_confirmed, deaths, confirmed, recovered,  y_hat_nb_log_6, e_hat_nb_log_6)

temp %>% filter(country_state=="Italy NA") %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + geom_line(aes(x=days_since_1_confirmed,y=y_hat_nb_log_6))
temp %>% filter(country_state=="China Hubei") %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + geom_line(aes(x=days_since_1_confirmed,y=y_hat_nb_log_6))
temp %>% filter(country_state=="Spain NA") %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + geom_line(aes(x=days_since_1_confirmed,y=y_hat_nb_log_6))

p_all <- temp %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed)) + geom_line(aes(x=days_since_1_confirmed,y=y_hat_nb_log_6), color="red") + facet_wrap(~country_state, scales="free") + theme_bw()
p_all

#lowess smoothing
?
all_long_100 <- all_long_100 %>% arrange(country_state,days_since_1_confirmed) %>% group_by(country_state) %>% mutate(confirmed_lowess  = exp(lowess( confirmed_log , f = 10/n() , iter = 5 )$y ) ) %>% ungroup()
rmsle(all_long_100$confirmed_lowess,all_long_100$confirmed) #0.0697239

#cars.lo <- loess(dist ~ speed, cars, span=5)
all_long_100 <- all_long_100 %>% arrange(country_state,date_asdate) %>% group_by(country_state) %>% mutate(confirmed_loess  = exp(loess(confirmed_log ~ days_since_1_confirmed, span=.5, degree=1)$fitted) ) %>% ungroup()
all_long_100$confirmed_loess[!is.finite(all_long_100$confirmed_loess)] <- NA
rmsle(all_long_100$confirmed_loess,all_long_100$confirmed) #0.2712001

p_all <- all_long_100 %>% ggplot() + geom_point(aes(x=days_since_1_confirmed,y=confirmed)) +
           geom_line(aes(x=days_since_1_confirmed,y=confirmed_loess), color="red") + 
           #geom_line(aes(x=days_since_1_confirmed,y=y_hat_nb_log_6), color="blue") + 
           facet_wrap(~country_state, scales="free") + theme_bw()
p_all

temp <- all_long_100 %>% dplyr::select(country_state, date_asdate, days_since_1_confirmed, deaths, confirmed, recovered,  confirmed_lowess, confirmed_loess)


library(tibbletime)
rolling_mean <- rollify(mean, window = 5)
all_long_100 <- all_long_100 %>% arrange(country_state,date_asdate) %>% group_by(country_state) %>% mutate(confirmed_mean_5= exp(rolling_mean(confirmed_log)))  %>% ungroup()

