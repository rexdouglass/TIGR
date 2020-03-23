library(tidyverse)
library(googlesheets4)
library(hablar)
library(cronR)
    
getwd()
setwd("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/")

sheets_auth(email = "rexdouglass@gmail.com")
df_wide <- read_sheet("https://docs.google.com/spreadsheets/d/1NjWTKLaBB_CrTZPGhLUHpawj2z250Lub4wmz7Zs0_cE/edit?usp=sharing")
questions <- sort(names(df_wide)[2:ncol(df_wide)])
#dim(df_wide)
df_wide <- df_wide[,sort(names(df_wide))]
df_wide <- as.data.frame(lapply(df_wide, as.character))
colnames(df_wide) <- colnames(df_wide) %>% str_replace("\\.\\..*$","")
#glimpse(df_wide)

df_long <- df_wide %>%
  mutate(entry=1:n()) %>%
  mutate(country=X03) %>%
  mutate(location=X04) %>%
  mutate(location_wiki=X05) %>%
  mutate(date=X02) %>%
  mutate(url=X01) %>%
  
  select(-X01,
         -X02,
         -X03,
         -X04,
         -X05
  ) %>%
  
  
  pivot_longer(cols = starts_with("X"), names_to = "question", values_to = "answer") %>%
  #filter(!is.na(answer)) %>%
  mutate(question_number=as.numeric(str_replace_all(question,"[^0-9]",""))) %>%
  mutate(question_text=questions[question_number]) %>%
  mutate(question_text=str_replace_all(question_text,"^X|\\(.*?$","")) %>%
  mutate(question_text=str_replace_all(question_text,"^[0-9]*\\)","")) %>%
  mutate(question_text=trimws(question_text)) %>%
  
  mutate(location_wiki=ifelse(is.na(location_wiki), as.character(location), as.character(location_wiki)) ) %>%
  
  mutate(answer_letter= str_extract(answer,"^([a-z])\\)") ) %>%
  
  mutate(answer_letter=str_replace_all(answer_letter,"\\)","")) 


#head(df_long)


for_viewing <- df_long %>% select(country, location_wiki, date, question_text, answer, url, answer_letter) %>% arrange(country, location_wiki, date, question_text) %>%
                mutate(location_wiki=trimws(location_wiki)) %>% 
                mutate(location_wiki=str_replace_all(location_wiki," {1,}|\n{1,}"," ")) %>%
                mutate(location_wiki = strsplit(as.character(location_wiki), " ")) %>% 
                unnest(location_wiki) %>%
                mutate(location_wiki=trimws(location_wiki))
library(lubridate)
for_viewing_wide <- for_viewing %>% 
  mutate(question_answer=paste0(question_text, "-", answer_letter)) %>% filter(!is.na(answer_letter)) %>% 
  select(country, location_wiki, question_answer, date)  %>% 
  group_by(country, location_wiki, question_answer) %>%
  summarize(date=median(ymd(date))) %>%
  ungroup() %>%
  mutate(date=str_replace_all(date,"2020-","")) %>%
  mutate(location_wiki=str_replace_all(location_wiki,"https://en.wikipedia.org/wiki/|https://en.m.wikipedia.org/wiki/","")) %>%
  arrange(question_answer) %>%
  pivot_wider(
    names_from = question_answer,
    values_from = date,
    values_fn = list(breaks = paste, sep=";")
  ) %>% arrange(country, location_wiki)
  
saveRDS(for_viewing_wide, "/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/data_temp/for_viewing_wide.Rds")

for_consumption_long <- for_viewing_wide %>% 
                        pivot_longer(-c(country,location_wiki),
                                     names_to = "question_answer", values_to = "date") %>% na.omit() %>% 
                        separate(question_answer, c("question","answer"), sep="-", remove=T) %>% 
                        arrange(country,location_wiki, question, answer)
write_tsv(for_consumption_long,"/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/data_out/TIGR_version1_latest.tsv")
  
#install.packages("git2r")
library(git2r)

# Configure git.
git2r::config(user.name = "rexdouglass",user.email = "rexdouglass@gmail.com")
library(rmarkdown)
rmarkdown::render("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/TIGR/docs/TIGR_landing_page.Rmd")

repo <- repository(here::here())
add(repo, "./docs/TIGR_landing_page.nb.html")
add(repo, "./docs/TIGR_landing_page.Rmd")
add(repo, "./TIGR_version1_latest.tsv")
commit(repo, "Commit message")

# Push changes to github.
#saveRDS( cred_user_pass("", ""), "/home/skynet2/Downloads/secret_credentials.Rds") #remove the original text, only using the rds file which doesn't get pushed
secret_credentials=readRDS("/home/skynet2/Downloads/secret_credentials.Rds")

push(repo, credentials=secret_credentials)

