library(tidyverse)
library(googlesheets4)
library(hablar)
library(cronR)

df_wide <- read_sheet("https://docs.google.com/spreadsheets/d/1NjWTKLaBB_CrTZPGhLUHpawj2z250Lub4wmz7Zs0_cE/edit?usp=sharing")
questions <- sort(names(df_wide)[2:ncol(df_wide)])
dim(df_wide)
df_wide <- df_wide[,sort(names(df_wide))]
df_wide <- as.data.frame(lapply(df_wide, as.character))

glimpse(df_wide)

df_long <- df_wide %>%
  mutate(entry=1:n()) %>%
  mutate(country=X03..Country.of.the.restriction) %>%
  mutate(location=X04..If.the.restriction.targets.only.part.of.a.country...what.is.the.name.of.that.part...examples..California.......Hubei.province.......New.Rochelle..New.York......Alameda.County..California...Marin.County..California...) %>%
  mutate(location_wiki=X05..Does.the.part.of.a.country.you.put.above.in.question.4.have.a.wikipedia.article.about.it...find.and.paste.the.URL.here.) %>%
  mutate(date=X02..Date.the.restriction.went.into.effect......NOT.the.date.of.the.article......if.multiple.restrictions.with.DIFFERENT.dates.then.submit.different.reports.for.each.date.) %>%
  mutate(url=X01..URL.of.News.Report.for.Source.of.Information..only.directly.links.to.reputable.news.sources...no.rumors..reddit.pages..or.tweets.) %>%
  
  select(-X01..URL.of.News.Report.for.Source.of.Information..only.directly.links.to.reputable.news.sources...no.rumors..reddit.pages..or.tweets.,
         -X02..Date.the.restriction.went.into.effect......NOT.the.date.of.the.article......if.multiple.restrictions.with.DIFFERENT.dates.then.submit.different.reports.for.each.date.,
         -X03..Country.of.the.restriction,
         -X04..If.the.restriction.targets.only.part.of.a.country...what.is.the.name.of.that.part...examples..California.......Hubei.province.......New.Rochelle..New.York......Alameda.County..California...Marin.County..California...,
         -X05..Does.the.part.of.a.country.you.put.above.in.question.4.have.a.wikipedia.article.about.it...find.and.paste.the.URL.here.
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


head(df_long)


for_viewing <- df_long %>% select(country, location_wiki, date, question_text, answer, url, answer_letter) %>% arrange(country, location_wiki, date, question_text)

for_viewing_wide <- for_viewing %>% 
  mutate(question_answer=paste0(question_text, "-", answer_letter)) %>% filter(!is.na(answer_letter)) %>% 
  select(country, location_wiki, question_answer, date)  %>%
  mutate(date=str_replace_all(date,"2020-","")) %>%
  mutate(location_wiki=str_replace_all(location_wiki,"https://en.wikipedia.org/wiki/|https://en.m.wikipedia.org/wiki/","")) %>%
  
  pivot_wider(
    names_from = question_answer,
    values_from = date,
    values_fn = list(breaks = paste, sep=";")
  )

saveRDS(for_viewing_wide, "./data_temp/for_viewing_wide.Rds")

#install.packages("git2r")
library(git2r)

# Configure git.
git2r::config(user.name = "rexdouglass",user.email = "rexdouglass@gmail.com")
library(rmarkdown)
rmarkdown::render("./docs/COVID19_interventions.Rmd")

add(repo, "./docs/COVID19_interventions.nb.html")
add(repo, "./docs/COVID19_interventions.Rmd")
commit(repo, "Commit message")

# Push changes to github.
#saveRDS( cred_user_pass("", ""), "/home/skynet2/Downloads/secret_credentials.Rds") #remove the original text, only using the rds file which doesn't get pushed
secret_credentials=readRDS("/home/skynet2/Downloads/secret_credentials.Rds")

push(repo, credentials=secret_credentials)



