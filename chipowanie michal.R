library(dplyr)
library(vistime)
library(lubridate)
library(rjson)
library(plotly)
library(stringr)
library(tidyverse)
setwd("C:/Users/mmate/Documents/chipowanie")
df<- read.csv("aw-events-export-aw-watcher-window_LAPTOP-H69L424N-2023-01-07.csv")
df_lastfm <- read.csv("lgfn.csv")
df_afk <- read.csv("aw-events-export-aw-watcher-afk_LAPTOP-H69L424N-2023-01-07.csv")



df<- df%>% mutate(date = substr(timestamp, 1, 10), hour = substr(timestamp, 12, 19)) %>%
  mutate(start = as.POSIXct(paste(date, hour, sep = " "))) %>% mutate(end = start + as.integer(duration))
df<-df %>% filter(duration>0, start>=as.POSIXct("2022-12-12 00:00:00"), app!="unknown")
df$app<- str_to_title(substring(df$app,1, nchar(df$app)-4))
df <- df %>% mutate(app = ifelse(app == "Winword", "Word", app)) %>%
  mutate(app = ifelse(app == "Mtga", "MTG Arena", app)) %>% mutate(category = ifelse(
    app %in% c("Firefox", "Explorer", "Rstudio", "MS Word", "Shellexperiencehost", "Idea64", "Teams",
               "Winrar", "Matlab", "Msteams", "Excel", "Acrobat", "Aw-Qt"),
  "work", "play"))

firefox_play <- c("youtube", "facebook", "fmovies", "123movies", "spotify",
                  "Rate Your Music", "tetris", "netflix", "film", "nowe horyzonty",
                  "wizzair", "skyscanner", "booking", "ryanair")


#timeline

#zmienna(dzieñ)
day <- "2022-12-16"
top <- df %>% filter(date == day) %>% group_by(app) %>%
  summarize(dur = sum(duration)) %>% filter(dur>5*60)
df_timelines <- df %>% filter(date == day, app %in% top$app) %>% arrange(app)

####nowy if, w œrodku stary kod, w elsie nowy kod
if (nrow(df_timelines) != 0){
  i <- 1
  while(!is.na(df_timelines[i+1,"app"])){
    
    
    if (is.na(df_timelines[i+1,"app"]) ){break}
    
    if(df_timelines[i, "app"]==df_timelines[i+1, "app"]){
      if(as.numeric(difftime(df_timelines[i, "start"], df_timelines[i+1, "end"], units = "secs")) < 120){
        df_timelines[i, "start"] <- df_timelines[i+1, "start"]
        df_timelines[i, "duration"] <- abs(as.numeric(difftime(df_timelines[i, "end"], df_timelines[i, "start"], units = "secs")))
        df_timelines<-df_timelines[-(i+1),]
        rownames(df_timelines) <- NULL
        i <-i-1
        
      }
    }
    i <- i+1
  }
  colnames(df_timelines)[c(7,8)] <- c("start_action", "end_action")

}else{
  df_timelines <- data.frame(app = " ",
                            start_action = as.POSIXct(paste(day, "00:00:00", sep=" ")),
                            end_action = as.POSIXct(paste(day, "23:59:59", sep=" ")))
}

#kolor
###nowa zmienna color_value u¿yta potem w deklaracji color
color_value <- ifelse(df_timelines$app[1] == " ", "transparent", "green")
color <- rep(color_value, nrow(df_timelines))

df_timelines <- cbind(df_timelines, color)
df_timelines <- df_timelines %>% mutate(tooltip = paste(app,"\n", "Od", format(start_action, "%H:%M:%S"), "do", format(end_action, "%H:%M:%S")))
plot<- vistime(df_timelines,
           col.event = "app",
           col.start = "start_action",
           col.end = "end_action",
           col.group = "app",
           show_labels = F,) %>%
  layout(
    xaxis = list(
      type = "date",
      tickformat = "%H:00"
    ),
    title = paste("Oœ czasu u¿ycia aplikacji u¿ywanych ponad 5 min w dniu", day)
  )
plot



### porównanie z muzyczk¹ (atrapa)
df_lastfm <- read.csv("lgfn.csv")
colnames(df_lastfm)[4] <- "start"
df_lastfm<- df_lastfm %>% mutate(start = paste(start, ":00", sep = ""))
df_lastfm$start<-parse_date_time(df_lastfm$start, "d b y H:M:S")
df_lastfm <- df_lastfm %>% filter(start>=parse_date_time("12 Dec 2022 00:00:00", "d b y H:M:S"))
end <- df_lastfm[,4] + seconds(197)
df_lastfm <- cbind(df_lastfm, end)

work <-rep(as.difftime(0, units = "secs"), nrow(df_lastfm))
play <-rep(as.difftime(0, units = "secs"), nrow(df_lastfm))
afk<- difftime(df_lastfm$end, df_lastfm$start, units = "secs")
df_lastfm<- cbind(df_lastfm, work, play, afk)
i <- nrow(df_lastfm)
j<- nrow(df)

while(i!=0){
  while(j != 0 && df_lastfm[i,"start"]>df[j,"end"]){
    j<-j-1
  }
  if(j==0){break}
  
  if(df_lastfm[i,"start"]>=df[j, "start"] && df_lastfm[i, "end"] <= df[j, "end"]){
    df_lastfm[i, df$category[j]] <- df_lastfm$afk[i]
    df_lastfm$afk[i] <- as.difftime(0, units="secs")
  }
  else if(df_lastfm[i,"start"]>=df[j, "start"]){
    time_not_afk <- difftime(df[j, "end"], df_lastfm[i, "start"], units = "secs")
    df_lastfm[i, df$category[j]] <- df_lastfm[i, df$category[j]] + time_not_afk
    df_lastfm$afk[i]<-df_lastfm$afk[i] - time_not_afk
  }
  else if(df$end[j]>=df_lastfm$end[i] && df$start[j] < df_lastfm$end[i]){
    time_not_afk <- difftime(df_lastfm[i, "end"], df[j, "start"],  units = "secs")
    df_lastfm[i, df$category[j]] <- df_lastfm[i, df$category[j]] + time_not_afk
    df_lastfm$afk[i]<-df_lastfm$afk[i] - time_not_afk
  }
  else if(df$end[j]<df_lastfm$end[i]&& df$start[j] < df_lastfm$end[i]){
    time_not_afk <- difftime(df$end[j], df$start[j], units = "secs")
    df_lastfm[i, df$category[j]] <- df_lastfm[i, df$category[j]] + time_not_afk
    df_lastfm$afk[i]<-df_lastfm$afk[i] - time_not_afk
  }
  i <- i-1
}
df_lastfm[,"work"]<- as.numeric(df_lastfm[,"work"])
df_lastfm[,"play"]<- as.numeric(df_lastfm[,"play"])
df_lastfm[,"afk"]<- as.numeric(df_lastfm[,"afk"])

work<-sum(df_lastfm$work)
play<-sum(df_lastfm$play)
afk<-sum(df_lastfm$afk)

plot2 <-plot_ly(
  x = c("Praca", "Rozrywka", "AFK"),
  y = c(as.integer(work/60), as.integer(play/60), as.integer( afk/60)),
  name = "wpa",
  type = "bar",
  marker = list(color = "green")
) %>% layout(
  yaxis <- list(title = "D³ugoœæ przes³uchanej muzyki (w min)"),
  title = "Kiedy najczêœciej s³uchamy muzyki?"
 
)
plot2


###json
library(jsonlite)
phone <- jsonlite::fromJSON("aw-bucket-export_aw-watcher-android-test.json",)
df_phone <- phone$buckets$`aw-watcher-android-test`$events
df_phone <- df_phone %>% select(2:4)
df_phone$data <- df_phone$data$app
colnames(df_phone)[3] <- "app"


### podzia³ praca/rozrywka/inne z telefonu
work <- c("Outlook", "Teams", "Multimedia i urz¹dzenia", "Kalkulator",
        "Dokumenty", "Moje pliki", "Dysk", "Discord", "Pliki",
        "Arkusze", "Write Japanese", "Japanese GG", "Duolingo", "ActivityWatch",
        "Zegar")
play <- c("Chrome", "Mastodon", "Facebook", "Po³¹cz.", "YouTube", "Aparat",
          "Spotify", "Messenger", "Santander", "Notatki Keep",
          "Telefon", "Mapy", "Bolt", "Instagram", "Genius",
          "Wiadomoœci", "Galeria", "Gmail", "¿appka", "Netflix",
          "Kalendarz", "Dyktafon", "McDonald's", "Going.",
          "Booking.com", "Vinted", "Empik", "H&M", "Odtwarzacz wideo",
          "KFC Polska", "Pyszne", "Uber Eats", "Amazon Shopping", "Miêœnie brzucha w 30 dni",
          "Bandcamp", "Jakdojade")
df_phone_with_categories <- df_phone %>% mutate(category = ifelse(app %in% work, "work", ifelse(app %in% play, "play", "other")))


### spotify

spotify_michal <- jsonlite::fromJSON("StreamingHistory0.json")
spotify_michal$endTime <- as.POSIXct(spotify_michal$endTime)
spotify_michal <- spotify_michal %>% filter(endTime > as.POSIXct("2022-12-12 00:00:00"), endTime < as.POSIXct("2023-01-10 23:59:59"))
spotify_michal <- spotify_michal %>% mutate(start = endTime - msPlayed/1000)
colnames(spotify_michal)[1] <- "end"
spotify_michal <- as.data.frame(t(rev(as.data.frame(t(spotify_michal)))))
rownames(spotify_michal) <- NULL

##ten sam kod co wy¿ej
work <-rep(as.difftime(0, units = "secs"), nrow(spotify_michal))
play <-rep(as.difftime(0, units = "secs"), nrow(spotify_michal))
afk<- round(difftime(spotify_michal$end, spotify_michal$start, units = "secs"), digits = 0)
spotify_michal<- cbind(spotify_michal, work, play, afk)

i <- nrow(spotify_michal)
j<- nrow(df)

while(i!=0){
  while(j != 0 && spotify_michal[i,"start"]>df[j,"end"]){
    j<-j-1
  }
  if(j==0){break}
  
  if(spotify_michal[i,"start"]>=df[j, "start"] && spotify_michal[i, "end"] <= df[j, "end"]){
    spotify_michal[i, df$category[j]] <- spotify_michal$afk[i]
    spotify_michal$afk[i] <- as.difftime(0, units="secs")
  }
  else if(spotify_michal[i,"start"]>=df[j, "start"]){
    time_not_afk <- difftime(df[j, "end"], spotify_michal[i, "start"], units = "secs")
    spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
    spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
  }
  else if(df$end[j]>=spotify_michal$end[i] && df$start[j] < spotify_michal$end[i]){
    time_not_afk <- difftime(spotify_michal[i, "end"], df[j, "start"],  units = "secs")
    spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
    spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
  }
  else if(df$end[j]<spotify_michal$end[i]&& df$start[j] < spotify_michal$end[i]){
    time_not_afk <- difftime(df$end[j], df$start[j], units = "secs")
    spotify_michal[i, df$category[j]] <- spotify_michal[i, df$category[j]] + time_not_afk
    spotify_michal$afk[i]<-spotify_michal$afk[i] - time_not_afk
  }
  i <- i-1
}
spotify_michal[,"work"]<- as.numeric(spotify_michal[,"work"])
spotify_michal[,"play"]<- as.numeric(spotify_michal[,"play"])
spotify_michal[,"afk"]<- as.numeric(spotify_michal[,"afk"])

work<-sum(spotify_michal$work)
play<-sum(spotify_michal$play)
afk<-sum(spotify_michal$afk)

plot2 <-plot_ly(
  x = c("Praca", "Rozrywka", "AFK"),
  y = c(as.integer(work/60), as.integer(play/60), as.integer( afk/60)),
  name = "wpa",
  type = "bar",
  marker = list(color = "green")
) %>% layout(
  yaxis = list(title = "Czas s³uchania muzyki (w min)"),
  title = "Kiedy najczêœciej s³uchamy muzyki?"
  
)
plot2
