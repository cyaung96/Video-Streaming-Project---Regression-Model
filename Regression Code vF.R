#install deplyr
install.packages("dplyr")
library(dplyr)
library(ggplot2)

#bring in file that is already filtered to drama genre and has not rated content removed
streaming_data = read.csv(file.choose())

#filter to only exclusive content
exclusive_streaming_data = streaming_data %>% filter(exclusive == 'exclusive')
#exclusive = streaming_data %>% filter(exclusive == '1')

#The attach() function in R can be used to make objects within dataframes accessible in R with fewer keystrokes
attach(exclusive_streaming_data)

#Features
names(exclusive_streaming_data)

#create exclusive dummy variable (this one doesn't end up in our final model, since we filtered it down to only exclusive data)
exclusive_streaming_data$exclusive[exclusive=='exclusive']=1
exclusive_streaming_data$exclusive[!exclusive=='exclusive']=0
exclusive_streaming_data$exclusive=factor(exclusive_streaming_data$exclusive) # just making it a factor variable in R
str(exclusive_streaming_data$exclusive) #double check the structure

#create HBO dummy variable
exclusive_streaming_data$HBO[Platform=='HBO Max']=1
exclusive_streaming_data$HBO[!Platform=='HBO Max']=0
exclusive_streaming_data$HBO=factor(exclusive_streaming_data$HBO) # just making it a factor variable in R
str(exclusive_streaming_data$HBO)

#create movie dummy variable
exclusive_streaming_data$MOVIE[type=='MOVIE']=1
exclusive_streaming_data$MOVIE[!type=='MOVIE']=0
exclusive_streaming_data$MOVIE=factor(exclusive_streaming_data$MOVIE) # just making it a factor variable in R
str(exclusive_streaming_data$MOVIE)

#create mature content dummy variable
exclusive_streaming_data$age_cert_mature[age_certification_segments=='mature']=1
exclusive_streaming_data$age_cert_mature[!age_certification_segments=='mature']=0
exclusive_streaming_data$age_cert_mature=factor(exclusive_streaming_data$age_cert_mature) # just making it a factor variable in R
str(exclusive_streaming_data$age_cert_mature)

#create latest 10 years vs. old content dummy variable
exclusive_streaming_data$L10Y_vs_old[L10Y_vs_old=='L10Y']=1
exclusive_streaming_data$L10Y_vs_old[!L10Y_vs_old=='L10Y']=0
exclusive_streaming_data$L10Y_vs_old=factor(exclusive_streaming_data$L10Y_vs_old) # just making it a factor variable in R
str(exclusive_streaming_data$L10Y_vs_old)

#single linear regression (only platform vs. imdb_score)
imdb_score_regression_basic <- lm(imdb_score ~ HBO, exclusive_streaming_data)
print(summary(imdb_score_regression_basic))

ggplot(exclusive_streaming_data,aes(HBO,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_summary(geom="line",fun=mean,group=1,col="black" ) +
  labs(title = "HBO vs IMDB Score")

#multiple linear regression
imdb_score_regression <- lm(imdb_score ~ log(imdb_votes)+ HBO + MOVIE + 
                              age_cert_mature + L10Y_vs_old + 
                              runtime, exclusive_streaming_data)
print(summary(imdb_score_regression))

#plot multiple linear regression
#options(scipen=999)
ggplot(exclusive_streaming_data,aes(log(imdb_votes),imdb_score, col = HBO)) + 
  geom_point() + 
  stat_smooth(method = "lm",
              geom = "smooth") +
  labs(title = "IMDB Votes vs IMDB Score by Platform")

# #added variable plots
# #load car package
# library(car)
# #produce added variable plots
# avPlots(imdb_score_regression)

#log(imdb_votes)
imdb_score_regression_imdb_votes <- lm(imdb_score ~ log(imdb_votes), exclusive_streaming_data)
print(summary(imdb_score_regression_imdb_votes))

ggplot(exclusive_streaming_data,aes(log(imdb_votes),imdb_score, col = HBO)) + 
  geom_point() + 
  stat_smooth(method = "lm",
              geom = "smooth") +
  labs(title = "IMDB Votes vs IMDB Score by Platform")

#HBO
imdb_score_regression_HBO <- lm(imdb_score ~ HBO, exclusive_streaming_data)
print(summary(imdb_score_regression_HBO))

ggplot(exclusive_streaming_data,aes(HBO,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_summary(geom="line",fun=mean,group=1,col="black" ) +
  labs(title = "HBO vs IMDB Score")

#MOVIE
#single linear regression (only movie vs. imdb_score)
imdb_score_regression_MOVIE <- lm(imdb_score ~ MOVIE, exclusive_streaming_data)
print(summary(imdb_score_regression_MOVIE))

ggplot(exclusive_streaming_data,aes(MOVIE,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_summary(geom="line",fun=mean,group=1,col="black" ) +
  labs(title = "Movie vs IMDB Score")

#age_cert_mature
imdb_score_regression_age_cert_mature <- lm(imdb_score ~ age_cert_mature, exclusive_streaming_data)
print(summary(imdb_score_regression_age_cert_mature))

ggplot(exclusive_streaming_data,aes(age_cert_mature,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_summary(geom="line",fun=mean,group=1,col="black" ) +
  labs(title = "Mature Rating vs IMDB Score")

#L10Y_vs_old
imdb_score_regression_L10Y_vs_old <- lm(imdb_score ~ L10Y_vs_old, exclusive_streaming_data)
print(summary(imdb_score_regression_L10Y_vs_old))

ggplot(exclusive_streaming_data,aes(L10Y_vs_old,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_summary(geom="line",fun=mean,group=1,col="black" ) +
  labs(title = "Latest 10 Years vs IMDB Score")

#runtime
imdb_score_regression_runtime <- lm(imdb_score ~ runtime, exclusive_streaming_data)
print(summary(imdb_score_regression_runtime))

ggplot(exclusive_streaming_data,aes(runtime,imdb_score, col = HBO)) + 
  geom_point() + 
  stat_smooth(method = "lm",
              geom = "smooth") +
  labs(title = "Runtime vs IMDB Score by Platform")
