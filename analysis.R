library(purrr)
library(tidyr)
stem_subjs = c("Biology", "Chemistry", "Neuroscience", "BME", "ECE", "Mechanical Engineering", 
               "Computer Science", "Math", "Statistics", "Environmental Science", "Environmental Engineering")

extract_majors = function(s){
  split = unlist(strsplit(s, ","))
  split = trimws(split)
  return(split)
}

is_stem = function(majors){
  isStem = any(stem_subjs %in% majors)
  if(isStem) return(1)
  else return(0)
}

refresh_dat = function(){
  # refresh OAuth token
  gs_auth()
  # rescrape data from google sheet 
  gsheet_title = "Survey: Technological Modifications of the Human  (Responses)"
  gsheet = gs_title(gsheet_title)
  ws_title = gs_ws_ls(gsheet)[1]
  dat = gs_read(ss=gsheet, ws=ws_title)
  dat = as.data.frame(dat)
  # remove timestamp and extra column at end
  dat = dat[c(-1, -ncol(dat))] 
  # switch order of B3 and B4, error in ordering of initial survey 
  dat = dat[,c(1,2,3,4,5,6,8,7,9,10,11,12,13,14,15,16,17,18)]
  # set new column names for brevity
  qcols = c(paste('A', seq(1,4), sep=""),
            paste('B', seq(1,4), sep=""),
            paste('C', seq(1,4), sep=""),
            paste('D', seq(1,4), sep=""))
  colnames(dat) = c(qcols, 'Majors', colnames(dat)[18])
  # extract numerical graduating year
  dat$Year = as.numeric(str_extract_all(dat$Year, "[0-9]+")) 
  dat$Year = as.factor(dat$Year)
  # extract majors and create STEM column
  dat$Majors = sapply(dat$Majors, extract_majors)
  dat$Stem = sapply(dat$Majors, is_stem)
  saveRDS(dat, file = "survey_dat.rds")
  return(dat)
}

# dat = refresh_dat()
dat = readRDS(file = 'survey_dat.rds')

# reshape to one observation per row
dat = dat %>% gather(scenario, Response, -Year, -Majors, -Stem)
colnames(dat) = c('majors', 'year', 'stem', 'scenario', 'response')

# shorten response, make categorical 
dat$response = as.factor(dat$response)
# dat$response = as.factor(as.numeric(str_extract_all(dat$response, "[0-9]+")))

# reorder columns for clarity
dat = dat[,c(5,4,1,2,3)]

# create binary dummy variables from year
# freshman baseline
dat$sophomore = as.numeric(dat$year == '2020')
dat$sophomore = as.factor(dat$sophomore)
dat$junior = as.numeric(dat$year == '2019')
dat$junior = as.factor(dat$junior)
dat$senior = as.numeric(dat$year == '2018')
dat$senior = as.factor(dat$senior)

# remove year
dat = dat[-c(4)]

# something familiar / already ubiquitous in society 
ubiq = c('A1', 'B1', 'C1', 'C2', 'D1')
dat$ubiquitous = as.numeric(dat$scenario %in% ubiq)
dat$ubiquitous = as.factor(dat$ubiquitous)

# restores functionality / returns to 'normal' state
rest = c('A2', 'A3', 'B1', 'B2', 'C2', 'D2')
dat$restorative = as.numeric(dat$scenario %in% rest)
dat$restorative = as.factor(dat$restorative)

# enhancing / not restorative
enh = c('A4', 'D1', 'D3', 'D4')
dat$enhancing = as.numeric(dat$scenario %in% enh)
dat$enhancing = as.factor(dat$enhancing)

# not restorative or enhancing / generative 
gen = c('B3', 'B4', 'C3', 'C4', 'D3', 'D4')
dat$generative = as.numeric(dat$scenario %in% gen)
dat$generative = as.factor(dat$generative)

# invasive
inv = c('A2', 'A3', 'A4', 'B1','B2','B3','B4','C2','C4','D2','D3','D4')
dat$invasive = as.numeric(dat$scenario %in% inv)
dat$invasive = as.factor(dat$invasive)

# irreversible
irr = c('A1', 'A2', 'A3', 'A4', 'B2', 'B3', 'B4', 'C1' ,'C2', 'C3', 'C4', 'D1')
dat$irreversible = as.numeric(dat$scenario %in% irr)
dat$irreversible = as.factor(dat$irreversible)

# hereditary
her = c('A2', 'A3', 'A4', 'C4')
dat$hereditary = as.numeric(dat$scenario %in% her)
dat$hereditary = as.factor(dat$hereditary)

### MULTINOMIAL LOGIT ### 

library(nnet)
dat$response2 = relevel(dat$response, ref='2: Neutral')
test = multinom(response2 ~ stem + sophomore + junior + senior + ubiquitous + restorative
                + enhancing + generative + invasive + irreversible + hereditary, data=dat)
summary(test)

z = summary(test)$coefficients/summary(test)$standard.errors
p = (1-pnorm(abs(z),0,1)) * 2
p

exp(summary(test)$coefficients)/(1+exp(summary(test)$coefficients))

# statistically significant predictors: restorative (5.4e-06, 9.5e-11), invasive (2.1e-09, 3.3e-04), irreversible (0.003, 0.053)


# ggplot(data = dat, aes(x=response,fill=invasive)) + geom_bar(stat='count',position='dodge')
# 
# ggplot(data = dat, aes(x=invasive,fill=response)) + geom_bar(stat='count',position='dodge')



### LOGIT ### 

dat2 = dat[!(dat$response == '2: Neutral'),]

logit_fit = glm(response ~ stem + sophomore + junior + senior + ubiquitous + restorative + 
      enhancing + generative + invasive + irreversible + hereditary, 
    data = dat2, family = "binomial")

summary(logit_fit)
exp(logit_fit$coefficients)/(1+exp(logit_fit$coefficients))

# For every one unit change in [predictor], the log odds of supporting (versus rejecting) increases by [estimate].
# " probability of supporting increases by odds/(1+odds)
# odds = exp([estimate])

# statistically significant predictors: restorative (2e-16, 5.15), invasive(2e-16, -3.6), irreversible(4.69e-05, -2.1), hereditary(0.074, -0.71)
# secondarily significant: stem (0.15, -0.3)
