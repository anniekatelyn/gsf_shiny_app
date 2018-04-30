# script to scrape questions

categories = c('Genetic Modification and Testing', 'Electrogenic Human', 'Cloning', 'Modification of Sense of Smell')
dict = c()
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
questions = colnames(dat)[seq(1,16)]
questions = gsub('\\[', '', questions)
questions = gsub('\\]', '', questions)
keys = c(paste('A', seq(1,4), sep=""), paste('B', seq(1,4), sep=""), paste('C', seq(1,4), sep=""), paste('D', seq(1,4), sep=""))
add = 0
for(i in 1:4){
  for(j in 1:4){
    dict = c(dict, paste(keys[j+add], ":", questions[j+add], sep=" "))
  }
  add = add + 4
  dict = c(dict, '<br/>')
}
write(dict, file='key.txt')