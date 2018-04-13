library(shiny)
library(ggplot2)
library(googlesheets)
library(stringr)
library(tidyr)
library(gridExtra)
library(dplyr)

### Google Sheet Values ###
gsheet_title = "Survey: Technological Modifications of the Human  (Responses)"
gsheet = gs_title(gsheet_title)
ws_title = gs_ws_ls(gsheet)[1]

### Plot Theme ###
red='#e78285'
blue='#91bbe4'
green='#51b797'
font='Calibri'
plot_theme = theme(plot.title = element_text(family=font,face="bold"),
                   axis.text = element_text(family = font),
                   axis.title = element_text(family=font),
                   legend.text = element_text(family=font))

### Scenario Key ###
categories = c('Genetic Modification and Testing', 'Electrogenic Human', 'Cloning', 'Modification of Sense of Smell')
dict = c()
dat = gs_read(ss=gsheet, ws=ws_title)
dat = as.data.frame(dat)
dat = dat[c(-1, -ncol(dat))]
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

### Clean & Get Data ### 
extract_majors = function(s){
  split = unlist(strsplit(s, ","))
  split = trimws(split)
  return(split)
}
stem_subjs = c("Biology", "Chemistry", "Neuroscience", "BME", "ECE", "Mechanical Engineering", 
               "Computer Science", "Math", "Statistics", "Environmental Science", "Environmental Engineering")
is_stem = function(majors){
  isStem = any(stem_subjs %in% majors)
  if(isStem) return("STEM")
  else return("Non-STEM")
}

get_dat = function(){
  dat = gs_read(ss=gsheet, ws=ws_title)
  dat = as.data.frame(dat)
  dat = dat[c(-1, -ncol(dat))] # remove first and last column
  questions = colnames(dat)[-c(17,18)] # extract questions and rename columns
  qcols = c(paste('A', seq(1,4), sep=""),
            paste('B', seq(1,4), sep=""),
            paste('C', seq(1,4), sep=""),
            paste('D', seq(1,4), sep=""))
  
  colnames(dat) = c(qcols, 'Majors', colnames(dat)[18])
  dat$Year = as.numeric(str_extract_all(dat$Year, "[0-9]+")) # extract numerical graduating year
  dat$Year = as.factor(dat$Year)
  dat$Majors = sapply(dat$Majors, extract_majors)
  dat$stem = sapply(dat$Majors, is_stem)
  return(dat)
}

dat = get_dat()

function(input, output) {
  
  output$description = renderText(
    "This survey provides four ways in which humans can use genetic engineering or other technologies to alter 
    the human body for various purposes. Not all of these technologies are currently possible, but could be in the future. 
    Each scenario is rated with a level of comfort on a scale of 1-3 (1 = opposed, 2 = neutral, and 3 = support). 
    The results of this survey analyze conceptions about the ethical dilemmas we face as technological advances break down 
    boundaries between human and machine."
  )
  
  output$plot = renderPlot({
    dataset = dat

    if(input$compare){
      # reshape tables for ggplot
      dataset$ID = seq.int(nrow(dataset))
      dat1 = dataset[c(seq(1,4),seq(17,ncol(dataset)))]
      dat1 = dat1 %>% gather(Question, Response, -Year, -Majors, -ID, -stem)
      dat2 = dataset[c(seq(5,8),seq(17,ncol(dataset)))]
      dat2 = dat2 %>% gather(Question, Response, -Year, -Majors, -ID, -stem)
      dat3 = dataset[c(seq(9,12),seq(17,ncol(dataset)))]
      dat3 = dat3 %>% gather(Question, Response, -Year, -Majors, -ID, -stem)
      dat4 = dataset[seq(13,ncol(dataset))]
      dat4 = dat4 %>% gather(Question, Response, -Year, -Majors, -ID, -stem)
      
      pt1 = ggplot(dat1, aes(x=Response)) + labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green)) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt2 = ggplot(dat2, aes(x=Response)) + labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green)) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt3 = ggplot(dat3, aes(x=Response)) + labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green)) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt4 = ggplot(dat4, aes(x=Response)) + labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green)) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      if(input$stem){
        pt1 = pt1 + geom_bar(position='stack', aes(fill=stem))
        pt2 = pt2 + geom_bar(position='stack', aes(fill=stem))
        pt3 = pt3 + geom_bar(position='stack', aes(fill=stem))
        pt4 = pt4 + geom_bar(position='stack', aes(fill=stem))
      } else{
        pt1 = pt1 + geom_bar(position='dodge', aes(fill=Response))
        pt2 = pt2 + geom_bar(position='dodge', aes(fill=Response))
        pt3 = pt3 + geom_bar(position='dodge', aes(fill=Response))
        pt4 = pt4 + geom_bar(position='dodge', aes(fill=Response))
      }
      
      
      if(input$count){
        pt1 = pt1 + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1), size=2)
        pt2 = pt2 + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1), size=2)
        pt3 = pt3 + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1), size=2)
        pt4 = pt4 + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1), size=2)
      }
      
      plots = list(pt1, pt2, pt3, pt4)
      grid.arrange(grobs = plots, ncol = 2)
      
    } else{
      switch(input$cat,
             'Genetic Modification and Testing' = {dataset = dataset[c(seq(1,4),seq(17,ncol(dataset)))]},
             'Electrogenic Human' = {dataset = dataset[c(seq(5,8),seq(17,ncol(dataset)))]},
             'Cloning' = {dataset = dataset[c(seq(9,12),seq(17,ncol(dataset)))]},
             'Modification of Sense of Smell' = {dataset = dataset[seq(13,ncol(dataset))]})

      dataset$ID = seq.int(nrow(dataset))
      dataset = dataset %>% gather(Question, Response, -Year, -Majors, -ID, -stem)
      
      title = paste0('Responses to "', input$cat, '" Scenarios')
      
      pt = ggplot(dataset, aes(x=Response)) + labs(title=title, y="Count", x="") +
      scale_fill_manual(values=c(red, blue, green)) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      if(input$stem) pt = pt + geom_bar(position='stack', aes(fill=stem))
      else pt = pt + geom_bar(position='dodge', aes(fill=Response))
      
      if(input$count) {
        pt = pt + geom_text(stat='count', aes(label=..count..), vjust=-1, position = position_dodge(width = 1), size=3)
      }
      pt
    }
  }, height=600)
  
  output$demo_plot = renderPlot({
    dataset = dat
    
    pt1 = ggplot(dataset, aes(Year)) + labs(title = "Graduation Year of Participants", x="Graduating Year", y="Count") + 
      plot_theme + scale_fill_manual(values=c(red, blue)) 
    
    majors = dataset %>% pull(Majors) %>% unlist() %>% table() %>% as.data.frame()
    colnames(majors) = c('major', 'freq')
    majors$stem = sapply(majors$major, is_stem)
    majors$major <- factor(majors$major, levels = majors$major[order(majors$freq, decreasing = T)])
    pt2 = ggplot(majors, aes(x=major, y=freq)) + labs(title = "Majors of Participants", x="Major", y="Count") + 
      plot_theme + theme(axis.text.x=element_text(angle=60,hjust=1)) + scale_fill_manual(values=c(red, blue)) 
      
    if(input$stem){
      pt1 = pt1 + geom_bar(position='stack', width = 0.5, aes(fill=stem))
      pt2 = pt2 + geom_bar(position='stack', stat = "identity", aes(fill=stem))
    } else{
      pt1 = pt1 + geom_bar(width = 0.5, fill=green)
      pt2 = pt2 + geom_bar(stat = "identity", fill = red) 
    }
    
    if(input$count){
      pt1 = pt1 + geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)
      pt2 = pt2 + geom_text(aes(label=freq), vjust=-1, size=3)
    }
    
    plots = list(pt1, pt2)
    
    grid.arrange(grobs = plots, ncol = 2)
    
  }, height=600)
  
  output$google_form = renderUI({
    dat = get_dat()
    
    tags$iframe(id = "googleform",
                src = "https://docs.google.com/forms/d/e/1FAIpQLSeTr3axzQEzfiuf2LpP6Gzi4cb8Uk3F_yf1CDEYI3GnA97Lgw/viewform?embedded=true",
                width = 900,
                height = 700,
                frameborder = 0,
                marginheight = 0)
    
  })
  
  output$qdict = renderUI({
    header = paste0("<h4>", 'Scenario Key :', "</h4>")
    if(input$compare){
      HTML(paste0(header, paste(dict, collapse = '<br/>'))) 
    } else{
      key = NULL
      switch(input$cat,
             'Genetic Modification and Testing' = {key = paste(dict[1:5], collapse = '<br/>')},
             'Electrogenic Human' = {key = paste(dict[6:10], collapse = '<br/>')},
             'Cloning' = {key = paste(dict[11:15], collapse = '<br/>')},
             'Modification of Sense of Smell' = {key = paste(dict[16:20], collapse = '<br/>')})
      return(HTML(paste0(header, key)))
    }
  })
}


### OLD PLOTS ###

# pt1 = ggplot(dat1, aes(x=Question, group = Response, fill=Response)) + geom_bar(position="dodge") +
#   labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Response Count", x="Scenario") +
#   scale_fill_manual(values=c(red, blue, green)) + plot_theme

# pt2 = ggplot(dat2, aes(x=Question, group = Response, fill=Response)) + geom_bar(position="dodge") +
#   labs(title='Responses to "Electrogenic Human" Scenarios', y="Response Count", x="Scenario") +
#   scale_fill_manual(values=c(red, blue, green)) + plot_theme

# pt3 = ggplot(dat3, aes(x=Question, group = Response, fill=Response)) + geom_bar(position="dodge") +
#   labs(title='Responses to "Cloning" Scenarios', y="Response Count", x="Scenario") +
#   scale_fill_manual(values=c(red, blue, green)) + plot_theme
# 
# pt4 = ggplot(dat4, aes(x=Question, group = Response, fill=Response)) + geom_bar(position="dodge") +
#   labs(title='Responses to "Modification of Sense of Smell" Scenarios', y="Response Count", x="Scenario") +
#   scale_fill_manual(values=c(red, blue, green)) + plot_theme