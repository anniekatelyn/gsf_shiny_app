library(shiny)
library(ggplot2)
library(googlesheets)
library(stringr)
library(tidyr)
library(gridExtra)
library(dplyr)

# setwd('/Users/AnnieTang/Documents/DUKE/17-18/GSF 366/gsf_shiny_app')

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
  dat$Stem = as.factor(dat$Stem)
  saveRDS(dat, file = "survey_dat.rds")
  return(dat)
}

# dat = refresh_dat()

dict = scan('key.txt', what='character', sep='\n')
dat = readRDS(file="survey_dat.rds")

### Plot Theme ###
red='#e78285'
blue='#91bbe4'
green='#51b797'
font='Calibri'
plot_theme = theme(plot.title = element_text(family=font,face="bold"),
                   axis.text = element_text(family = font),
                   axis.title = element_text(family=font),
                   legend.text = element_text(family=font))

function(input, output) {
  
  observeEvent(input$refresh, {
    dat = refresh_dat()
  })
  
  output$description = renderUI({
    desc_txt = scan('proj_desc.txt', what='character', sep='\n')
    desc = paste(desc_txt, collapse='<br/><br/>')
    HTML(desc) 
  })
  
  output$plot = renderPlot({
    dataset = dat

    if(input$compare){
      # reshape tables for ggplot
      dataset$ID = seq.int(nrow(dataset))
      dat1 = dataset[c(seq(1,4),seq(17,ncol(dataset)))]
      dat1 = dat1 %>% gather(Question, Response, -Year, -Majors, -ID, -Stem)
      dat2 = dataset[c(seq(5,8),seq(17,ncol(dataset)))]
      dat2 = dat2 %>% gather(Question, Response, -Year, -Majors, -ID, -Stem)
      dat3 = dataset[c(seq(9,12),seq(17,ncol(dataset)))]
      dat3 = dat3 %>% gather(Question, Response, -Year, -Majors, -ID, -Stem)
      dat4 = dataset[seq(13,ncol(dataset))]
      dat4 = dat4 %>% gather(Question, Response, -Year, -Majors, -ID, -Stem)
      
      pt1 = ggplot(dat1, aes(x=Response)) + labs(title='Responses to "Genetic Modification and Testing" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green,'gray')) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt2 = ggplot(dat2, aes(x=Response)) + labs(title='Responses to "Electrogenic Human" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green,'gray')) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt3 = ggplot(dat3, aes(x=Response)) + labs(title='Responses to "Cloning" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green,'gray')) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      pt4 = ggplot(dat4, aes(x=Response)) + labs(title='Responses to "Modification of Sense of Smell" Scenarios', y="Count", x="") +
        scale_fill_manual(values=c(red, blue, green,'gray')) + facet_grid(~ Question) + plot_theme + theme(axis.text.x = element_blank())
      
      if(input$breakdown == '2'){
        pt1 = pt1 + geom_bar(position='stack', aes(fill=Stem))
        pt2 = pt2 + geom_bar(position='stack', aes(fill=Stem))
        pt3 = pt3 + geom_bar(position='stack', aes(fill=Stem))
        pt4 = pt4 + geom_bar(position='stack', aes(fill=Stem))
      } else if(input$breakdown == '3'){
        pt1 = pt1 + geom_bar(position='stack', aes(fill=Year))
        pt2 = pt2 + geom_bar(position='stack', aes(fill=Year))
        pt3 = pt3 + geom_bar(position='stack', aes(fill=Year))
        pt4 = pt4 + geom_bar(position='stack', aes(fill=Year))
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
      dataset = dataset %>% gather(Question, Response, -Year, -Majors, -ID, -Stem)
      
      title = paste0('Responses to "', input$cat, '" Scenarios')
      
      pt = ggplot(dataset, aes(x=Response)) + labs(title=title, y="Count", x="") +
      scale_fill_manual(values=c(red, blue, green,'gray')) + facet_grid(~ Question) + plot_theme
      
      if(input$breakdown == '2') pt = pt + geom_bar(position='stack', aes(fill=Stem))
      else if(input$breakdown =='3') pt = pt + geom_bar(position='stack', aes(fill=Year))
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
      plot_theme + scale_fill_manual(values=c(red, blue, green, 'gray')) 
    
    majors = dataset %>% select(Majors,Year) %>% unnest(Majors)
    colnames(majors) = c('Year', 'Major')
    majors$Stem = sapply(majors$Major, is_stem)
    majors$Stem = as.factor(majors$Stem)
    pt2 = ggplot(majors, aes(x=Major)) + labs(title = "Majors of Participants", x="Major", y="Count") + 
      plot_theme + theme(axis.text.x=element_text(angle=60,hjust=1)) + scale_fill_manual(values=c(red, blue, green, 'gray')) 
      
    if(input$breakdown == '2'){
      pt1 = pt1 + geom_bar(position='stack', width = 0.5, aes(fill=Stem))
      pt2 = pt2 + geom_bar(position='stack', aes(fill=Stem))
    } else if(input$breakdown == '3'){
      pt1 = pt1 + geom_bar(position='stack', width = 0.5, aes(fill=Year))
      pt2 = pt2 + geom_bar(position='stack', aes(fill=Year))
    } else{
      pt1 = pt1 + geom_bar(width = 0.5, fill=green)
      pt2 = pt2 + geom_bar(fill = red) 
    }
    
    if(input$count){
      pt1 = pt1 + geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)
      pt2 = pt2 + geom_text(stat='count', aes(label=..count..), vjust=-1, size=3)
    }
    
    plots = list(pt1, pt2)
    
    grid.arrange(grobs = plots, ncol = 2)
    
  }, height=600)
  
  output$google_form = renderUI({
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
