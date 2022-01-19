# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)

loadLTERnutrients <- function() {
  # Package ID: knb-lter-ntl.1.11 Cataloging System:https://pasta.lternet.edu.
  # Data set title: North Temperate Lakes LTER:
  # Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current
  inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/52/802d63a4c35050b09ef6d1e7da3efd3f"
  # infile1 <- tempfile()
  # download.file(inUrl1, infile1, method="curl")
  # 
  # LTERnutrients <- read_csv(infile1, skip=1, quote ='"',guess_max = 20000, col_names=c(
  #   "lakeid","year4","daynum","sampledate","depth","rep","sta","event","ph","phair","alk","dic","tic","doc","toc",
  #   "no3no2","no2","nh4","totnf","totnuf","totpf","totpuf","drsif","brsif","brsiuf","tpm","totnuf_sloh","no3no2_sloh",
  #   "nh4_sloh","kjdl_n_sloh","totpuf_sloh","drp_sloh","drsif_sloh","flagdepth","flagph","flagphair","flagalk","flagdic",
  #   "flagtic","flagdoc","flagtoc","flagno3no2","flagno2","flagnh4","flagtotnf","flagtotnuf","flagtotpf","flagtotpuf",
  #   "flagdrsif","flagbrsif","flagbrsiuf","flagtpm","flagtotnuf_sloh","flagno3no2_sloh","flagnh4_sloh","flagkjdl_n_sloh",
  #   "flagtotpuf_sloh","flagdrp_sloh","flagdrsif_sloh"))
  LTERnutrients = read_csv(inUrl1)
}
loadLTERtemp <- function() {
  # Package ID: knb-lter-ntl.29.8 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER:
  # Physical Limnology of Primary Study Lakes 1981 - current
  
  inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/28/03e232a1b362900e0f059859abe8eb97"
  # infile3 <- tempfile()
  # download.file(inUrl3,infile3,method="curl")
  # 
  # LTERtemp <-read_csv(infile3, skip=1, quote ='"',guess_max = 100000, col_names=c(
  #   "lakeid" ,"year4" ,"daynum" ,"sampledate" ,"depth" ,"rep" ,"sta" ,"event" ,"wtemp" ,"o2" ,"o2sat" ,"deck" ,
  #   "light" ,"frlight" ,"flagdepth" ,"flagwtemp" ,"flago2" ,"flago2sat" ,"flagdeck" ,"flaglight" ,"flagfrlight"))
  # 
  LTERtemp = read_csv(inUrl3)
}
loadLTERions <- function() {
  # Package ID: knb-lter-ntl.2.34 Cataloging System:https://pasta.edirepository.org.
  # Data set title: North Temperate Lakes LTER:
  # Chemical Limnology of Primary Study Lakes: Major Ions 1981 - current
  
  inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/2/34/3f740d0b77b3caf6930a8ce9cca4306a"
  # infile2 <- tempfile()
  # download.file(inUrl2,infile2,method="curl")
  # 
  # LTERions <-read_csv(infile2, skip=1, quote ='"',guess_max = 20000, col_names=c(
  #   "lakeid","year4","daynum","sampledate","depth","rep","sta","event","cl","so4","ca","mg","na","k","fe","mn",
  #   "cond","flagcl","flagso4","flagca","flagmg","flagna","flagk","flagfe","flagmn","flagcond"))
  
  LTERions = read_csv(inUrl2)
  
}

LTERtemp = 
  loadLTERtemp() %>%
  dplyr::select(-flagdepth) %>% 
  mutate(across(everything(), ~replace(., .<0 , NA))) %>%
  rename_at(vars(wtemp:frlight), ~ str_c("value_",.)) %>%
  rename_at(vars(flagwtemp:flagfrlight), ~ str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
  filter(!is.na(value) & value>= 0) %>%
  filter(!str_detect(error,'A|K|L|H') | is.na(error)) %>%
  dplyr::select(-error) #%>%
# pivot_wider(names_from = item, values_from = value) 

LTERnutrients = loadLTERnutrients() %>%
  mutate(across(everything(), ~replace(., .<0 , NA))) %>%
  rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
  rename_all( ~ str_replace(., "_n", '.n')) %>%
  rename_at(vars(ph:drsif.sloh), ~ str_c("value_",.)) %>%
  rename_at(vars(flagdepth:flagdrsif.sloh), ~ str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
  filter(!is.na(value) & value>= 0) %>%
  filter(!str_detect(error,'A|K|L|H') | is.na(error)) %>%
  dplyr::select(-error) %>% 
  mutate(value = case_when(str_detect(item, ".sloh") ~ value*1000, #change sloh from mg to µg
                           TRUE ~ value)) %>% 
  mutate(item = case_when(str_detect(item, ".sloh") ~  str_remove(item, ".sloh"),
                          TRUE ~ item))
# pivot_wider(names_from = item, values_from = value) 


LTERions = loadLTERions() %>%
  mutate(across(everything(), ~replace(., .<0 , NA))) %>%
  rename_all( ~ str_replace(., "_sloh", '.sloh')) %>%
  rename_all( ~ str_replace(., "_n", '.n')) %>%
  rename_at(vars(cl:cond), ~ str_c("value_",.)) %>%
  rename_at(vars(flagcl:flagcond), ~ str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:event), names_to = c('.value','item'), names_sep = '_') %>%
  filter(!is.na(value) & value>= 0) %>%
  filter(!str_detect(error,'A|K|L|H') | is.na(error)) %>%
  dplyr::select(-error)

matchtable = data.frame(vars =  c('wtemp','o2','o2sat','doc','dic','toc','tic','no3no2','nh4',
                                  'totnuf','totnf','drp','totpuf','totpf',
                                  'ph','alk',
                                  'ca','mg','na','k','so4','cl','cond'),
                        names = c('Water Temperature (°C)',
                                  'Dissolved Oxygen (mg/L)',
                                  'Dissolved Oxygen (% sat)',
                                  'Dissolved Organic Carbon (mg/L)',
                                  'Dissolved Inorganic Carbon (mg/L)',
                                  'Total Organic Carbon (mg/L)',
                                  'Total Inorganic Carbon (mg/L)',
                                  'Nitrate + Nitrite (µg/L)',
                                  'Ammonium (µg/L)',
                                  'Total Nitrogen unfiltered (µg/L)',
                                  'Total Nitrogen filtered (µg/L)',
                                  'Dissolved Reactive Phosphorus (µg/L)',
                                  'Total Phosphorus unfiltered (µg/L)',
                                  'Total Phosphorus filtered (µg/L)',
                                  'pH',
                                  'Alkalinity (ueq/L)',
                                  'Calcium (mg/L)',
                                  'Magnesium (mg/L)',
                                  'Sodium (mg/L)',
                                  'Potassium (mg/L)',
                                  'Sulfate (mg/L)',
                                  'Chloride (mg/L)',
                                  'Specific Conductance (µS/cm)'),
                        url = c(rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=29&revision=29',3),
                          rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=1&revision=52',13),
                          rep('https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-ntl&identifier=2&revision=34',7)))

lakelocations = data.frame(Lake = c("Allequash Lake", "Big Muskellunge Lake", 
                                    "Crystal Bog", "Crystal Lake", "Sparkling Lake", "Trout Bog", 
                                    "Trout Lake", "Lake Mendota", "Lake Monona", "Lake Wingra", "Fish Lake"),
                           Lat = c(46.038317, 46.021067, 46.007583, 46.00275, 46.007733, 
                                   46.04125, 46.029267, 43.09885, 43.06337, 43.05258, 43.28733), 
                           Long = c(89.620617, -89.611783, -89.606183, -89.612233, -89.701183, 
                                    -89.686283, -89.665017, -89.40545, -89.36086, -89.42499, 
                                    -89.65173))

allLTER = LTERnutrients %>% bind_rows(LTERtemp) %>% bind_rows(LTERions) %>% 
  mutate(lakename = case_when(lakeid == 'AL' ~ 'Allequash',
                              lakeid == 'BM' ~ 'Big Musky',
                              lakeid == 'CR' ~ 'Crystal',
                              lakeid == 'CB' ~ 'Crystal Bog',
                              lakeid == 'SP' ~ 'Sparkling',
                              lakeid == 'TR' ~ 'Trout',
                              lakeid == 'TB' ~ 'Trout Bog',
                              lakeid == 'ME' ~ 'Mendota',
                              lakeid == 'MO' ~ 'Monona',
                              lakeid == 'FI' ~ 'Fish',
                              lakeid == 'WI' ~ 'Wingra'))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Get full variable name from matchtable
  varname <- reactive({
    a <- matchtable %>% dplyr::filter(names == input$input.vars) %>% dplyr::pull(vars)
    return(a)
  })
  
  # Get url of dataset name from matchtable
  # output$urlname <- renderText({ 
  #   a <- matchtable %>% dplyr::filter(names == input$input.vars) %>% dplyr::pull(url)
  #   return(a)
  # })
  # Get url of dataset name from matchtable
  output$urlname <- renderUI({
    a <- matchtable %>% dplyr::filter(names == input$input.vars) %>% dplyr::pull(url)
    url <- a("EDI Dataset Page", href = a, target="_blank")
    tagList("A friendly reminder to please cite data! Data citation for this dataset can be found here: ", url)
  })
  
  renderUI({
    paste("URL link:", url)
  })
  
  # Get depths
  getdepths = reactive({
    if (input$input.lake == 'All northern lakes') {
      lakes = c('Allequash','Big Musky','Crystal','Crystal Bog','Sparkling','Trout','Trout Bog')
    } else if (input$input.lake == 'All southern lakes') {
      lakes = c('Mendota','Monona','Fish','Wingra')
    } else {
      lakes = input$input.lake
    }
    b = allLTER %>% 
      filter(lakename %in% lakes) %>% 
      filter(item == varname()) %>% 
      group_by(depth) %>% tally() %>% filter(n > 80) %>% pull(depth)
    return(b)
  })
  
  
  ## used to find depths dependent on data
  output$datadepths <- renderUI({
    selectInput("depths", "select depth (m)", choices = getdepths(), selected = 0)
  })
  
  allLTER_filtered <- reactive({ 
    d = allLTER %>% 
      filter(item == varname()) %>% 
      filter(depth == input$depths)
    
    if (input$plottype == 'plot.am') {
      d = d %>% group_by(lakeid, lakename, item, year4) %>% 
        filter(rep == 1) %>% 
        filter(year4 != 1981) %>% 
        summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
        mutate(sampledate = as.Date(paste0(year4,'-07-01')))
    }
    return(d)
  })
  
  # OUTPUT TESTING
  # output$testvar = renderText(print(varname()))
  # output$testvar2 = renderText(print(names(allLTER_filtered())))
  
  # Plots 
  plotInput <- reactive({ 
    
    p = if(input$input.lake == 'All northern lakes') {
      ggplot(allLTER_filtered() %>% filter(lakename %in% c('Allequash','Big Musky','Crystal','Crystal Bog','Sparkling','Trout','Trout Bog'))) +
        if('Free y-axis' %in% input$scales) {
          facet_wrap(~lakename, scales = "free_y") 
        } else {
          facet_wrap(~lakename)
        }
    } else if (input$input.lake == 'All southern lakes') {
      ggplot(allLTER_filtered() %>% filter(lakename %in% c('Mendota','Monona','Fish','Wingra'))) +
        if('Free y-axis' %in% input$scales) {
          facet_wrap(~lakename, scales = "free_y") 
        } else {
          facet_wrap(~lakename)
        }
    } else {
      ggplot(allLTER_filtered() %>% filter(lakename == input$input.lake))
    }
    
    if (input$plottype == 'plot.mb') {
      p = p +
        geom_boxplot(aes(x = month(sampledate), y = value, group = month(sampledate)), fill = '#c2d6f0') +
        scale_x_discrete(limits = month.abb) 
    } else {
      p = p +
        geom_line(aes(x = sampledate, y = value)) +
        geom_point(aes(x = sampledate, y = value), size = 0.4)
    }
    if ('Log y-axis' %in% input$scales) {
      p = p + scale_y_log10()
    }
    p = p +
      ylab(input$input.vars) +
      theme_minimal(base_size = 14, base_family = 'Helvetica') +
      theme(axis.title.x = element_blank(),
            panel.grid = element_line(colour = "grey80"))
  })
  
  # Dislay plot
  output$distPlot <- renderPlot({
    print(plotInput())
  })
  
  # Download plot
  output$downloadImage = downloadHandler(
    filename = function() {paste0(input$input.lake,'_',varname(),'_',input$input.depth,'m.png')},
    content = function(file) {
      ggsave(file, plot = plotInput() + theme_bw(base_size = 8), width = 6, height = 4, units = 'in', dpi = 500,
             device = "png")
    })
  
  # # Make map 
  map = leaflet(data = lakelocations) %>%
    addTiles() %>%
    addMarkers(~Long, ~Lat, popup = ~as.character(Lake), label = ~as.character(Lake)) %>%
    setView(-89.6, 44.5, zoom = 6)
  output$myMap = renderLeaflet(map)
  
  
})
