options(shiny.maxRequestSize = 3000 * 1024 ^ 2)

shinyServer(function(input, output, session) {
  values = reactiveValues()
  
  data_all = reactive({
    if (!is.null(input$file_txt)) {
      filename = input$file_txt$name
      # to determine if the input filepath is .txt or .zip
      if (grepl(".*\\.zip",filename)) {
        unzip(input$file_txt$datapath)
        files = list.files(pattern = ".txt")
        output = do.call("rbind", lapply(files, parse_all))
        return(output)
      }
      
      if (grepl(".*\\.txt", filename)) {
        output = parse_all(input$file_txt$datapath)
        return(output)
      }
      # end
      
      # or just read the parsed .csv
      if (grepl(".*\\.csv", filename)) {
        output = read.csv(input$file_txt$datapath)[,-1]
        return(output)
      }
      
    }else{
      NULL
    }
  })
  
  observe({
    df = data_all()
    values$df = df
    
    # updateinput controllers
    
    if (!is.null(df)) {
      year_slider_controller = c(min(as.numeric(as.character(df$year))), as.numeric(as.character(max(df$year))))
      updateSliderInput(
        session, "year_slider", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_journal_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_category_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_author_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_keyword_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_country_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_country_worldmap", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      updateSliderInput(
        session, "year_slider_for_institution_table", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      
      #
      hj_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(journal))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(journal))$year
      ))))
      updateSliderInput(
        session, "hj_slider", value = year_slider_controller,
        min = year_slider_controller[1], max = year_slider_controller[2]
      )
      hcat_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(domain))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(domain))$year
      ))))
      updateSliderInput(
        session, "hcat_slider", value = hcat_slider_controller,
        min = hcat_slider_controller[1], max = hcat_slider_controller[2]
      )
      hau_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(author))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(author))$year
      ))))
      updateSliderInput(
        session, "hau_slider", value = hau_slider_controller,
        min = hau_slider_controller[1], max = hau_slider_controller[2]
      )
      hkey_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(keyword))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(keyword))$year
      ))))
      updateSliderInput(
        session, "hkey_slider", value = hkey_slider_controller,
        min = hkey_slider_controller[1], max = hkey_slider_controller[2]
      )
      hc_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(country))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(country))$year
      ))))
      updateSliderInput(
        session, "hc_slider", value = hc_slider_controller,
        min = hc_slider_controller[1], max = hc_slider_controller[2]
      )
      hi_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(institution))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(institution))$year
      ))))
      updateSliderInput(
        session, "hi_slider", value = hi_slider_controller,
        min = hi_slider_controller[1], max = hi_slider_controller[2]
      )
      
      # network
      nau_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(author))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(author))$year
      ))))
      updateSliderInput(
        session, "nau_slider", value = nau_slider_controller,
        min = nau_slider_controller[1], max = nau_slider_controller[2]
      )
      nkey_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(keyword))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(keyword))$year
      ))))
      updateSliderInput(
        session, "nkey_slider", value = nkey_slider_controller,
        min = nkey_slider_controller[1], max = nkey_slider_controller[2]
      )
      nc_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(country))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(country))$year
      ))))
      updateSliderInput(
        session, "nc_slider", value = nc_slider_controller,
        min = nc_slider_controller[1], max = nc_slider_controller[2]
      )
      ni_slider_controller = c(min(as.numeric(as.character(
        subset(df,!is.na(institution))$year
      ))),
      max(as.numeric(as.character(
        subset(df,!is.na(institution))$year
      ))))
      updateSliderInput(
        session, "ni_slider", value = ni_slider_controller,
        min = ni_slider_controller[1], max = ni_slider_controller[2]
      )
      
      year_slice_max = year_slider_controller[2] - year_slider_controller[1]
      updateSliderInput(session, "year_slice_interval",
                        max = year_slice_max)
      
      # adv selectors
      updateSelectInput(session, "heatmap_journal_multi", choices = as.character(df$journal))
      updateSelectInput(session, "heatmap_category_multi", choices = as.character(df$domain))
      updateSelectInput(session, "heatmap_author_multi", choices = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "heatmap_keyword_multi", choices = unlist(lapply(df$keyword, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "heatmap_country_multi", choices = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "heatmap_institution_multi", choices = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_author_multi", choices = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "network_keyword_multi", choices = unlist(lapply(df$keyword, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "network_country_multi", choices = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      updateSelectInput(session, "network_institution_multi", choices = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_keyword_cm_author", choices = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_keyword_cm_country", choices = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_keyword_cm_institution", choices = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_author_cm_keyword", choices = unlist(lapply(df$keyword, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_country_cm_keyword", choices = unlist(lapply(df$keyword, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
      
      updateSelectInput(session, "network_institution_cm_keyword", choices = unlist(lapply(df$keyword, function(x) {
        strsplit(as.character(x), ";")[[1]]
      })))
    }
    
    # end of controllers
    
  })
  
  output$save_csv = downloadHandler(
    filename = 'download.csv',
    content = function(file) {
      write.csv(values$df, file)
    }
  )
  
  output$data_parse_all = DT::renderDataTable({
    df = values$df
    if (!is.null(df)) {
      df = df[,c(16,18,1,4,5,11)] # select several column to display
      df$link = unlist(lapply(df$title, function(x){
        if(is.na(x)){
          return(na)
        }else{
          output = createLink(x)
        }
      }))
      DT::datatable(
        df, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(
          style = 'caption-side: top;text-align: center;',
          'Table: ', htmltools::em('selected columns of the uploaded .txt files')
        ), escape = F
      )
    }else{
      NULL
    }
  })
  
  year_slider_data = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider[1]:input$year_slider[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  output$year_plot = renderPlot({
    df = year_slider_data()
    if (!is.null(df)) {
      year = as.data.frame(table(as.numeric(as.character(df$year))))
      year$Var1 = as.numeric(as.character(year$Var1))
      year$Freq = as.numeric(as.character(year$Freq))
      year_complete = data.frame(Var1 = min(year$Var1):max(year$Var1), Freq = NA)
      year_merge = merge(year, year_complete, by = "Var1", all = T)
      rm(year_complete)
      year = data.frame(year = as.character(year_merge$Var1), count = year_merge$Freq.x)
      rm(year_merge)
      year[is.na(year)] = 0
      ggplot(year, aes(x = year, y = count)) +
        geom_bar(stat = "identity", fill = 'dark green') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + # tilt the x tick
        xlab("Years") +
        ylab("Numbers of publications")
    }else{
      NULL
    }
  })
  
  year_slice_data = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider[1]:input$year_slider[2]
      df = values$df
      output = subset(df, year %in% year_range)
      n = input$year_slice_interval
      group = split(year_range, ceiling(seq_along(year_range) / n))
      year = unlist(lapply(group, function(x) {
        paste(min(x), max(x), sep = "-")
      }))
      count = unlist(lapply(group, function(x) {
        tmp = subset(df, year %in% x)
        sum(as.data.frame(table(as.numeric(
          as.character(tmp$year)
        )))$Freq)
      }))
      
      return(data.frame(year = year, count = count))
    }else{
      return(NULL)
    }
  })
  
  output$year_slice_plot = renderPlot({
    year = year_slice_data()
    if (!is.null(year)) {
      ggplot(year, aes(x = year, y = count)) +
        geom_bar(stat = "identity", fill = 'dark green') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + # tilt the x tick
        xlab("Years") +
        ylab("Numbers of publications")
    }else{
      NULL
    }
  })
  
  year_slider_data_for_journal_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_journal_table[1]:input$year_slider_for_journal_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  output$journal_table = DT::renderDataTable({
    df = year_slider_data_for_journal_table()
    if (!is.null(df)) {
      # start to build journal table
      journal = as.data.frame(table(df$journal))
      journal = journal[order(journal$Freq, decreasing = T),]
      journal$`TA (P)` = NA
      journal$`TC (P)` = NA
      journal$tcp = NA
      journal$category = NA
      
      for (i in 1:nrow(journal)) {
        # calculate the percentage of total articles
        journal$`TA (P)`[i] = paste(journal$Freq[i], " (",
                                    round(journal$Freq[i] / nrow(df), 3) * 100,
                                    ")", sep = "")
        
        # find the category
        journal$category[i] = as.character(df[which(grepl(journal$Var1[i], df$journal)),]$domain[1])
        
      }
      
      journal$`TC (P)` = unlist(lapply(journal$Var1, function(x) {
        sum(as.numeric(as.character(df$cited[which(df$journal == x)])))
      }))
      
      journal$tcp = unlist(lapply(journal$`TC (P)`, function(x) {
        round(x / sum(journal$`TC (P)`), 3) * 100
      }))
      
      journal$`TC (P)` = paste(journal$`TC (P)`, " (", journal$tcp, ")", sep = "")
      journal = journal[,-c(2,5)]
      colnames(journal)[1] = "Journal"
      # end of building
      
      DT::datatable(
        journal, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(
          style = 'caption-side: top;text-align: center;',
          'Table: ', htmltools::em(
            'Journals with the number of articles and Web of Science category of journals'
          ),
          htmltools::br(), 'TA (P): numbers of total articles (percentages)',
          htmltools::br(), 'TC (P): numbers of total cited times (percentages)'
        )
      )
    }else{
      NULL
    }
  })
  
  year_slider_data_for_category_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_category_table[1]:input$year_slider_for_category_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  output$category_table = DT::renderDataTable({
    df = year_slider_data_for_category_table()
    if (!is.null(df)) {
      # start to build category table
      category = as.data.frame(table(df$domain))
      category = category[order(category$Freq, decreasing = T),]
      category$P = round(category$Freq / nrow(df), 3) * 100
      colnames(category)[1:2] = c('Category', 'TA')
      # end of building
      
      DT::datatable(
        category, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(
          style = 'caption-side: top;text-align: center;',
          'Table: ', htmltools::em('Web of Science Categories'),
          htmltools::br(), 'TA: numbers of total articles',
          htmltools::br(), 'P: percentages'
        )
      )
    }else{
      NULL
    }
  })
  
  year_slider_data_for_author_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_author_table[1]:input$year_slider_for_author_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  year_slider_data_for_keyword_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_keyword_table[1]:input$year_slider_for_keyword_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  year_slider_data_for_country_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_country_table[1]:input$year_slider_for_country_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  year_slider_data_for_institution_table = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_institution_table[1]:input$year_slider_for_institution_table[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  output$author_table = DT::renderDataTable({
    df = year_slider_data_for_author_table()
    if (!is.null(df)) {
      # start to build author table
      author = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      author = as.data.frame(table(author))
      author = author[order(author$Freq, decreasing = T),]
      
      first_author = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]][1]
      }))
      first_author = as.data.frame(table(first_author))
      first_author = first_author[order(first_author$Freq, decreasing = T),]
      
      corresponding_author = unlist(lapply(df$c.author, function(x) {
        strsplit(as.character(x), ";")[[1]][1]
      }))
      corresponding_author = as.data.frame(table(corresponding_author))
      corresponding_author = corresponding_author[order(corresponding_author$Freq, decreasing = T),]
      
      author$`TA R (%)` = paste(
        rank(-author$Freq, ties.method = 'min'), " (",
        round(author$Freq / nrow(df), 3) * 100,
        ")", sep = ""
      )
      
      author$`FA (FR)` = unlist(lapply(author$author, function(x) {
        match = which(first_author$first_author == as.character(x))
        if (length(match) == 0)
          return(NA)
        else
          return(first_author$Freq[match])
      }))
      
      author$`FA (FR)` = paste(author$`FA (FR)`, " (",
                               rank(-author$`FA (FR)`, ties.method = 'min'),
                               ")", sep = "")
      
      author$`RP (RP R)` = unlist(lapply(author$author, function(x) {
        match = which(corresponding_author$corresponding_author == as.character(x))
        if (length(match) == 0)
          return(NA)
        else
          return(corresponding_author$Freq[match])
      }))
      
      author$`RP (RP R)` = paste(author$`RP (RP R)`, " (",
                                 rank(-author$`RP (RP R)`, ties.method = 'min'),
                                 ")", sep = "")
      
      colnames(author)[2] = "TA"
      # end of building
      
      DT::datatable(
        author, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(style = 'caption-side: top;text-align: center;',
                                          'Table: ', htmltools::em('Authors'),
                                          htmltools::br(), 'TA: numbers of total articles',
                                          htmltools::br(), 'R: rankings',
                                          htmltools::br(), '%: percentages',
                                          htmltools::br(), 'FA (FR): first author articles (rankings)',
                                          htmltools::br(), 'RP (RP R): corresponding author articles (rankings)')
      )
    }else{
      NULL
    }
  })
  
  output$keyword_table = DT::renderDataTable({
    df = year_slider_data_for_keyword_table()
    if (!is.null(df)) {
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      colnames(keyword)[2] = 'TA'
      DT::datatable(
        keyword, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(style = 'caption-side: top;text-align: center;',
                                          'Table: ', htmltools::em('keywords'),
                                          htmltools::br(), 'TA: total numbers of articles')
      )
    }else{
      NULL
    }
  })
  
  output$country_table = DT::renderDataTable({
    df = year_slider_data_for_country_table()
    if (!is.null(df)) {
      # start to build country table
      country = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      country = as.data.frame(table(country))
      country = country[order(country$Freq, decreasing = T),]
      country$rank = rank(-country$Freq, ties.method = "min")
      country$`TA R (%)` = paste(country$rank, " (",
                                 round(country$Freq / nrow(df), 3) * 100,
                                 ")", sep = "")
      country$`CA (CR)` = unlist(lapply(country$country, function(x) {
        match = as.character(df$country[which(grepl(x, as.character(df$country)))])
        sum(grepl(';', match)) # count cooperative article
      }))
      
      country$`CA (CR)` = paste(country$`CA (CR)`, " (",
                                rank(-country$`CA (CR)`, ties.method = 'min'),
                                ")", sep = "")
      
      country$`SA (SR)` = unlist(lapply(country$country, function(x) {
        match = as.character(df$country[which(grepl(x, as.character(df$country)))])
        sum(!grepl(';', match)) # count single article
      }))
      
      country$`SA (SR)` = paste(country$`SA (SR)`, " (",
                                rank(-country$`SA (SR)`, ties.method = 'min'),
                                ")", sep = "")
      
      # country$`RP (RP R)` = unlist(lapply(country$country, function(x){
      #   rp.country = as.data.frame(table(df$c.author.country))
      #   match = grepl(as.character(x), as.character(rp.country$Var1))
      #   if(sum(match) == 1) return(rp.country$Freq[which(match)]) else return(NA)
      # }))
      #
      # country$`RP (RP R)` = paste(country$`RP (RP R)`, " (",
      #                           rank(-country$`RP (RP R)`, ties.method = 'min'),
      #                           ")", sep = "")
      
      country$`S (%)` = round(unlist(lapply(country$country, function(x) {
        match = as.character(df$country[which(grepl(x, as.character(df$country)))])
        sum(!grepl(';', match)) # count single article
      })) / country$Freq, 3) * 100
      
      country = country[,-3]
      colnames(country)[1:2] = c('Country', 'TA')
      
      # end of building
      
      DT::datatable(
        country, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(style = 'caption-side: top;text-align: center;',
                                          'Table: ', htmltools::em('countries'),
                                          htmltools::br(), 'TA: total numbers of articles',
                                          htmltools::br(), 'R: rankings',
                                          htmltools::br(), '%: percentages',
                                          htmltools::br(), 'CA (CR): cooperative articles and rankings',
                                          htmltools::br(), 'SA (SR): single country articles and rankings',
                                          htmltools::br(), 'S: single country articles')
      )
    }else{
      NULL
    }
  })
  
  year_slider_data_for_country_worldmap = reactive({
    if (!is.null(input$file_txt)) {
      year_range = input$year_slider_for_country_worldmap[1]:input$year_slider_for_country_worldmap[2]
      df = values$df
      output = subset(df, year %in% year_range)
      return(output)
    }else{
      return(NULL)
    }
  })
  
  output$country_world_map = renderPlot({
    df = year_slider_data_for_country_worldmap()
    if (!is.null(df)) {
      country = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      country = as.data.frame(table(country))
      country$country = countrycode(country$country, origin = "country.name", destination = "iso3c")
      sPDF = joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")
      mapCountryData(sPDF ,nameColumnToPlot='Freq', catMethod = "fixedWidth", mapTitle = "")
      
    }else{
      NULL
    }
  })
  
  output$institution_table = DT::renderDataTable({
    df = year_slider_data_for_institution_table()
    if (!is.null(df)) {
      # start to build institution table
      institution = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      institution = as.data.frame(table(institution))
      institution = institution[order(institution$Freq, decreasing = T),]
      institution$rank = rank(-institution$Freq, ties.method = "min")
      institution$`TA R (%)` = paste(institution$rank, " (",
                                     round(institution$Freq / nrow(df), 3) * 100,
                                     ")", sep = "")
      institution$`CA (CR)` = unlist(lapply(institution$institution, function(x) {
        match = as.character(df$institution[which(grepl(x, as.character(df$institution)))])
        sum(grepl(';', match)) # count cooperative article
      }))
      
      institution$`CA (CR)` = paste(
        institution$`CA (CR)`, " (",
        rank(-institution$`CA (CR)`, ties.method = 'min'),
        ")", sep = ""
      )
      
      institution$`SA (SR)` = unlist(lapply(institution$institution, function(x) {
        match = as.character(df$institution[which(grepl(x, as.character(df$institution)))])
        sum(!grepl(';', match)) # count single article
      }))
      
      institution$`SA (SR)` = paste(
        institution$`SA (SR)`, " (",
        rank(-institution$`SA (SR)`, ties.method = 'min'),
        ")", sep = ""
      )
      
      # institution$`RP (RP R)` = unlist(lapply(institution$institution, function(x){
      #   rp.institution = as.data.frame(table(df$c.author.institution))
      #   match = grepl(as.character(x), as.character(rp.institution$Var1))
      #   if(sum(match) == 1) return(rp.institution$Freq[which(match)]) else return(NA)
      # }))
      #
      # institution$`RP (RP R)` = paste(institution$`RP (RP R)`, " (",
      #                            rank(-institution$`RP (RP R)`, ties.method = 'min'),
      #                            ")", sep = "")
      
      institution$`S (%)` = round(unlist(lapply(institution$institution, function(x) {
        match = as.character(df$institution[which(grepl(x, as.character(df$institution)))])
        sum(!grepl(';', match)) # count single article
      })) / institution$Freq, 3) * 100
      
      institution = institution[,-3]
      colnames(institution)[1:2] = c('institution', 'TA')
      # end of building
      
      DT::datatable(
        institution, filter = 'bottom', rownames = F,
        caption = htmltools::tags$caption(style = 'caption-side: top;text-align: center;',
                                          'Table: ', htmltools::em('institutions'),
                                          htmltools::br(), 'TA: total numbers of articles',
                                          htmltools::br(), 'R: rankings',
                                          htmltools::br(), '%: percentages',
                                          htmltools::br(), 'CA (CR): cooperative articles and rankings',
                                          htmltools::br(), 'SA (SR): single institution articles and rankings',
                                          htmltools::br(), 'S: single instituion articles')
      )
    }else{
      NULL
    }
  })
  
  journal_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_journal_number
      df = values$df
      
      journal = as.character(df$journal)
      journal = as.data.frame(table(journal))
      journal = journal[order(journal$Freq, decreasing = T),]
      journal = journal[1:top,]
      
      # verify the first year with the presence of journal name
      
      
      journal_heatmap = NULL
      journal_heatmap_year_row = NULL
      range = input$hj_slider[1]:input$hj_slider[2]
      for (i in 1:nrow(journal)) {
        for (j in range) {
          tmp = sum(grepl(journal$journal[i], subset(df, year %in% j)$journal))
          journal_heatmap_year_row = c(journal_heatmap_year_row, tmp)
        }
        journal_heatmap = rbind(journal_heatmap, journal_heatmap_year_row)
        journal_heatmap_year_row = NULL
      }
      
      colnames(journal_heatmap) = range
      row.names(journal_heatmap) = journal$journal
      
      for (i in 1:nrow(journal_heatmap)) {
        journal_heatmap[i,] = journal_heatmap[i,] / sum(journal_heatmap[i,])
      }
      return(journal_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$journal_heatmap_top = renderPlot({
    df = journal_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  category_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_category_number
      df = values$df
      
      category = as.character(df$domain)
      category = as.data.frame(table(category))
      category = category[order(category$Freq, decreasing = T),]
      category = category[1:top,]
      
      # verify the first year with the presence of category name
      
      
      category_heatmap = NULL
      category_heatmap_year_row = NULL
      range = input$hcat_slider[1]:input$hcat_slider[2]
      for (i in 1:nrow(category)) {
        for (j in range) {
          tmp = sum(grepl(category$category[i], subset(df, year %in% j)$domain))
          category_heatmap_year_row = c(category_heatmap_year_row, tmp)
        }
        category_heatmap = rbind(category_heatmap, category_heatmap_year_row)
        category_heatmap_year_row = NULL
      }
      
      colnames(category_heatmap) = range
      row.names(category_heatmap) = category$category
      
      for (i in 1:nrow(category_heatmap)) {
        category_heatmap[i,] = category_heatmap[i,] / sum(category_heatmap[i,])
      }
      return(category_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$category_heatmap_top = renderPlot({
    df = category_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  author_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_au_number
      df = values$df
      
      author = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      author = as.data.frame(table(author))
      author = author[order(author$Freq, decreasing = T),]
      author = author[1:top,]
      
      author_heatmap = NULL
      author_heatmap_year_row = NULL
      range = input$hau_slider[1]:input$hau_slider[2]
      for (i in 1:nrow(author)) {
        for (j in range) {
          tmp = sum(grepl(author$author[i], subset(df, year %in% j)$author))
          author_heatmap_year_row = c(author_heatmap_year_row, tmp)
        }
        author_heatmap = rbind(author_heatmap, author_heatmap_year_row)
        author_heatmap_year_row = NULL
      }
      
      colnames(author_heatmap) = range
      row.names(author_heatmap) = author$author
      
      for (i in 1:nrow(author_heatmap)) {
        author_heatmap[i,] = author_heatmap[i,] / sum(author_heatmap[i,])
      }
      return(author_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$author_heatmap_top = renderPlot({
    df = author_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  keyword_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_key_number
      df = values$df
      
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      keyword = keyword[1:top,]
      
      keyword_heatmap = NULL
      keyword_heatmap_year_row = NULL
      range = input$hkey_slider[1]:input$hkey_slider[2]
      for (i in 1:nrow(keyword)) {
        for (j in range) {
          tmp = sum(grepl(keyword$keyword[i], subset(df, year %in% j)$keyword))
          keyword_heatmap_year_row = c(keyword_heatmap_year_row, tmp)
        }
        keyword_heatmap = rbind(keyword_heatmap, keyword_heatmap_year_row)
        keyword_heatmap_year_row = NULL
      }
      
      colnames(keyword_heatmap) = range
      row.names(keyword_heatmap) = keyword$keyword
      
      for (i in 1:nrow(keyword_heatmap)) {
        keyword_heatmap[i,] = keyword_heatmap[i,] / sum(keyword_heatmap[i,])
      }
      return(keyword_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$keyword_heatmap_top = renderPlot({
    df = keyword_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  country_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_c_number
      df = values$df
      
      country = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      country = as.data.frame(table(country))
      country = country[order(country$Freq, decreasing = T),]
      country = country[1:top,]
      
      # verify the first year with the presence of country name
      
      
      country_heatmap = NULL
      country_heatmap_year_row = NULL
      range = input$hc_slider[1]:input$hc_slider[2]
      for (i in 1:nrow(country)) {
        for (j in range) {
          tmp = sum(grepl(country$country[i], subset(df, year %in% j)$country))
          country_heatmap_year_row = c(country_heatmap_year_row, tmp)
        }
        country_heatmap = rbind(country_heatmap, country_heatmap_year_row)
        country_heatmap_year_row = NULL
      }
      
      colnames(country_heatmap) = range
      row.names(country_heatmap) = country$country
      
      for (i in 1:nrow(country_heatmap)) {
        country_heatmap[i,] = country_heatmap[i,] / sum(country_heatmap[i,])
      }
      return(country_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$country_heatmap_top = renderPlot({
    df = country_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  institution_heatmap_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$heatmap_i_number
      df = values$df
      
      institution = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      institution = as.data.frame(table(institution))
      institution = institution[order(institution$Freq, decreasing = T),]
      institution = institution[1:top,]
      
      # verify the first year with the presence of institution name
      
      
      institution_heatmap = NULL
      institution_heatmap_year_row = NULL
      range = input$hi_slider[1]:input$hi_slider[2]
      for (i in 1:nrow(institution)) {
        for (j in range) {
          tmp = sum(grepl(
            institution$institution[i], subset(df, year %in% j)$institution
          ))
          institution_heatmap_year_row = c(institution_heatmap_year_row, tmp)
        }
        institution_heatmap = rbind(institution_heatmap, institution_heatmap_year_row)
        institution_heatmap_year_row = NULL
      }
      
      colnames(institution_heatmap) = range
      row.names(institution_heatmap) = institution$institution
      
      for (i in 1:nrow(institution_heatmap)) {
        institution_heatmap[i,] = institution_heatmap[i,] / sum(institution_heatmap[i,])
      }
      return(institution_heatmap)
    }else{
      return(NULL)
    }
  })
  
  output$institution_heatmap_top = renderPlot({
    df = institution_heatmap_top_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      NULL
    }
  })
  
  journal_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_journal_multi)) {
      select = input$heatmap_journal_multi
      df = values$df
      
      df = subset(df, journal %in% select)
      
      journal = as.character(df$journal)
      journal = as.data.frame(table(journal))
      journal = journal[order(journal$Freq, decreasing = T),]
      
      journal_heatmap = NULL
      journal_heatmap_year_row = NULL
      range = input$hj_slider[1]:input$hj_slider[2]
      for (i in 1:nrow(journal)) {
        for (j in range) {
          tmp = sum(grepl(journal$journal[i], subset(df, year %in% j)$journal))
          journal_heatmap_year_row = c(journal_heatmap_year_row, tmp)
        }
        journal_heatmap = rbind(journal_heatmap, journal_heatmap_year_row)
        journal_heatmap_year_row = NULL
      }
      
      colnames(journal_heatmap) = range
      row.names(journal_heatmap) = journal$journal
      
      for (i in 1:nrow(journal_heatmap)) {
        journal_heatmap[i,] = journal_heatmap[i,] / sum(journal_heatmap[i,])
      }
      return(journal_heatmap)
    }else
      (return(NULL))
  })
  
  output$journal_heatmap_multi = renderPlot({
    df = journal_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  category_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_category_multi)) {
      select = input$heatmap_category_multi
      df = values$df
      
      df = subset(df, domain %in% select)
      
      category = as.character(df$domain)
      category = as.data.frame(table(category))
      category = category[order(category$Freq, decreasing = T),]
      
      category_heatmap = NULL
      category_heatmap_year_row = NULL
      range = input$hcat_slider[1]:input$hcat_slider[2]
      for (i in 1:nrow(category)) {
        for (j in range) {
          tmp = sum(grepl(category$category[i], subset(df, year %in% j)$domain))
          category_heatmap_year_row = c(category_heatmap_year_row, tmp)
        }
        category_heatmap = rbind(category_heatmap, category_heatmap_year_row)
        category_heatmap_year_row = NULL
      }
      
      colnames(category_heatmap) = range
      row.names(category_heatmap) = category$category
      
      for (i in 1:nrow(category_heatmap)) {
        category_heatmap[i,] = category_heatmap[i,] / sum(category_heatmap[i,])
      }
      return(category_heatmap)
    }else
      (return(NULL))
  })
  
  output$category_heatmap_multi = renderPlot({
    df = category_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  author_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_author_multi)) {
      select = input$heatmap_author_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$author)),]
      
      
      author_heatmap = NULL
      author_heatmap_year_row = NULL
      range = input$hau_slider[1]:input$hau_slider[2]
      for (i in 1:length(select)) {
        for (j in range) {
          tmp = sum(grepl(select[i], subset(df, year %in% j)$author))
          author_heatmap_year_row = c(author_heatmap_year_row, tmp)
        }
        author_heatmap = rbind(author_heatmap, author_heatmap_year_row)
        author_heatmap_year_row = NULL
      }
      
      colnames(author_heatmap) = range
      row.names(author_heatmap) = select
      
      for (i in 1:nrow(author_heatmap)) {
        author_heatmap[i,] = author_heatmap[i,] / sum(author_heatmap[i,])
      }
      return(author_heatmap)
    }else
      (return(NULL))
  })
  
  output$author_heatmap_multi = renderPlot({
    df = author_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  keyword_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_keyword_multi)) {
      select = input$heatmap_keyword_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$keyword)),]
      
      
      keyword_heatmap = NULL
      keyword_heatmap_year_row = NULL
      range = input$hkey_slider[1]:input$hkey_slider[2]
      for (i in 1:length(select)) {
        for (j in range) {
          tmp = sum(grepl(select[i], subset(df, year %in% j)$keyword))
          keyword_heatmap_year_row = c(keyword_heatmap_year_row, tmp)
        }
        keyword_heatmap = rbind(keyword_heatmap, keyword_heatmap_year_row)
        keyword_heatmap_year_row = NULL
      }
      
      colnames(keyword_heatmap) = range
      row.names(keyword_heatmap) = select
      
      for (i in 1:nrow(keyword_heatmap)) {
        keyword_heatmap[i,] = keyword_heatmap[i,] / sum(keyword_heatmap[i,])
      }
      return(keyword_heatmap)
    }else
      (return(NULL))
  })
  
  output$keyword_heatmap_multi = renderPlot({
    df = keyword_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  country_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_country_multi)) {
      select = input$heatmap_country_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$country)),]
      
      
      country_heatmap = NULL
      country_heatmap_year_row = NULL
      range = input$hc_slider[1]:input$hc_slider[2]
      for (i in 1:length(select)) {
        for (j in range) {
          tmp = sum(grepl(select[i], subset(df, year %in% j)$country))
          country_heatmap_year_row = c(country_heatmap_year_row, tmp)
        }
        country_heatmap = rbind(country_heatmap, country_heatmap_year_row)
        country_heatmap_year_row = NULL
      }
      
      colnames(country_heatmap) = range
      row.names(country_heatmap) = select
      
      for (i in 1:nrow(country_heatmap)) {
        country_heatmap[i,] = country_heatmap[i,] / sum(country_heatmap[i,])
      }
      return(country_heatmap)
    }else
      (return(NULL))
  })
  
  output$country_heatmap_multi = renderPlot({
    df = country_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  institution_heatmap_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$heatmap_institution_multi)) {
      select = input$heatmap_institution_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$institution)),]
      
      
      institution_heatmap = NULL
      institution_heatmap_year_row = NULL
      range = input$hi_slider[1]:input$hi_slider[2]
      for (i in 1:length(select)) {
        for (j in range) {
          tmp = sum(grepl(select[i], subset(df, year %in% j)$institution))
          institution_heatmap_year_row = c(institution_heatmap_year_row, tmp)
        }
        institution_heatmap = rbind(institution_heatmap, institution_heatmap_year_row)
        institution_heatmap_year_row = NULL
      }
      
      colnames(institution_heatmap) = range
      row.names(institution_heatmap) = select
      
      for (i in 1:nrow(institution_heatmap)) {
        institution_heatmap[i,] = institution_heatmap[i,] / sum(institution_heatmap[i,])
      }
      return(institution_heatmap)
    }else
      (return(NULL))
  })
  
  output$institution_heatmap_multi = renderPlot({
    df = institution_heatmap_multi_data()
    if (!is.null(df)) {
      pheatmap(
        df, cluster_rows = F, cluster_cols = F, legend = T, height = "auto"
      )
    }else{
      return(NULL)
    }
  })
  
  # network: only include author, keyword, country and institution
  author_network_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$network_author_number
      df = values$df
      
      author = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      author = as.data.frame(table(author))
      author = author[order(author$Freq, decreasing = T),]
      author = author[1:top,]
      
      range = input$nau_slider[1]:input$nau_slider[2]
      
      matrix_author = NULL
      matrix_author_row = NULL
      for (i in 1:nrow(author)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$author[j]), ";")[[1]] == author[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_author_row = c(matrix_author_row, tmp1)
        }
        matrix_author = rbind(matrix_author, matrix_author_row)
        matrix_author_row = NULL
      }
      colnames(matrix_author) = 1:nrow(df)
      row.names(matrix_author) = author[,1]
      total_occurrence = rowSums(as.array(matrix_author))
      data_matrix = as.matrix(matrix_author)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_author)
    }else{
      return(NULL)
    }
  })
  
  output$author_network_top = renderForceNetwork({
    data = author_network_top_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  keyword_network_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$network_keyword_number
      df = values$df
      
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      keyword = keyword[1:top,]
      
      range = input$nkey_slider[1]:input$nkey_slider[2]
      
      matrix_keyword = NULL
      matrix_keyword_row = NULL
      for (i in 1:nrow(keyword)) {
        for (j in 1:nrow(df)) {
          tmp = which(toupper(stemDocument(tolower(strsplit(as.character(df$keyword[j]), ";")[[1]]))) == keyword[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_keyword_row = c(matrix_keyword_row, tmp1)
        }
        matrix_keyword = rbind(matrix_keyword, matrix_keyword_row)
        matrix_keyword_row = NULL
      }
      colnames(matrix_keyword) = 1:nrow(df)
      row.names(matrix_keyword) = keyword[,1]
      total_occurrence = rowSums(as.array(matrix_keyword))
      data_matrix = as.matrix(matrix_keyword)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_keyword)
    }else{
      return(NULL)
    }
  })
  
  output$keyword_network_top = renderForceNetwork({
    data = keyword_network_top_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  country_network_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$network_country_number
      df = values$df
      
      country = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      country = as.data.frame(table(country))
      country = country[order(country$Freq, decreasing = T),]
      country = country[1:top,]
      
      range = input$nau_slider[1]:input$nau_slider[2]
      
      matrix_country = NULL
      matrix_country_row = NULL
      for (i in 1:nrow(country)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$country[j]), ";")[[1]] == country[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_country_row = c(matrix_country_row, tmp1)
        }
        matrix_country = rbind(matrix_country, matrix_country_row)
        matrix_country_row = NULL
      }
      colnames(matrix_country) = 1:nrow(df)
      row.names(matrix_country) = country[,1]
      total_occurrence = rowSums(as.array(matrix_country))
      data_matrix = as.matrix(matrix_country)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_country)
    }else{
      return(NULL)
    }
  })
  
  output$country_network_top = renderForceNetwork({
    data = country_network_top_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  institution_network_top_data = reactive({
    if (!is.null(input$file_txt)) {
      top = input$network_institution_number
      df = values$df
      
      institution = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      institution = as.data.frame(table(institution))
      institution = institution[order(institution$Freq, decreasing = T),]
      institution = institution[1:top,]
      
      range = input$ni_slider[1]:input$ni_slider[2]
      
      matrix_institution = NULL
      matrix_institution_row = NULL
      for (i in 1:nrow(institution)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$institution[j]), ";")[[1]] == institution[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_institution_row = c(matrix_institution_row, tmp1)
        }
        matrix_institution = rbind(matrix_institution, matrix_institution_row)
        matrix_institution_row = NULL
      }
      colnames(matrix_institution) = 1:nrow(df)
      row.names(matrix_institution) = institution[,1]
      total_occurrence = rowSums(as.array(matrix_institution))
      data_matrix = as.matrix(matrix_institution)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_institution)
    }else{
      return(NULL)
    }
  })
  
  output$institution_network_top = renderForceNetwork({
    data = institution_network_top_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  
  
  author_network_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$network_author_multi)) {
      select = input$network_author_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$author)),]
      
      range = input$nau_slider[1]:input$nau_slider[2]
      
      matrix_author = NULL
      matrix_author_row = NULL
      for (i in 1:length(select)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$author[j]), ";")[[1]] == select[i])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_author_row = c(matrix_author_row, tmp1)
        }
        matrix_author = rbind(matrix_author, matrix_author_row)
        matrix_author_row = NULL
      }
      colnames(matrix_author) = 1:nrow(df)
      row.names(matrix_author) = select
      total_occurrence = rowSums(as.array(matrix_author))
      data_matrix = as.matrix(matrix_author)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_author)
    }else{
      return(NULL)
    }
    
  })
  
  output$author_network_multi = renderForceNetwork({
    data = author_network_multi_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  keyword_network_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$network_keyword_multi)) {
      select = input$network_keyword_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$keyword)),]
      
      range = input$nkey_slider[1]:input$nkey_slider[2]
      
      matrix_keyword = NULL
      matrix_keyword_row = NULL
      for (i in 1:length(select)) {
        for (j in 1:nrow(df)) {
          tmp = which(toupper(stemDocument(tolower(strsplit(as.character(df$keyword[j]), ";")[[1]]))) == select[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_keyword_row = c(matrix_keyword_row, tmp1)
        }
        matrix_keyword = rbind(matrix_keyword, matrix_keyword_row)
        matrix_keyword_row = NULL
      }
      colnames(matrix_keyword) = 1:nrow(df)
      row.names(matrix_keyword) = select
      total_occurrence = rowSums(as.array(matrix_keyword))
      data_matrix = as.matrix(matrix_keyword)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_keyword)
    }else{
      return(NULL)
    }
    
  })
  
  output$keyword_network_multi = renderForceNetwork({
    data = keyword_network_multi_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  country_network_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$network_country_multi)) {
      select = input$network_country_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$country)),]
      
      range = input$nc_slider[1]:input$nc_slider[2]
      
      matrix_country = NULL
      matrix_country_row = NULL
      for (i in 1:length(select)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$country[j]), ";")[[1]] == select[i])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_country_row = c(matrix_country_row, tmp1)
        }
        matrix_country = rbind(matrix_country, matrix_country_row)
        matrix_country_row = NULL
      }
      colnames(matrix_country) = 1:nrow(df)
      row.names(matrix_country) = select
      total_occurrence = rowSums(as.array(matrix_country))
      data_matrix = as.matrix(matrix_country)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_country)
    }else{
      return(NULL)
    }
    
  })
  
  output$country_network_multi = renderForceNetwork({
    data = country_network_multi_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  institution_network_multi_data = reactive({
    if (is.null(input$file_txt))
      return(NULL)
    if (!is.null(input$network_institution_multi)) {
      select = input$network_institution_multi
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$institution)),]
      
      range = input$ni_slider[1]:input$ni_slider[2]
      
      matrix_institution = NULL
      matrix_institution_row = NULL
      for (i in 1:length(select)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$institution[j]), ";")[[1]] == select[i])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_institution_row = c(matrix_institution_row, tmp1)
        }
        matrix_institution = rbind(matrix_institution, matrix_institution_row)
        matrix_institution_row = NULL
      }
      colnames(matrix_institution) = 1:nrow(df)
      row.names(matrix_institution) = select
      total_occurrence = rowSums(as.array(matrix_institution))
      data_matrix = as.matrix(matrix_institution)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_institution)
    }else{
      return(NULL)
    }
    
  })
  
  output$institution_network_multi = renderForceNetwork({
    data = institution_network_multi_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  author_network_cross_match_data = reactive({
    if (!is.null(input$network_author_cm_keyword)) {
      select = input$network_author_cm_keyword
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$keyword)),]
      
      author = unlist(lapply(df$author, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      author = as.data.frame(table(author))
      author = author[order(author$Freq, decreasing = T),]
      
      top = input$network_author_number_for_cm
      author = author[1:top,]
      
      range = input$nau_slider[1]:input$nau_slider[2]
      
      matrix_author = NULL
      matrix_author_row = NULL
      for (i in 1:nrow(author)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$author[j]), ";")[[1]] == author[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_author_row = c(matrix_author_row, tmp1)
        }
        matrix_author = rbind(matrix_author, matrix_author_row)
        matrix_author_row = NULL
      }
      colnames(matrix_author) = 1:nrow(df)
      row.names(matrix_author) = author[,1]
      total_occurrence = rowSums(as.array(matrix_author))
      data_matrix = as.matrix(matrix_author)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_author)
    }else{
      return(NULL)
    }
  })
  
  output$author_network_cross_match = renderForceNetwork({
    data = author_network_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  keyword_network_author_cross_match_data = reactive({
    if (!is.null(input$network_keyword_cm_author)) {
      select = input$network_keyword_cm_author
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$author)),]
      
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      
      top = input$network_keyword_number_for_cm
      keyword = keyword[1:top,]
      
      range = input$nkey_slider[1]:input$nkey_slider[2]
      
      matrix_keyword = NULL
      matrix_keyword_row = NULL
      for (i in 1:nrow(keyword)) {
        for (j in 1:nrow(df)) {
          tmp = which(toupper(stemDocument(tolower(strsplit(as.character(df$keyword[j]), ";")[[1]]))) == keyword[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_keyword_row = c(matrix_keyword_row, tmp1)
        }
        matrix_keyword = rbind(matrix_keyword, matrix_keyword_row)
        matrix_keyword_row = NULL
      }
      colnames(matrix_keyword) = 1:nrow(df)
      row.names(matrix_keyword) = keyword[,1]
      total_occurrence = rowSums(as.array(matrix_keyword))
      data_matrix = as.matrix(matrix_keyword)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_keyword)
    }else{
      return(NULL)
    }
  })
  
  output$keyword_network_author_cross_match = renderForceNetwork({
    data = keyword_network_author_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  keyword_network_country_cross_match_data = reactive({
    if (!is.null(input$network_keyword_cm_country)) {
      select = input$network_keyword_cm_country
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$country)),]
      
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      
      top = input$network_keyword_number_for_cm
      keyword = keyword[1:top,]
      
      range = input$nkey_slider[1]:input$nkey_slider[2]
      
      matrix_keyword = NULL
      matrix_keyword_row = NULL
      for (i in 1:nrow(keyword)) {
        for (j in 1:nrow(df)) {
          tmp = which(toupper(stemDocument(tolower(strsplit(as.character(df$keyword[j]), ";")[[1]]))) == keyword[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_keyword_row = c(matrix_keyword_row, tmp1)
        }
        matrix_keyword = rbind(matrix_keyword, matrix_keyword_row)
        matrix_keyword_row = NULL
      }
      colnames(matrix_keyword) = 1:nrow(df)
      row.names(matrix_keyword) = keyword[,1]
      total_occurrence = rowSums(as.array(matrix_keyword))
      data_matrix = as.matrix(matrix_keyword)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_keyword)
    }else{
      return(NULL)
    }
  })
  
  output$keyword_network_country_cross_match = renderForceNetwork({
    data = keyword_network_country_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  keyword_network_institution_cross_match_data = reactive({
    if (!is.null(input$network_keyword_cm_institution)) {
      select = input$network_keyword_cm_institution
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$institution)),]
      
      keyword = unlist(lapply(df$keyword, function(x) {
        if(!is.na(x)) {
          x = gsub("-", " ", x)
          x = gsub("^ ", "", x)
          x = strsplit(as.character(x), ";")[[1]]
          x = strsplit(x, " ")
          x_last = x[[1]][length(x[[1]])]
          x_last = toupper(stemDocument(tolower(x_last)))
          x = paste(x[[1]][-length(x[[1]])], x_last,  sep = " ")
        }else{return(NULL)}
      }))
      keyword = as.data.frame(table(keyword))
      keyword = keyword[order(keyword$Freq, decreasing = T),]
      
      top = input$network_keyword_number_for_cm
      keyword = keyword[1:top,]
      
      range = input$nkey_slider[1]:input$nkey_slider[2]
      
      matrix_keyword = NULL
      matrix_keyword_row = NULL
      for (i in 1:nrow(keyword)) {
        for (j in 1:nrow(df)) {
          tmp = which(toupper(stemDocument(tolower(strsplit(as.character(df$keyword[j]), ";")[[1]]))) == keyword[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_keyword_row = c(matrix_keyword_row, tmp1)
        }
        matrix_keyword = rbind(matrix_keyword, matrix_keyword_row)
        matrix_keyword_row = NULL
      }
      colnames(matrix_keyword) = 1:nrow(df)
      row.names(matrix_keyword) = keyword[,1]
      total_occurrence = rowSums(as.array(matrix_keyword))
      data_matrix = as.matrix(matrix_keyword)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_keyword)
    }else{
      return(NULL)
    }
  })
  
  output$keyword_network_institution_cross_match = renderForceNetwork({
    data = keyword_network_institution_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  country_network_cross_match_data = reactive({
    if (!is.null(input$network_country_cm_keyword)) {
      select = input$network_country_cm_keyword
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$keyword)),]
      
      country = unlist(lapply(df$country, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      country = as.data.frame(table(country))
      country = country[order(country$Freq, decreasing = T),]
      
      top = input$network_country_number_for_cm
      country = country[1:top,]
      
      range = input$nc_slider[1]:input$nc_slider[2]
      
      matrix_country = NULL
      matrix_country_row = NULL
      for (i in 1:nrow(country)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$country[j]), ";")[[1]] == country[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_country_row = c(matrix_country_row, tmp1)
        }
        matrix_country = rbind(matrix_country, matrix_country_row)
        matrix_country_row = NULL
      }
      colnames(matrix_country) = 1:nrow(df)
      row.names(matrix_country) = country[,1]
      total_occurrence = rowSums(as.array(matrix_country))
      data_matrix = as.matrix(matrix_country)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_country)
    }else{
      return(NULL)
    }
  })
  
  output$country_network_cross_match = renderForceNetwork({
    data = country_network_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
  institution_network_cross_match_data = reactive({
    if (!is.null(input$network_institution_cm_keyword)) {
      select = input$network_institution_cm_keyword
      df = values$df
      
      df = df[which(grepl(paste(select, collapse = "|"), df$keyword)),]
      
      institution = unlist(lapply(df$institution, function(x) {
        strsplit(as.character(x), ";")[[1]]
      }))
      institution = as.data.frame(table(institution))
      institution = institution[order(institution$Freq, decreasing = T),]
      
      top = input$network_institution_number_for_cm
      institution = institution[1:top,]
      
      range = input$nc_slider[1]:input$nc_slider[2]
      
      matrix_institution = NULL
      matrix_institution_row = NULL
      for (i in 1:nrow(institution)) {
        for (j in 1:nrow(df)) {
          tmp = which(strsplit(as.character(df$institution[j]), ";")[[1]] == institution[i,1])
          if (length(tmp) == 0)
            tmp1 = F
          else
            tmp1 = tmp
          matrix_institution_row = c(matrix_institution_row, tmp1)
        }
        matrix_institution = rbind(matrix_institution, matrix_institution_row)
        matrix_institution_row = NULL
      }
      colnames(matrix_institution) = 1:nrow(df)
      row.names(matrix_institution) = institution[,1]
      total_occurrence = rowSums(as.array(matrix_institution))
      data_matrix = as.matrix(matrix_institution)
      co_occurrence = data_matrix %*% t(data_matrix)
      output = list(co_occurrence, total_occurrence, matrix_institution)
    }else{
      return(NULL)
    }
  })
  
  output$institution_network_cross_match = renderForceNetwork({
    data = institution_network_cross_match_data()
    if (!is.null(data)) {
      # start to plot
      node = data.frame(name = row.names(data[[3]]),
                        group = "1",
                        size = rowSums(data[[3]]))
      #  / sum(rowSums(a[[3]])) * node_size
      
      row.names(node) = as.character(1:nrow(node))
      matrix = data[[1]]
      row.names(matrix) = as.character(1:nrow(matrix))
      colnames(matrix) = as.character(1:ncol(matrix))
      
      link = NULL
      for (i in 1:nrow(matrix)) {
        tmp = data.frame(
          source = as.numeric(rep(i, length(matrix[i,][-c(1:i)]))),
          target = as.numeric(names(matrix[i,][-c(1:i)])),
          value = as.numeric(matrix[i,][-c(1:i)])
        )
        link = rbind(link, tmp)
      }
      
      # remove the rows when value == 0 in link
      link = link[which(link$value != 0),]
      
      # standardize the node size and edge width
      link$value = link$value / sum(link$value)
      node$size = node$size / sum(node$size)
      
      forceNetwork(
        Links = transform(link, source = source-1, target = target - 1),
        Nodes = node, Group = "group",
        Source = "source", Target = "target",
        Value = "value", NodeID = "name", Nodesize = "size",
        opacity = 1, legend = F, zoom = T, opacityNoHover = 1
      )
    }else
      (return(NULL))
  })
  
})
