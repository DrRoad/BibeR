library(shiny)
library(gsubfn) # strapllyc to get time cited from citation
library(ggplot2)
library(DT)
#library(d3heatmap)
#library(igraph)
library(countrycode)
library(rworldmap)
library(networkD3)
library(dplyr) # filter function
library(pheatmap)
# a new parser to parse plain txt
require(stringr) # str_extract()
require(tm)  # tolower() or toupper()
# function
parser = function(x, item) {
  #
  if (item %in% c('author')) {
    author = str_extract(x, "(?<=\nAU )[a-zA-Z]+, [a-zA-Z]+(\n   [a-zA-Z]+, [a-zA-Z]+)*")
    author = gsub("\n   ", ";", author)
    author = toupper(author)
    return(author)
  }
  
  if (item %in% c('c_author', 'corresponding author', 'correspond')) {
    c_author = str_extract(x, "(?<=\nRP )[a-zA-Z]+, [a-zA-Z]+(\n   [a-zA-Z]+, [a-zA-Z]+)*")
    c_author = gsub("\n   ", ";", c_author)
    c_author = toupper(c_author)
    return(c_author)
  }
  
  if (item %in% c('c_author address', 'corresponding author address', 'correspond address')) {
    c_author_address = str_extract(x, "(?<=\nRP ).*(\n   .*)*")
    c_author_address = gsub("\n   ", ";", c_author_address)
    c_author_address = toupper(c_author_address)
    return(c_author_address)
  }
  
  if (item %in% c('title')) {
    title = str_extract(x, "(?<=\nTI ).*(\n   .*)*")
    title = gsub("\n   ", " ", title)
    title = toupper(title)
    return(title)
  }
  
  if (item %in% c('journal')) {
    journal = str_extract(x, "(?<=\nSO ).*(\n   .*)*")
    journal = gsub("\n   ", " ", journal)
    journal = toupper(journal)
    return(journal)
  }
  
  if (item %in% c('language')) {
    language = str_extract(x, "(?<=\nLA ).*(\n   .*)*")
    language = gsub("\n   ", "", language)
    language = toupper(language)
    return(language)
  }
  
  if (item %in% c('type')) {
    type = str_extract(x, "(?<=\nDT ).*(\n   .*)*")
    type = gsub("\n   ", " ", type)
    type = toupper(type)
    return(type)
  }
  
  if (item %in% c('keyword')) {
    keyword = str_extract(x, "(?<=\nDE ).*(\n   .*)*")
    keyword = gsub("\n   ", " ", keyword)
    keyword = toupper(keyword)
    return(keyword)
  }
  
  if (item %in% c('keyword plus')) {
    keyword.plus = str_extract(x, "(?<=\nDE ).*(\n   .*)*")
    keyword.plus = gsub("\n   ", " ", keyword.plus)
    keyword.plus = toupper(keyword.plus)
    return(keyword.plus)
  }
  
  if (item %in% c('abstract')) {
    abstract = str_extract(x, "(?<=\nAB ).*(\n   .*)*")
    abstract = gsub("\n   ", " ", abstract)
    abstract = toupper(abstract)
    return(abstract)
  }
  
  if (item %in% c('address')) {
    address = str_extract(x, "(?<=\nC1 ).*(\n   .*)*")
    address = gsub("\n   ", "\r", address)
    address = toupper(address)
    return(address)
  }
  
  if (item %in% c('cited reference','reference')) {
    ref = str_extract(x, "(?<=\nNR )[0-9]*")
    ref = as.numeric(ref)
    return(ref)
  }
  
  if (item %in% c('times cited','cited')) {
    cited = str_extract(x, "(?<=\nTC )[0-9]*")
    cited = as.numeric(cited)
    return(cited)
  }
  
  if (item %in% c('year','date')) {
    year = str_extract(x, "(?<=\nPY )[0-9]*")
    year = as.numeric(year)
    return(year)
  }
  
  if (item %in% c('page','page number')) {
    page = str_extract(x, "(?<=\nPG )[0-9]*")
    page = as.numeric(page)
    return(page)
  }
  
  if (item %in% c('research domain', 'subject', 'domain')) {
    domain = str_extract(x, "(?<=\nSC ).*(\n   .*)*")
    domain = gsub("\n   ", " ", domain)
    domain = toupper(domain)
    return(domain)
  }
  
}


# function
parse_all = function(file_path) {
  require(stringr) # str_extract()
  require(tm)  # tolower() or toupper()
  
  file = readChar(file_path, file.info(file_path)$size)
  split.txt = unlist(strsplit(file, "\n\n"))
  split.txt = split.txt[-length(split.txt)] # remove the last one which is a null entry
  
  author = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'author')))
  c_author = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'c_author')))
  c_author_address = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'c_author address')))
  title = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'title')))
  journal = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'journal')))
  language = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'language')))
  type = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'type')))
  keyword = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'keyword')))
  keyword.plus = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'keyword plus')))
  abstract = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'abstract')))
  address = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'address')))
  ref = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'reference')))
  cited = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'cited')))
  year = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'year')))
  page = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'page')))
  domain = unlist(lapply(split.txt, function(x)
    parser(x = x, item = 'domain')))
  
  keyword = unlist(lapply(keyword, function(x){
    if (!is.na(x)) {
      keyword = gsub("; ", ";", x)
      return(keyword)
    }else{return(NA)}
  }))
  
  country = unlist(lapply(address, function(address) {
    if (!is.na(address)) {
      c = strsplit(address, "\r")[[1]]
      c.output = unlist(lapply(c, function(x) {
        c.input = strsplit(x, ",")[[1]]
        c.input = c.input[length(c.input)]
        c.input = removeNumbers(c.input)
        c.input = gsub("\\.", "", c.input)
        c.input = trimws(c.input) # remove the first and last white space, require R 3.2.0
        c.input = gsub("[A-Z]{2}  USA", "USA", c.input)
        c.input = gsub("[A-Z] USA", "USA", c.input)
        c.input = gsub("[A-Z]USA", "USA", c.input)
        c.input_tmp = str_extract(c.input, "(?<=^ )[A-Z]*")
        if(!is.na(c.input_tmp)) c.input = c.input_tmp
        if (nchar(c.input) == 2)
          c.input = 'USA'
        return(c.input)
      }))
      
      c.output = paste(unique(c.output), collapse = ";")
      return(c.output)
    }else{
      NA
    }
  }))
  
  institution = unlist(lapply(address, function(address) {
    if (!is.na(address)) {
      a = strsplit(address, "\r")[[1]]
      a.output = unlist(lapply(a, function(x) {
        a.input = gsub("\\[.*\\]", "", x)
        a.input = trimws(a.input)
        a.input = strsplit(a.input, ",")[[1]][1]
        
        # get the country of the institution
        c.input = strsplit(x, ",")[[1]]
        c.input = c.input[length(c.input)]
        c.input = removeNumbers(c.input)
        c.input = gsub("\\.", "", c.input)
        c.input = trimws(c.input) # remove the first and last white space, require R 3.2.0
        c.input = gsub("[A-Z]{2}  USA", "USA", c.input)
        c.input = gsub("[A-Z] USA", "USA", c.input)
        c.input = gsub("[A-Z]USA", "USA", c.input)
        c.input_tmp = str_extract(c.input, "(?<=^ )[A-Z]*")
        if(!is.na(c.input_tmp)) c.input = c.input_tmp
        if (nchar(c.input) == 2)
          c.input = 'USA'
        
        a.input = paste(a.input, c.input, sep = "! ")
        return(a.input)
      }))
      
      a.output = paste(unique(a.output), collapse = ";")
      return(a.output)
    }else{
      NA
    }
  }))
  
  c_author_country = unlist(lapply(c_author_address, function(address) {
    if (!is.na(address)) {
      cac.input = strsplit(address, ",")[[1]]
      cac.input = cac.input[length(cac.input)]
      cac.input = removeNumbers(cac.input)
      cac.input = gsub("\\.", "", cac.input)
      cac.input = trimws(cac.input) # remove the first and last white space, require R 3.2.0
      cac.input = gsub("[A-Z]{2}  USA", "USA", cac.input)
      cac.input = gsub("[A-Z] USA", "USA", cac.input)
      cac.input = gsub("[A-Z]USA", "USA", cac.input)
      cac.input_tmp = str_extract(cac.input, "(?<=^ )[A-Z]*")
      if(!is.na(cac.input_tmp)) cac.input = cac.input_tmp
      if (nchar(cac.input) == 2)
        cac.input = 'USA'
      return(cac.input)
    }else{
      NA
    }
  }))
  
  c_author_institution = unlist(lapply(c_author_address, function(address) {
    if (!is.na(address)) {
      a.input = gsub("\\[.*\\]", "", address)
      a.input = trimws(a.input)
      a.input = strsplit(a.input, ",")[[1]][3]
      
      return(a.input)
    }else{
      NA
    }
  }))
  
  c_author_institution = paste(c_author_institution, c_author_country, sep = ", ")
  
  df = data.frame(
    author = as.character(author),
    c.author = as.character(c_author),
    c.author.country = as.character(c_author_country),
    title = as.character(title),
    journal = as.character(journal),
    language = as.character(language),
    type = as.character(type),
    keyword = as.character(keyword),
    keyword.plus = as.character(keyword.plus),
    abstract = as.character(abstract),
    country = as.character(country),
    institution = as.character(institution),
    c.author.institution = as.character(c_author_institution),
    ref = as.character(ref),
    cited = as.character(cited),
    year = year,
    page = page,
    domain = as.character(domain)
  )
  
  # if country is NA, but when c_author country is available, define country as c_author country
  df$country = as.character(df$country)
  df[which(is.na(df$country)), ]$country = "not" # "not" is a random string to let to match, which is odd
  df[which(df$country == "not"),]$country = as.character(df$c.author.country[which(df$country == "not")])
  
  # other case like when corresponding author did not appear in author address, need to add it into the author country
  a = as.character(df$c.author.country)
  b = as.character(df$country)
  n = length(a)
  tmp = NULL
  for(i in 1:n){
    tmp = c(tmp, grepl(a[i],b[i]))
  }
  
  df[which(tmp == 0), ]$country = paste(df[which(tmp == 0), ]$country, 
                                        df[which(tmp == 0), ]$c.author.country, 
                                        sep = ";")
  rm(tmp)
  rm(a)
  rm(b)
  rm(i)
  # end of this part

  df$inter_cooper = unlist(lapply(df$country, function(x) {
    if (is.na(x))
      return(NA)
    if (grepl(';', x))
      return(T)
    if (!grepl(';', x))
      return(F)
  }))
  
  #df$c.author.institution = gsub("!", ",", df$c.author.institution)
  df$institution = gsub("!", ",", df$institution)
  
  return(df)
}


createLink = function(val) {
  sprintf('<a href="https://scholar.google.com/scholar?q=%s" target="_blank" class="btn btn-primary">Link</a>',val)
}
