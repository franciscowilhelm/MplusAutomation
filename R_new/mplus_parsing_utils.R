# utilities for parsing mplus output

# Levels of nesting:
# level 1: sections
# level 2: classes
# level 3: line types

# section parser only needs to understand alternative parameterizations
# define section headers

section_headers <- c("alternative parameterizations for the categorical latent variable regression",
                     "odds ratio for the alternative parameterizations for the categorical latent variable regression",
                     "quality of numerical results")


mplus_section_parser <- function(mplustxt, chunknames) {
  # chunkpositions <- map(chunknames, ~str_detect(mplustxt, .x)) #too fuzzy, look for exact matches
  chunkpositions <- map(chunknames, function(x) mplustxt == x) # exact matches
  sectioncol <- vector(mode = "character", length = length(mplustxt))
  sectioncol[sectioncol == ""] <- NA
  for(chunk in seq_along(chunknames)) {
    sectioncol[chunkpositions[[chunk]] == TRUE] <- chunknames[chunk]
  }
  
  return(sectioncol)
}

# takes file, converts, creates sections
convert_mplus <- function(file) {
  out <- read.delim(file, stringsAsFactors = FALSE)
  names(out) <- "output"
  out <- tibble(output = tolower(out$output)) %>% mutate(linenumber = row_number())
  
  # generate section header column
  out$section <- mplus_section_parser(out$output, section_headers)
  #fill all section rows with corresponding section 
  out <- out %>% tidyr::fill(section,  .direction = "down")
  
  # because tidytext is unsutaible for mplus output, define another chain of string splits and trimmings
  out$output <- map(out$output, ~mplus_line_parser(.x))
  out <- out %>% mutate(line_type = map(out$output, ~mplus_line_classifier(.x)))
  # leads to a weird structure of output column but ok ...
  return(out)
}


# parses lines, splitting them into words/elements

mplus_line_parser <- function(line) {
  stri_split_boundaries(line) %>% flatten() %>% str_trim(side = "both")
}

# line classifier

## define line types
line_classifier_options <- tibble(type = c("class_regressed",
                                           "parameters",
                                           "refclass"),
                                  regexpr = c("\\bon\\b",
                                              str_c(varnames_grouping, collapse =  "|"),
                                              "parameterization using reference class.\\d"))


## classifies lines function
mplus_line_classifier <- function(line) {

  line_c <- str_c(line, collapse = " ")
  classified <- map2_chr(line_classifier_options$type, line_classifier_options$regexpr,
           function(x, y) {
             om <- "om"
             return(ifelse(any(str_detect(line_c, y)), x, NA))
             })
  
  classified <- classified[!is.na(classified)] #insert the one which is not NA
  if(is_empty(classified)) {
    classified <- "unclassified"
  }  #character(0) to unclassified
  return(classified)
}  
  
# parses input lines line_type-specific

mplus_parameters_parser <- function(lines, line_type) {
  # precreate df
  df <- tibble(ref_class = NA, y_class = "",
                param = NA, estimate = NA, se = NA, est_se = NA, pval = NA, .rows = sum(line_type == "parameters"))
  p <- 1 #holds the current row for passing of parameter to df, which is unequal the current text line
  
  # go thourhg line by line
  for(l in 1:113) {
    if(line_type[l] == "refclass") {
      line_c <- lines[[l]] %>% str_c(collapse = " ")
      ref_class = str_extract(line_c, "\\d")    
    }
    if(line_type[l] == "class_regressed") {
      line_c <- lines[[l]] %>% str_c(collapse = " ")
      y_class = str_extract(line_c, "wc#\\d")
    }
    if(line_type[l] == "parameters") {
      line <- stri_remove_empty(lines[[l]])
      df[p,] <- tibble(ref_class = ref_class, y_class = y_class,
                   param = line[1], estimate = line[2], se = line[3], est_se = line[4], pval = line[5])
      p <- p+1
    }
  }
  df <- df %>% mutate_at(vars(estimate, se, est_se, pval), list(~as.numeric(.))) #convert some columns
  return(df)
}
