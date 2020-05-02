test_model <- readModels('test/cov_lag_2.out')

# output that is not fully parsed is in parameters
View(test_model[["parameters"]][["unstandardized"]])


# Running mplus parsing, homebrew --------------

# define var names to look for in output file (used in regexps)
varnames = c("se_l1", "ji_qt_l1")


# read in file
mplus_output <- convert_mplus('test/cov_lag_2.out')

# discard useless stuff...
mplus_output <- mplus_output %>% filter(section == 'alternative parameterizations for the categorical latent variable regression')

# line type parser
mlpa_cov_output <- mplus_parameters_parser(mplus_output$output, mplus_output$line_type)



# tests shizzle --------------

mplus_output[6,1]
stri_remove_empty(mplus_output[6,1] %>% flatten() %>% flatten())

# classifies lines according to regexp.
clusternames = c("bc", "wc")
varnames_grouping <- str_c("(",varnames, ")")


# categorizer
y <- "(se_l1)|(ji_qt_l1)"
example_line <- " wc#3 on"
example_line2 <- " se_l1 0.267 0.492 0.542 0.588"

str_detect(example_line2, "(se_l1)|(ji_qt_l1)")
str_detect(example_line, "(se_l1)|(ji_qt_l1)")

mplusout_pretidy$output[[1]]

# use this to copy out the category names
reference_line <- str_c(mplus_output$output[[26]], collapse = " ")
str_extract(reference_line, "\\d")
reference_line <- str_c(mplus_output$output[[93]], collapse = " ")
str_extract(reference_line, "wc#\\d")
