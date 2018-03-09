billionaires_data <- readRDS('./Data/billionaires_data.rds')
b_data <- readRDS('./Data/b_data.rds')

mydata_2018 <- readRDS('./Data/mydata_2018.rds')

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: visible;
content: 'There is no data within that factor.Please select a different range.'; }
}
"