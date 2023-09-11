## utils

### load data
library(readxl)

loadXLSX_url <- function(url) {
  temp_file <- tempfile(fileext = ".xlsx")
  download.file(url = url, destfile = temp_file, mode = "wb", quiet = TRUE)
  readxl::read_xlsx(temp_file)
}


schema_url<-"https://github.com/vignevin/vitisdatacrop/blob/main/experimental_context_description.xlsx?raw=true"
thesaurus_url<-"https://github.com/vignevin/vitisdatacrop/blob/main/grapevine_experimental_thesaurus.xlsx?raw=true"

schema <- loadXLSX_url(schema_url)
thesaurus <- loadXLSX_url(thesaurus_url)

### load library
library(dplyr)
library(kableExtra)
library(knitr)


#### functions
knit_table <- function(df,entity_selected,nchar_max=300,caption="")
{
  options(knitr.kable.NA = '')
  df$example <- substr(df$example,start=1,stop=nchar_max) # to limit text size to 500 characters
  sel<-(!is.na(df$example)&nchar(df$example)==nchar_max)
  df$example[sel]<-paste0(df$example[sel]," [...]") # to mark truncated texts
  df$description[!is.na(df$source)]=paste0(df$description[!is.na(df$source)]," [",df$source[!is.na(df$source)],"]") # to concatenate description and source
  df <- df[grep(entity_selected,strsplit(df$name, split=",", fixed = TRUE)),]
  df <- df %>%
    #filter(name == entity_selected) %>%
    filter(core == "true") %>%
    select(label_fr,property,description,example,enumList,priority,order) %>%
    #mutate_if(is.numeric,round,digits=1) %>%
    mutate(enumList=gsub(x=enumList,pattern=",",replacement="<br>")) %>%
    mutate(label_fr=paste0(label_fr,"<br>(",property,")")) %>%
    rename("Label (nom)"="label_fr",
           #"Nom" = "property",
           "Description"="description",
           "Exemple"="example",
           #"Type"="type",
           "Liste"="enumList") %>%
    arrange(order)
  
  df %>% # mise en forme
    select(!"priority"&!"order"&!"property") %>% # suppression colonne priority
    select(where(~ !(all(is.na(.)) | all(. == "")))) %>% # remove empty col
    knitr::kable(format="html",escape="F",caption=caption) %>%
#    unclass() %>% cat()
    # "html", escape=F
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F) %>%
    row_spec(which(df$priority == 1), background = "white",bold=T)
}



