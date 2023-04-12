## utils

### load data
library(readxl)
schema <- readxl::read_xlsx("../vitisdatacrop/experimental_context_description.xlsx")
thesaurus <- readxl::read_xlsx("../vitisdatacrop/grapevine_experimental_thesaurus.xlsx")

### load library
library(dplyr)
library(kableExtra)
library(knitr)


#### functions
knit_table <- function(df,context,nchar_max=300)
{
  options(knitr.kable.NA = '')
  df$example_fr <- substr(df$example_fr,start=1,stop=nchar_max) # to limit text size to 500 characters
  sel<-(!is.na(df$example_fr)&nchar(df$example_fr)==nchar_max)
  df$example_fr[sel]<-paste0(df$example_fr[sel]," [...]") # to mark truncated texts
  df$description_fr[!is.na(df$source)]=paste0(df$description_fr[!is.na(df$source)]," [",df$source[!is.na(df$source)],"]") # to concatenate description and source
  df <- df %>%
    filter(subcontext==context) %>%
    select(label_fr,description_fr,example_fr,enum,priority,order) %>%
    #mutate_if(is.numeric,round,digits=1) %>%
    mutate(enum=gsub(x=enum,pattern=",",replacement="<br>")) %>%
    rename("Label"="label_fr",
           "Description"="description_fr",
           "Exemple"="example_fr",
           #"Type"="type",
           "Liste"="enum") %>%
    arrange(order)
  
  df %>% # mise en forme
    select(!"priority"&!"order") %>% # suppression colonne priority
    select(where(~ !(all(is.na(.)) | all(. == "")))) %>% # remove empty col
    knitr::kable(format="html",escape="F") %>%
    # "html", escape=F
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F) %>%
    row_spec(which(df$priority == 1), background = "white",bold=T)
}



