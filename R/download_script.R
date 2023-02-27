### libraries
library(rvest)

### FIESP
dir.create("FIESP")

eds <- c(3:40)
ent <- "FIESP"

for (i in 1:length(eds)){
dir.create(paste0(ent, "/", eds[i]))

Sys.sleep(1)
  
download.file(url = paste0("http://www.sindicatosemacao.com.br/edicao/", eds[i], "/files/assets/common/downloads/page0003.pdf"), 
              destfile = paste0(ent, "/", eds[i], "/", "003.pdf"),
              method = "curl"
              )

Sys.sleep(2)

download.file(url = paste0("http://www.sindicatosemacao.com.br/edicao/", eds[i], "/files/assets/common/downloads/page0004.pdf"),
              destfile = paste0(ent, "/", eds[i], "/", "004.pdf"), 
              method = "curl"
              )

Sys.sleep(2)

}

### FIESC

ent <- "FIESC"
eds <- 4:27

dir.create("FIESC")

links <- read_html("https://fiesc.com.br/revista/edicoes") %>% html_nodes(".link--estilizado") %>% html_attr("href")
links <- links[grep("pdf$", links)]
links <- rev(links)

for (i in 1:length(links)){
  
  download.file(url = links[i],
                destfile = paste0(ent, "/", "IC", eds[i], ".pdf"),
                method = "curl"
                )
  
  Sys.sleep(2)
}

### FIEC

ent <- "FIEC"
eds <- 89:147

dir.create("FIEC")

links <- read_html("https://www1.sfiec.org.br/sobre-nos/revista-da-fiec") %>% html_nodes(".capa_ a") %>% html_attr("href")
links <- links[grep("pdf$", links)]
links <- rev(links)

for (i in 43:length(links)){
  
  download.file(url = links[i],
                destfile = paste0(ent, "/", "RevFIEC", eds[i], ".pdf"),
                method = "curl"
  )
  
  Sys.sleep(2)
}

### FIEAM

# library(PressScrape)

library(rvest)
library(magick)
library(dplyr)

ISUU_get_link_jpg <- function(page) {
  link <- rvest::read_html(page) %>% rvest::html_nodes("meta") %>% rvest::html_attr('content')
  link <- unique(link[grep("jpg$", link)])
  return(link)
}

convert_jpg_cutting <- function(page){
  org_text <- magick::image_read(page)
  text_info <- magick::image_info(org_text)
  
  finished <- F
  
  while (isFALSE(finished)){
    
    print(org_text)
    
    cut_chos <- readline("Provide your cutting input: ")
    
    text <- magick::image_crop(image = org_text, geometry =  cut_chos, repage = T)
    
    print(text)
    
    finished <- as.logical(readline("Is this cut good enough? Answer as boolean value: "))
    
  }
  
  text <- magick::image_ocr(image = text, language = "por")
  
  return(text)
}

FIEAM_isuu <- read.table("FIEAM/isuu_links.txt")

links <- unlist(lapply(FUN = ISUU_get_link_jpg, X = FIEAM_isuu$V1))

links <- gsub("page_1", "page_3", links)

FIEAM_text <- character(length(links))
for (i in 1:length(links)){
FIEAM_text[i] <- convert_jpg_cutting(links[i]) # Tenho que arrumar o código para ele dar conta de textos com mais de uma coluna
}

FIEAM <- data.frame(Entidade = rep("FIEAM", 7), Texto = FIEAM_text[1:7])

### FIESC

fiesc <- pdf_render_page(pdf = "FIESC/IC20.pdf", page = 2, dpi = 120) %>% image_read()
image_composite(image = fiesc, composite_image = image_read("blackscreen.jpg"), offset = "800x500")

  %>% convert_jpg_cutting()
