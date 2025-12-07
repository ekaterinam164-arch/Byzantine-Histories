library(qpdf)
library(pdftools)
library(magick)
library(tidyverse)

trim_and_crop <- function(image_names){ 
  #на вход подается вектор названий картинок, см в конце, как это работает
  
  for (i in image_names){
    path <- paste('./', i, sep = '') 
    #записываем в path, чтобы эти картинки можно было открыть из папки
    processed <- image_read(path) |>
      image_trim(fuzz=60) 
    #эта функция автоматически отрезает фон, но иногда не справляется, в осн. все ошибки из-за этого
    
    
    pattern <- '(?<=_)[:digit:]+'
    page_number <- (str_extract(i, pattern))
    print(page_number)
    
    if (as.integer(page_number) %% 2 == 1){
      print('Обрезаю нечетную страницу...')
      processed <- image_crop(processed, geometry_area(width = 2650, 
                                                       height = 4100, 
                                                       x_off = 70, #это параметры, кот. я подобрала методом тыка, чтобы слева и справа отрезались чиселки
                                                       y_off = 100)
      )
    } else {
      print('Обрезаю четную страницу...')
      processed <- image_crop(processed, geometry_area(width = 2650, 
                                                       height = 4100, 
                                                       x_off = 230, 
                                                       y_off = 100))
    }
    image_write(processed, path = paste('./cropped_pages/', i, sep = ''), format = "png", density = "300x300") 
    #здесь мы сохраняем картинки в папку cropped_pages с исходными названиями
    
  }
  
}

#ПРИМЕР ИСПОЛЬЗОВАНИЯ, НАЗВАНИЯ ДОКУМЕНТОВ ВСТАВИТЬ СВОИ

#вырезаем нужные страницы из пдф документа и превращаем их в картинки
pdf_convert("./Симокатта.pdf", 
            format = "png", 
            dpi = 300,
            pages = 235:240,  # pages - страницы по нумерации pdf-документа
            filenames = paste0("Sim_", 218:223, ".png")) 
            #здесь имя картинки это Sim_51.png, 
            #51 - номер страницы именно с текстом - это надо самостоятельно посмотреть в издании!

#полученные картинки обрезаем, чтобы остался только осн. текст
#в рабочей директории нужно создать папку с названием cropped_pages

trim_and_crop(paste0("Sim_", 218:223, ".png"))
