library(tesseract)
library(tesseractgt)

create_gt_txt(
  folder = "./grc-ground-truth",
  extension = "png", 
  engine = tesseract::tesseract(language = "grc")
)

fix_gt_txt <- function(folder) {
  txts <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)
  for (f in txts) {
    # читаем файл
    x <- readLines(f, warn = FALSE, encoding = "UTF-8")
    
    # если пусто → оставляем пустую строку
    if (length(x) == 0) {
      one_line <- ""
    } else {
      # склеиваем все строки через пробел
      one_line <- paste(x, collapse = " ")
      # убираем лишние пробелы
      one_line <- trimws(one_line)
    }
    
    # записываем обратно в файл
    writeLines(enc2utf8(one_line), f, useBytes = TRUE)
  }
  message("Готово: все txt-файлы приведены к одной строке")
}

# запускаем
fix_gt_txt("./grc-ground-truth") # ваш путь к файлу

correct_gt_txt()
