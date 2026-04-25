library(tesseract)
library(stringdist)
library(tidyverse)

# Install via devtools or remotes
remotes::install_github("jenswaeckerle/wersim")
library(wersim)
library(quanteda)

#greek <- tesseract('grc')
#bgreek <- tesseract('bgrc')


#в папке для каждой страницы лежит обрезанное изображение .png, наш итоговый текст и tlg-текст
page_id <- list.files("./", pattern = '[0-9].txt', full.names = TRUE)
page_id <- gsub('.txt', '', page_id)

#регулярными выражениями убираем лишние цифры, пробелы и переносы, чтобы они не отражались на ошибке
clean_tlg_text <- function(text){

  #убираем числа в скобках
  text <- gsub('\\(\\d+\\) ?', '', text)
  #убираем числа с точкой - номера глав
  text <- gsub('\\d+.', '', text)
  #убираем переносы строки
  text <- gsub('\\n', ' ', text)
  #убираем оставшиеся знаки переноса
  text <- gsub('- ?', '', text)
  #убираем всякий другой мусор
  text <- gsub('@\\d?', '', text)
  #убираем лишние пробелы
  text <- gsub(' +', ' ', text)
}

#функция собирает все тексты, которые нужно сравнить
#готовые тексты - наш финальный и из тлг
#также внутри функции работают модели Тессеракт - базовая и наша дообученная
one_row <- function(id) {
  
  page_id <- id
  txt_name <- paste0(id, '.txt', sep='')
  bgrc_llm_text <- paste(readLines(txt_name),
                     collapse = "\n")
  
  tlg_name <- paste0(id, '_tlg.txt', sep='')
  tlg_text <- paste(readLines(tlg_name),
                    collapse = "\n")
  
  png_name <- paste0(id, '.png', sep='')
  grc_text <- ocr(png_name, engine = 'grc')
  bgrc_text <- ocr(png_name, engine = 'bgrc')
  
  tibble(
    id = page_id,
    grc_text = clean_tlg_text(grc_text),
    bgrc_text = clean_tlg_text(bgrc_text),
    bgrc_llm_text = clean_tlg_text(bgrc_llm_text),
    tlg_text = clean_tlg_text(tlg_text)
  )
  
}

#функция подсчета word error rate, возвращает число в процентах (от 1 до 100)
count_wer <- function(tlg, sample, id){
  
  hypothesis_data=data.frame(text=sample,
                             name=id,stringsAsFactors = F)
  hypothesis_corpus=quanteda::corpus(hypothesis_data,docid_field = "name", text_field = "text")
  reference_data=data.frame(text=tlg,
                            name=id,stringsAsFactors = F)
  reference_corpus=quanteda::corpus(reference_data,docid_field = "name", text_field = "text")
  result <- wer(r=reference_corpus,h=hypothesis_corpus)
  return(result$wer*100)
}

#собираем нужные тексты в тиббл
text_samples <- map_df(page_id, one_row)

#считаем метрики
#abs - абсолютное значение (для ошибок в символах)
#pct - число в процентах (от 0 до 100)
text_samples <- text_samples |>
  mutate(grc_CER_abs = stringdist(grc_text, tlg_text, method = 'lv')) |>
  mutate(grc_CER_pct = grc_CER_abs*100/nchar(tlg_text))

text_samples <- text_samples |>
  mutate(bgrc_CER_abs = stringdist(bgrc_text, tlg_text, method = 'lv')) |>
  mutate(bgrc_CER_pct = bgrc_CER_abs*100/nchar(tlg_text))

text_samples <- text_samples |>
  mutate(bgrc_llm_CER_abs = stringdist(bgrc_llm_text, tlg_text, method = 'lv')) |>
  mutate(bgrc_llm_CER_pct = bgrc_llm_CER_abs*100/nchar(tlg_text))

text_samples <- text_samples |>
  mutate(grc_WER_pct = count_wer(tlg_text, grc_text, id)) |>
  mutate(bgrc_WER_pct = count_wer(tlg_text, bgrc_text, id)) |>
  mutate(bgrc_llm_WER_pct = count_wer(tlg_text, bgrc_llm_text, id))

#save(text_samples, file = 'text_samples.RData')
