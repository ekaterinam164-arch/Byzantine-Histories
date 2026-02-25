library(tesseract)
library(ellmer)
library(ollamar)
library(tidyverse)

book_title <- 'Симокатта.pdf'
book_id <- 'Sim_'
pages_pdf <- 91:126
pages_book <- pages_pdf + 19


pdf_convert(book_title, 
            format = "png", 
            dpi = 300,
            pages = pages_pdf,  # pages - страницы по нумерации pdf-документа
            filenames = paste0(book_id, pages_book, ".png"))

# пути к картинкам (измените имя папки, если надо)
my_pngs <- list.files("./pages", pattern = '.png', full.names = TRUE)
my_pngs

# подключаем LLM

system_prompt <- "Ты - эксперт по древнегреческим текстам. 
1. Исправь OCR-ошибки в распознанном древнегреческом тексте, сверяясь с изображением
2. Сохраняй оригинальное форматирование (переносы строк, абзацы), а также знаки переноса (дефисы)
3. Удаляй критический аппарат, маргиналии, номера страниц, колонтитулы (как на греческом, так и на латыни) 
4. На полях могут встречаться латинские символы и цифры, неверно распознанные как греческие символы, их надо аккуратно удалить. 
5. Не добавляй и не удаляй слова в самом тексте - только исправляй ошибки распознавания. 
5. Сохраняй диакритические знаки (придыхания, ударения)
6. Если не уверен в исправлении - оставь как есть"


# ollamar::pull("bobowg/gemini-3-flash")

chat <- chat_ollama(
  system_prompt = system_prompt,
  # model = "gpt-oss:120b-cloud"
  # model = "gemma3:27b-cloud"
  model = "bobowg/gemini-3-flash"
)

#chat$chat(ocr_bgrc,
#          content_image_file(my_pngs[1]))


### ---- теперь поставим наше решение на конвейер ---- ###

# Создаём безопасные версии функций с возвратом списка (result, error)
safe_ocr <- safely(~ tesseract::ocr(.x, engine = "grc"), otherwise = NA_character_)

# Для LLM запроса: безопасная версия, которая принимает chat, текст и путь к изображению
safe_llm <- function(chat, text, image_path) {
  safely(~ chat$chat(text, content_image_file(image_path)), otherwise = NA_character_)()
}

# Функция обработки одного файла
process_one <- function(png_path, system_prompt) {
  id <- tools::file_path_sans_ext(basename(png_path))
  
  # --- OCR этап ---
  ocr_result <- safe_ocr(png_path)
  ocr_text <- ifelse(is.null(ocr_result$error), ocr_result$result, NA_character_)
  ocr_error <- ifelse(is.null(ocr_result$error), NA_character_, as.character(ocr_result$error))
  
  # Если OCR не удался, возвращаем запись с ошибкой и не вызываем LLM
  if (!is.null(ocr_result$error)) {
    return(tibble(
      id = id,
      original_ocr = ocr_text,
      corrected_llm = NA_character_,
      ocr_error = ocr_error,
      llm_error = "LLM not called due to OCR error"
    ))
  }
  
  # Небольшая пауза
  Sys.sleep(1)
  
  # --- LLM этап (новый чат для каждого файла) ---
  chat <- chat_ollama(system_prompt = system_prompt, model = "bobowg/gemini-3-flash")
  
  llm_result <- safe_llm(chat, ocr_text, png_path)
  corrected <- ifelse(is.null(llm_result$error), llm_result$result, NA_character_)
  llm_error <- ifelse(is.null(llm_result$error), NA_character_, as.character(llm_result$error))
  
  # Возвращаем итоговую строку
  tibble(
    id = id,
    original_ocr = ocr_text,
    corrected_llm = corrected,
    ocr_error = ocr_error,
    llm_error = llm_error
  )
}

# Применяем ко всем файлам с индикатором прогресса 
# ETA = Estimated Time of Arrival
results <- map_dfr(my_pngs, ~ process_one(.x, system_prompt), .progress = TRUE)

#Sim_results <- results 
Sim_results <- bind_rows(Sim_results, results) #это для того, чтобы можно было распознавать порциями

res_copy <- results
Sim_copy <- Sim_results

# Как вариант: записать каждую страницу в отдельный txt
walk2(
  .x = results$corrected_llm,
  .y = results$id,
  .f = ~ {
    # Пропускаем, если текст отсутствует (NA)
    # NB: имя папки укажите свое!
    if (!is.na(.x)) {
      readr::write_lines(.x, file.path("./texts/", paste0(.y, ".txt")))
    }
  }
)

# Сохранить в CSV
write_csv(Sim_results, "ocr_corrections.csv")

