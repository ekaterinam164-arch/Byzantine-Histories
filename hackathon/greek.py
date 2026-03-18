import re

def add_chapter_tags(text):
    # Находим все числа в формате "число."
    numbers = re.finditer(r'\b\d+\.\s*', text)
    # Находим все вхождения слова "ΙΣΤΟΡΙΑ"
    history_matches = re.finditer(r'\bΙΣΤΟΡΙΑ\b', text)
    
    result = []
    last_pos = 0
    in_book_tag = False  # Флаг для отслеживания тегов book
    in_chapter = False   # Флаг для отслеживания глав
    
    # Добавляем начальный тег <book>
    result.append('\n<book>')
    in_book_tag = True
    
    # Преобразуем совпадения в списки для удобного перебора
    numbers_list = list(numbers)
    history_list = list(history_matches)
    
    i = 0  # Счетчик для чисел
    j = 0  # Счетчик для ΙΣΤΟΡΙΑ
    
    while i < len(numbers_list) or j < len(history_list):
        # Определяем ближайшую позицию
        if j < len(history_list) and (i >= len(numbers_list) or 
            history_list[j].start() < numbers_list[i].start()):
            # Добавляем текст до ΙΣΤΟΡΙΑ
            result.append(text[last_pos:history_list[j].start()])
            
            # Закрываем текущую главу перед новым book
            if in_chapter:
                result.append('</chapter>')
                in_chapter = False
            
            # Закрываем текущий book и открываем новый
            if in_book_tag:
                result.append('</book>\n<book>')
            
            # Добавляем само слово
            result.append(history_list[j].group())
            
            last_pos = history_list[j].end()
            j += 1
            in_book_tag = True
        else:
            # Добавляем текст до числа
            result.append(text[last_pos:numbers_list[i].start()])
            
            # Добавляем теги chapter
            if in_chapter:
                result.append('</chapter>\n<chapter>')
            else:
                result.append('\n<chapter>')
                in_chapter = True
            
            # Добавляем число
            result.append(numbers_list[i].group())
            
            last_pos = numbers_list[i].end()
            i += 1
    
    # Добавляем оставшийся текст
    result.append(text[last_pos:])
    
    # Закрываем последний chapter перед book
    if in_chapter:
        result.append('</chapter>')
    
    # Закрываем последний book
    if in_book_tag:
        result.append('</book>')
    
    return ''.join(result)

# Чтение файла
with open('Sim_fulltext.txt', "r", encoding='utf-8') as file:
    text = file.read()

# Обработка текста
processed_text = add_chapter_tags(text)

# Запись результата
with open('Sim_fulltext_chapters.xml', "w", encoding='utf-8') as file:
    file.write(processed_text)
