split.text.to.words <- function(text) {
    # split text into rows of words
    words <- text %>% select(ID, Text) %>% unnest_tokens(word, Text)
    }

remove.words <- function(words, word.list){
    text.filt <- anti_join(words, word.list, by = 'word');
    }

extract.words  <- function(words, word.list){
    word.list <- tolower(unique(word.list));
    word.df   <- data_frame(word = word.list);
    text.filt <- semi_join(words, word.df, by = 'word');
    return(unique(text.filt));
    }
