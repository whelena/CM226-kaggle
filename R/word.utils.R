split.text.to.words <- function(text) {
    # split text into rows of words
    words <- text %>% select(ID, Text) %>% unnest_tokens(word, Text)
    }

remove.words <- function(words, word.list){
    text.filt <- anti_join(words, word.list, by = 'word');
    }

extract.words  <- function(words, word.list, inverse = FALSE){
    word.list <- tolower(unique(word.list));
    word.df   <- data.frame(word = word.list);
    if (inverse) {
        text.filt <- words[!words$word %in% word.list, ];
    } else {
        text.filt <- words[words$word %in% word.list, ];
    }
    return(text.filt);
    }
