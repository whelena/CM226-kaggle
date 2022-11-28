get.tf.idf <- function(word.df, matrix = FALSE) {
    # word.df <- train.word[c(1:100, 3375:3385, 6222:6322),]
    freq <- count(word.df, ID, word);
    tf.idf <- bind_tf_idf(freq, word, ID, n);
    if (matrix) {
        tf.idf.mat <- convert.df2array(
            DF = tf.idf,
            value = 'tf_idf',
            x.axis = 'word',
            yaxis = 'ID'
            );
        return(tf.idf.mat);
        }
    return(tf.idf);
    }
