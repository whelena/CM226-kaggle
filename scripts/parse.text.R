###################################################################################################
# SETUP
###################################################################################################
devtools::load_all('.');

train.var <- read.csv('data/training_variants');
test.var  <- read.csv('data/test_variants');

train.txt.dump  <- tibble(text = read_lines('data/training_text', skip = 1));
train.txt       <- train.txt.dump %>% separate(text, into = c('ID', 'Text'), sep = '\\|\\|');
train.txt       <- train.txt %>% mutate(ID = as.integer(ID));

test.txt.dump   <- tibble(text = read_lines('data/test_text', skip = 1));
test.txt        <- test.txt.dump %>% separate(text, into = c('ID', 'Text'), sep = '\\|\\|');
test.txt        <- test.txt %>% mutate(ID = as.integer(ID));

# filter for null text and split into words
train.txt       <- train.txt[nchar(train.txt$Text) > 10, ];
train.word      <- split.text.to.words(text = train.txt);
###################################################################################################
# remove stop words from text files
###################################################################################################
# load stop words from tidytext and custom file
data('stop_words');
custom.stopwords <- data_frame(read.table('data/custom.stopwords.txt', header = TRUE));
numbers          <- data.frame(word = as.character(1:100));
custom.stopwords <- rbind(stop_words[, 'word'], custom.stopwords, numbers);
train.word       <- remove.words(words = train.word, word.list = custom.stopwords);
train.word       <- merge(train.var[, c('ID', 'Class')], train.word, by = 'ID');
word.frequency   <- table(train.word$word) |> sort(decreasing = TRUE);

###################################################################################################
# get gene per class frequency
###################################################################################################
# extract genes that are in the training and testing set
train.genes  <- extract.words(words = train.word, word.list = c(train.var$Gene, test.var$Gene));
train.genes  <- merge(train.var[, c('ID', 'Class')], train.genes, by = 'ID');
IDs.no.genes <- train.var$ID[!train.var$ID %in% unique(train.genes$ID)];

gene.count              <- count(train.genes, word, Class);
gene.count.per.class    <- spread(gene.count, word, n);
gene.count.per.class[is.na(gene.count.per.class)] <- 0;
gene.frequency          <- sort(colSums(gene.count.per.class), decreasing = TRUE);
gene.plot               <- get.top.features(df = gene.count, feature.column = 'word', top.n = 20);

barplot.top.feature(
    df = gene.plot,
    group = 'Class',
    # barplot arguments
    formula = n ~ word,
    filename = 'result/plot/gene_barplot.png',
    xaxis.lab = unique(gene.plot$word),
    xlab.label = 'Gene names',
    ylab.label = 'Frequency',
    height = 5,
    width = 10
    );
save.df(df = gene.count, fname = 'result/train_gene_count.tsv');
###################################################################################################
# get TF-IDF
###################################################################################################
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

tf.idf <- get.tf.idf(train.word);
save.df(df = tf.idf, fname = 'result/train_tf_idf.tsv');

min.n <- 2;
tf.idf.mat <- convert.df2array(
            DF = tf.idf[tf.idf$n > min.n, ],
            value = 'tf_idf',
            x.axis = 'word',
            y.axis = 'ID'
            );
save.df(df = tf.idf.mat, fname = paste0('result/train_matrix_tf_idf_min', min.n, '.tsv'));

tf.mat <- convert.df2array(
            DF = tf.idf[tf.idf$n > min.n, ],
            value = 'tf',
            x.axis = 'word',
            y.axis = 'ID'
            );
save.df(df = tf.mat, fname = paste0('result/train_matrix_frequency_min', min.n, '.tsv'));