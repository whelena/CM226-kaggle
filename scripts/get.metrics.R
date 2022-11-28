devtools::load_all('.');

train.var <- read.csv('data/training_variants');
###################################################################################################
# get TF-IDF for all words
###################################################################################################
load(file = 'result/processed_train_word.rda');
tf.idf <- get.tf.idf(train.word);
tf.idf <- merge(train.var[, c('ID', 'Class')], tf.idf, by = 'ID');
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

# get summary stats
tf.idf <- tf.idf[tf.idf$n > min.n, ];
word.sum.df <- group_by(tf.idf, word) %>%
    mutate(
        mean.tf.idf     = mean(tf_idf, na.rm = TRUE),
        sd.tf.idf       = sd(tf_idf, na.rm = TRUE),
        median.tf.idf   = median(tf_idf, na.rm = TRUE),
        mean.tf         = mean(tf, na.rm = TRUE),
        sd.tf           = sd(tf, na.rm = TRUE),
        median.tf       = median(tf, na.rm = TRUE),
        total.n         = sum(n),
        log10.total.n   = log10(total.n),
        n.class         = length(unique(Class)),
        n.id            = length(unique(ID))
        )  %>%
    ungroup() %>%
    select(-c(ID, Class, n, tf, idf, tf_idf)) %>% 
    unique() %>%
    arrange(desc(total.n)) %>%
    as.data.frame;
save.df(df = word.sum.df , fname = paste0('result/word_summary_min', min.n, '.tsv'));

for (col in c('total.n', 'n.id', 'median.tf', 'median.tf.idf')) {
    create.histogram(
        filename = paste0('result/plot/hist_', col, '.png'),
        x = word.sum.df[[col]],
        type = 'count',
        xlab.label = paste('log10', col),
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        ylab.label = 'Frequency',
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1
        );
    }

###################################################################################################
# get TF-IDF for genes only
###################################################################################################
load(file = 'result/train_genes.rda');
tf.idf.genes <- tf.idf[tf.idf$word %in% train.genes$word, ];

tf.idf.genes.mat <- convert.df2array(
            DF = tf.idf.genes,
            value = 'tf_idf',
            x.axis = 'word',
            y.axis = 'ID'
            );
#save.df(df = tf.idf.genes.mat, fname = paste0('result/train_genes_matrix_tf_idf_min', min.n, '.tsv'));
save(tf.idf.genes.mat, file = "result/tf_idf_genes_mat.rda");


# tf.idf.genes.only <- get.tf.idf(train.genes);
# tf.idf <- get.tf.idf(train.word);
# tf.idf <- merge(train.var[, c('ID', 'Class')], tf.idf, by = 'ID');
