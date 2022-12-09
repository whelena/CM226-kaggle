devtools::load_all('.');

name <- 'test';
var  <- read.csv(file.path('data', paste0(name, '_variants')));
###################################################################################################
# get TF-IDF for all words
###################################################################################################
load(file = file.path('result', 'parsed.text', paste0(name, '.rda')));
tf.idf <- get.tf.idf(word.df);
if (name == 'training') {
    tf.idf <- merge(var[, c('ID', 'Class')], tf.idf, by = 'ID');
    }
save.df(df = tf.idf, fname = file.path('result', 'matrix', paste(name, 'tf_idf.tsv', sep = '_')));

min.n <- 2;
tf.idf.mat <- convert.df2array(
            DF = tf.idf[tf.idf$n > min.n, ],
            value = 'tf_idf',
            x.axis = 'word',
            y.axis = 'ID'
            );
# zero.prop <- apply(tf.idf.mat, 2, function(x) sum(x == 0) / length(x));
# sub.tf.idf.mat <- tf.idf.mat[, which(arr.ind = TRUE)]
#save.df(df = tf.idf.mat, fname = paste0('result/train_matrix_tf_idf_min', min.n, '.tsv'));
save(tf.idf.mat, file = file.path('result', 'matrix', paste(name, 'tf_idf_mat.rda', sep = '_')));

tf.mat <- convert.df2array(
            DF = tf.idf[tf.idf$n > min.n, ],
            value = 'tf',
            x.axis = 'word',
            y.axis = 'ID'
            );
save.df(df = tf.mat, fname = file.path('result', 'matrix', paste(name, 'tf_mat.rda', sep = '_')));
###################################################################################################
# get TF-IDF for genes only
###################################################################################################
genes <- read.csv(file.path('data', 'genes.txt'));
tf.idf.genes <- tf.idf[tf.idf$word %in% tolower(genes$x), ];

tf.idf.mat <- convert.df2array(
            DF = tf.idf.genes,
            value = 'tf_idf',
            x.axis = 'word',
            y.axis = 'ID'
            );
save(tf.idf.mat, file = file.path('result', 'matrix', paste(name, 'genes_tf_idf_mat.rda', sep = '_')));

###################################################################################################
# get TF-IDF for feature selection
###################################################################################################
if (name == 'training') {
    tf.idf.class <- get.tf.idf(word.df,  class = TRUE);
    # tf.idf.class <- merge(var[, c('ID', 'Class')], tf.idf.class, by = 'ID');
    save.df(
        df = tf.idf.class,
        fname = file.path('result', 'parsed.text', paste(name, 'class_tf_idf.tsv', sep = '_'))
        );
    tf.idf.mat <- convert.df2array(
        DF = tf.idf.class,
        value = 'tf_idf',
        x.axis = 'word',
        y.axis = 'Class'
        );
    save(
        tf.idf.mat,
        file = file.path('result', 'matrix', paste(name, 'class_tf_idf_mat.rda', sep = '_'))
        );
    }
