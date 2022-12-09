###################################################################################################
# Load stuff
###################################################################################################
devtools::load_all('.');
library(widyr);

train.df <- read.table(
    file = file.path('result', 'matrix', 'training_tf_idf.tsv'),
    header = TRUE,
    sep = '\t'
    );

test.df <- read.table(
    file = file.path('result', 'matrix', 'test_tf_idf.tsv'),
    header = TRUE,
    sep = '\t'
    );

get.correlation <- function(DF) {
    corr <-  group_by(DF, Class) %>%
        select(-Class) %>%
        pairwise_cor(Class, word, tf_idf, upper = FALSE, diag = TRUE)
    }

sus.class <- c(1, 2, 3, 4, 5, 7);
train.corr <- get.correlation(na.omit(train.df));
summary(train.corr$correlation[train.corr$correlation < 0.99])
summary(train.corr$correlation[train.corr$item1 %in% sus.class & train.corr$item2 %in% sus.class & train.corr$correlation < 0.99])
summary(train.corr$correlation[!(train.corr$item1 %in% sus.class & train.corr$item2 %in% sus.class) & train.corr$correlation < 0.99])
train.corr[train.corr$correlation > 0.072, ];

test.corr <- get.correlation(na.omit(test.df));
summary(test.corr$correlation[test.corr$item1 %in% sus.class & test.corr$item2 %in% sus.class & test.corr$correlation < 0.99])
summary(test.corr$correlation[!(test.corr$item1 %in% sus.class & test.corr$item2 %in% sus.class) & test.corr$correlation < 0.99])
