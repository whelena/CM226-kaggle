devtools::load_all('.');

train.var <- read.csv('data/training_variants');
test.var  <- read.csv('data/test_variants');

train.txt.dump  <- tibble(text = read_lines('data/training_text', skip = 1));
train.txt       <- train.txt.dump %>% separate(text, into = c('ID', 'Text'), sep = '\\|\\|');
train.txt       <- train.txt %>% mutate(ID = as.integer(ID));

test.txt.dump   <- tibble(text = read_lines('data/test_text', skip = 1));
test.txt        <- test.txt.dump %>% separate(text, into = c('ID', 'Text'), sep = '\\|\\|');
test.txt        <- test.txt %>% mutate(ID = as.integer(ID));


# try to get abbreviations by searching for ()
pattern <- '\\(([^()]+)\\)'
abbreviation <- gsub(pattern, '\\1', str_extract_all(train.txt[1, 2], pattern)[[1]])
# doesn't really work :(

# remove stop words from text files
data('stop_words') # load stop words from tidytext
custom.stopwords <- data_frame(read.table('data/custom.stopwords.txt', header = TRUE))
custom.stopwords <- rbind(stop_words[, 'word'], custom.stopwords);



train.word  <- split.text.to.words(text = train.txt);
train.word  <- remove.words(words = train.word, word.list = custom.stopwords);
# extract genes that are in the training and testing set
train.genes <- extract.words(words = train.word, word.list = c(train.var$Gene, test.var$Gene));

# bind class info to train.txt;
train.genes <- merge(train.var[, c('ID', 'Class')], train.genes, by = 'ID');

gene.count <- count(train.genes, word, Class);
gene.count.per.class <- spread(gene.count, word, n);
gene.count.per.class[is.na(gene.count.per.class)] <- 0;
gene.frequency <- sort(colSums(gene.count.per.class), decreasing = TRUE);


gene.plot <- get.top.features(df = gene.count, feature.column = 'word', top.n = 20);


gene.plot <- gene.count[gene.count$word %in% names(gene.frequency)[1:20], ];
gene.plot$Class <- factor(gene.plot$Class, levels = 1:9);

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
<<<<<<< Updated upstream
    )
=======
    );
save(train.genes, file = "result/train_genes.rda");
###################################################################################################
# remove plurals from non genes
###################################################################################################
train.no.genes      <- train.word[train.word$word %in% unique(train.genes$word), ];
plural.map          <- singularize(unique(train.no.genes$word));
names(plural.map)   <- unique(train.no.genes$word);
train.no.genes$word <- plural.map[match(train.no.genes$word, names(plural.map))]
train.word <- rbind(train.no.genes, train.genes);

save(train.word, file = "result/processed_train_word.rda");

###################################################################################################
# PCA
###################################################################################################
library(ggfortify)
library(ggplot2)

# Try selecting 2 or 3 classes that are most dissimilar and see if we can 
# capture more variance in the PCA.

n <- nrow(tf.idf.mat);
select_subset_idxs <- runif(n, 1, nrow(tf.idf.mat));
tf.idf.mat.subset  <- tf.idf.mat[select_subset_idxs,];

tf.idf.pca <- prcomp(tf.idf.mat.subset);

autoplot(tf.idf.pca, data = tf.idf.mat.subset)

var_explained = tf.idf.pca$sdev^2 / sum(tf.idf.pca$sdev^2)

scree_df <- data.frame(pc = c(1:10), var_explained = var_explained[1:10]);
ggplot(data = scree_df, mapping = aes(x = pc, y = var_explained)) + 
  geom_point() + 
  geom_line() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#####################################
load('training_tf_idf_mat.rda')

rowSums(tf.idf.mat)
numZeros <- apply(tf.idf.mat, 1, function(x) sum(x == 0) < (0.5 * ncol(tf.idf.mat)));
keep_idx <- c();
for(i in 1:length(numZeros)){
  if(numZeros[i] == TRUE){
    keep_idx <- c(keep_idx, i);
  }
}
tf.idf.mat.subset <- tf.idf.mat[keep_idx, ];

tf.idf.pca <- prcomp(tf.idf.mat.subset);

autoplot(tf.idf.pca, data = tf.idf.mat.subset)

var_explained = tf.idf.pca$sdev^2 / sum(tf.idf.pca$sdev^2)

scree_df <- data.frame(pc = c(1:5), var_explained = var_explained[1:5]);
ggplot(data = scree_df, mapping = aes(x = pc, y = var_explained)) + 
  geom_point() + 
  geom_line() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

load('/Users/jackdodson/Desktop/training_class_tf_idf_mat.rda')
tf.idf.pca <- prcomp(t(tf.idf.mat));
autoplot(tf.idf.pca, data = t(tf.idf.mat))

var_explained = tf.idf.pca$sdev^2 / sum(tf.idf.pca$sdev^2)

scree_df <- data.frame(pc = c(1:5), var_explained = var_explained[1:5]);
ggplot(data = scree_df, mapping = aes(x = pc, y = var_explained)) + 
  geom_point() + 
  geom_line() +
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

save(tf.idf.pca, file = '/Users/jackdodson/Desktop/tf.idf.pca.rda')

pc1 <- tf.idf.pca$rotation[,1]
selected_features <- c(pc1[pc1 > 0])

tf.idf.df <- data.frame(t(tf.idf.mat))
fit_1 <- lm(cancer ~ ., data = tf.idf.df)

library(dplyr)
components <- cbind(cancer = tf.idf.df[, "g161v"], tf.idf.pca$x[, 1:2]) %>%
  as.data.frame()

fit_2 <- lm(cancer ~ ., data = components)

summary(fit_1)$adj.r.squared
summary(fit_2)$adj.r.squared

tf.idf.pca %>% biplot(cex = .5)
>>>>>>> Stashed changes
