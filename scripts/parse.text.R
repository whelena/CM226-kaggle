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
custom.stopwords <- data.frame(read.table('data/custom.stopwords.txt', header = TRUE));
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
#train.genes  <- merge(train.var[, c('ID', 'Class')], train.genes, by = 'ID');
IDs.no.genes <- train.var$ID[!train.var$ID %in% unique(train.genes$ID)];

gene.count              <- count(train.genes, word, Class);
gene.count.per.class    <- spread(gene.count, word, n);
gene.count.per.class[is.na(gene.count.per.class)] <- 0;
# gene.frequency          <- sort(colSums(gene.count.per.class), decreasing = TRUE);
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
