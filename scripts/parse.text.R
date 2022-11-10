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
    )
