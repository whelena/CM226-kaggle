###################################################################################################
# SETUP
###################################################################################################
devtools::load_all('.');

# var1       <- read.csv(file.path('data', 'test_variants'));
# var2       <- read.csv(file.path('data', 'training_variants'));
# write.csv(
#     x = c(var1$Gene, var2$Gene),
#     file = 'data/genes.txt',
#     row.names = FALSE,
#     col.names = NA,
#     quote = FALSE
#     );
###################################################################################################
   
name <- 'training';

var       <- read.csv(file.path('data', paste0(name, '_variants')));
txt.dump  <- tibble(text = read_lines(file.path('data', paste0(name, '_text')), skip = 1));
txt       <- txt.dump %>% separate(text, into = c('ID', 'Text'), sep = '\\|\\|');
txt       <- txt %>% mutate(ID = as.integer(ID));
gene.list <- read.csv(file.path('data', 'genes.txt'));

# filter for null text and split into words
txt       <- txt[nchar(txt$Text) > 10, ];
word.df   <- split.text.to.words(text = txt);
###################################################################################################
# remove numbers, stop words from text files
###################################################################################################
# load stop words from tidytext and custom file
data('stop_words');
custom.stopwords <- data.frame(read.table('data/custom.stopwords.txt', header = TRUE));
#numbers          <- data.frame(word = as.character(1:100));
custom.stopwords <- rbind(stop_words[, 'word'], custom.stopwords);
word.df          <- remove.words(words = word.df, word.list = custom.stopwords);
word.df          <- word.df[which(!grepl('^\\d.+$', word.df$word)), ];
if (name == 'training') {
    word.df          <- merge(var[, c('ID', 'Class')], word.df, by = 'ID');
    }
word.frequency   <- table(word.df$word) |> sort(decreasing = TRUE);

###################################################################################################
# get gene per class frequency
###################################################################################################
# extract genes that are in the training and testing set
genes.df  <- extract.words(words = word.df, word.list = gene.list$x);
# genes.df  <- merge(var[, c('ID', 'Class')], genes.df, by = 'ID');
IDs.no.genes <- var$ID[!var$ID %in% unique(genes.df$ID)];

if (name == 'training') {
    gene.count              <- count(genes.df, word, Class);
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
    }
save(genes.df, file = file.path('result', paste(name, 'genes.rda', sep = '_')));
###################################################################################################
# remove plurals from non genes
###################################################################################################
no.genes.df         <- word.df[word.df$word %in% unique(genes.df$word), ];
plural.map          <- singularize(unique(no.genes.df$word));
names(plural.map)   <- unique(no.genes.df$word);
no.genes.df$word    <- plural.map[match(no.genes.df$word, names(plural.map))]
word.df <- rbind(no.genes.df, genes.df);

save(word.df, file = file.path('result', paste(name, 'words.rda', sep = '_')));
