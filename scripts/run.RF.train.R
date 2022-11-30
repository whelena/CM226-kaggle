###################################################################################################
# SETUP
###################################################################################################
devtools::load_all('.');

train.var           <- read.csv('data/training_variants');
rownames(train.var) <- train.var$ID;

name   <- 'training'
metric <- 'tf_idf' # either tf_idf or frequency
load(file = file.path('result', paste(name, metric, 'mat.rda', sep = '_')));

data <- t(tf.idf.mat);
class <- as.factor(train.var[rownames(data), 'Class']);

# Fit random forest
mtry <- 3 * floor(sqrt(ncol(data)));
ntree <- 500;
RF.fit <- randomForest(
    x           = data,
    y           = class,
    importance  = TRUE,          # outputing importance of each predictors
    nodesize    = 1,             # complexity of each tree - minimum size of terminal nodes, putting it higher than 1 gives incomplete trees
    ntree       = ntree,         # number of trees that are grown
    mtry        = mtry,          # number of variables to choose from (default = sqrt of total number)
    na.action   = na.omit        # drop NA values
    );

# plot(RF.fit, type = 'l');

oob.err <- RF.fit$err.rate[nrow(RF.fit$err.rate), 'OOB'];
conf.plot <- plot.confusion.mat(
    conf.mat = RF.fit$confusion[, -10],
    colour.scheme = c('white', 'blue'),
    filename = file.path('result', 'plot', paste(name, metric, 'confusion_matrix.png', sep = '_'))
    );
# Remove large, unneeded attributes to reduce model size
attr(RF.fit$terms, 'factors') <- NULL;

# Add fit and vector of predictor names to output list
save(
    RF.fit,
    file = file.path('result', paste(name, metric, 'RF.rda', sep = '_'))
    );
