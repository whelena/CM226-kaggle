###################################################################################################
# SETUP
###################################################################################################
devtools::load_all('.');

name            <- 'training_pca_tf_idf';
result.dir      <- file.path('result', 'RF');
test.df         <- read.csv(file.path('data', 'test_variants'));
load(file = file.path(result.dir, paste(name, 'RF.rda', sep = '_')));
load(file = file.path('result', 'matrix', 'test_tf_idf_mat.rda'));
common.predictor            <- word.predictor[word.predictor %in% colnames(tf.idf.mat)];
missing.predictor           <- word.predictor[!word.predictor %in% colnames(tf.idf.mat)];
test.mat                    <- t(tf.idf.mat[common.predictor, ]);
missing.predictor.df        <- data.frame(matrix(0, nrow = nrow(test.mat), ncol = length(missing.predictor)));
names(missing.predictor.df) <- missing.predictor;
test.mat                    <- cbind(test.mat, missing.predictor.df);

predicted.class <- predict(
    object = RF.fit,
    newdata = test.mat,
    type = 'response'
    );

prediction.prob <- predict(
    object = RF.fit,
    newdata = test.mat,
    type = 'prob'
    );
write.table(
    x = prediction.prob,
    file = file.path(result.dir, 'prediction_probability.tsv'),
    sep = '\t',
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
    );
bar.df <- as.data.frame(table(predicted.class));
create.barplot(
    filename = file.path('result', 'plot', 'test_predicted_class.png'),
    formula = Freq ~ predicted.class,
    data = bar.df[bar.df$Freq > 0, ],
    xaxis.cex = 0.6,
    xaxis.fontface = 1,
    xlab.label = 'Class',
    xlab.cex = 1,
    ylab.label = 'Frequency',
    ylab.cex = 1,
    yaxis.cex = 0.6,
    yaxis.fontface = 1,
    height = 2,
    width = 5
    );

predicted.df <- data.frame(
    ID = names(predicted.class),
    class = factor(predicted.class, levels = 1:9),
    probability = apply(prediction.prob, 1, max)
    );

predicted.df <- merge(test.df, predicted.df, by = 'ID', all.x = TRUE);
write.table(
    x = predicted.df,
    file = file.path(result.dir, 'predicted_test.tsv'),
    sep = '\t',
    col.names = TRUE,
    row.names = FALSE,
    quote = FALSE
    );

create.boxplot(
    filename = file.path('result', 'plot', 'test_predicted_boxplot.png'),
    formula = probability ~ class,
    data = predicted.df,
    xaxis.cex = 0.6,
    xaxis.fontface = 1,
    xlab.label = 'Class',
    xlab.cex = 1,
    ylab.label = 'Prediction probability',
    ylab.cex = 1,
    yaxis.cex = 0.6,
    yaxis.fontface = 1,
    height = 5,
    width = 5
);


