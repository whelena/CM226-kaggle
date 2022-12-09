devtools::load_all('.');
load('result/matrix/tf.idf.pca.rda');

quant <- 0.99;
selected.words <- list()
for (i in seq_along(1:9)) {
    pca.weights <- data.frame(sort(tf.idf.pca$rotation[, i], decreasing = TRUE));
    names(pca.weights) <- c('weight');
    pca.weights$word <- rownames(pca.weights);

    pca.plot <- pca.weights[abs(pca.weights$weight) > quantile(abs(pca.weights$weight), quant), ];
    # rbind(
    #     head(pca.weights, n = 25),
    #     tail(pca.weights, n = 25)
    #     );
    pca.plot$word <- factor(pca.plot$word, levels = pca.plot$word);
    create.barplot(
        filename = file.path('result', 'plot', paste('training_pca', i, 'words.png', sep = '_')),
        formula = weight ~ word,
        data = pca.plot,
        # xaxis.lab = rownames(pca.plot),
        xlab.label = 'Words',
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 90,
        ylab.label = 'Weights',
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        width = 8,
        height = 5
        );
    selected.words[[i]] <- pca.plot$word;
}

unique.words <- unique(unlist(selected.words));

load(file = file.path('result', 'matrix', 'training_tf_idf_mat.rda'));

tf.idf.mat <- tf.idf.mat[unique.words, ];
save(tf.idf.mat, file = file.path('result', 'matrix', 'training_pca_tf_idf_mat.rda'));
