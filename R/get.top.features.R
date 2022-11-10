get.top.features <- function (df, feature.column, top.n = 20) {
    df$feat <- df[[feature.column]];
    mat <- spread(df, feat, n);
    mat.freq <- sort(colSums(gene.count.per.class, na.rm = TRUE), decreasing = TRUE);
    df <- df[df$feat %in% names(mat.freq)[1:top.n], ];
    df$feat <- NULL;
    return(df);
}
