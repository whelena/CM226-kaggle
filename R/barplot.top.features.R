library(BoutrosLab.plotting.general)
library(BoutrosLab.utilities)

barplot.top.feature <- function(df, group, feature.col, fname, ...) {
    df$group.col <- factor(df[[group]], levels = sort(unique(df[[group]])));

    col.scheme <- default.colours(length(levels(df$group.col)), palette.type = 'pastel');

    legend.cls <- legend.grob(
        list(
            legend = list(
                title = group,
                labels = levels(df$group.col),
                colours = rev(col.scheme),
                border = 'black'
                )
            ),
        size = 1,
        title.cex = 0.75,
        label.cex = 0.6
        );

    create.barplot(
        data = df,
        stack = TRUE,
        groups = Class,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xlab.cex = 1,
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        col = col.scheme,
        legend = list(right = list(
            fun = legend.cls
            )),
        ...
        )

    }
