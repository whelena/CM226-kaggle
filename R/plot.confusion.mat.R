plot.confusion.mat <- function(
    conf.mat,
    ...
    ) {
 #conf.mat = RF.fit$confusion[, -10]                 
    # conf.count <- data.frame(
    #     class = rownames(conf.mat),
    #     truth = rowSums(conf.mat),
    #     predicted = colSums(conf.mat)
    #     );
    conf.mat <- apply(conf.mat, 1, function(x) x / colSums(conf.mat));

    # pred.bar <- create.barplot(
    #     formula = predicted ~ class,
    #     data = conf.count,
    #     yaxis.cex = 0,
    #     yaxis.tck = 0,
    #     xaxis.lab = rep('', nrow(conf.mat)),
    #     xaxis.tck = 0,
    #     xaxis.cex = 0,
    #     xaxis.rot = 90,
    #     resolution = 50
    #     );
    
    # truth.bar <- create.barplot(
    #     formula = truth ~ class,
    #     data = conf.count,
    #     xaxis.cex = 0,
    #     yaxis.lab = rep('', length(conf.mat)),
    #     yaxis.tck = 0,
    #     yaxis.cex = 0,
    #     resolution = 50,
    #     plot.horizontal = TRUE
    #     );
    
    hm <- create.heatmap(
        x = conf.mat,
        cluster.dimensions = 'none',
        xaxis.lab = rownames(conf.mat),
        xlab.label = 'Predicted Labels',
        xlab.cex = 1,
        xaxis.cex = 0.6,
        xaxis.fontface = 1,
        xaxis.rot = 0,
        yaxis.lab = colnames(conf.mat),
        ylab.label = 'True Labels',
        ylab.cex = 1,
        yaxis.cex = 0.6,
        yaxis.fontface = 1,
        print.colour.key = FALSE,
        row.pos = which(conf.mat > 0, arr.ind = TRUE)[,2],
        col.pos = which(conf.mat > 0, arr.ind = TRUE)[,1],
        cell.text = round(conf.mat[conf.mat > 0], 4),
        text.cex = 0.6,
        width = 5,
        height = 5,
        ...
        );
    
    # mp <- create.multiplot(
    #     plot.objects = list(hm, truth.bar, pred.bar),
    #     plot.layout = c(2, 2),
    #     layout.skip = c(FALSE, FALSE, FALSE, TRUE),
    #     panel.heights = c(0.3, 1),
    #     panel.widths = c(1, 0.2),
    #     plot.labels.to.retrieve = 1:3,
    #     xlab.label = c('\t', 'Predicted Labels', '\t', '\t', 'Predicted count'),
    #     xlab.cex = 0.7,
    #     xaxis.tck = 0,
    #     xaxis.cex = 0.6,
    #     xaxis.rot = 0,
    #     xaxis.fontface = 1,
    #     #xlab.to.xaxis.padding = 0.5,
    #     ylab.label = c( 'True Labels', '\t', '\t', 'True count', '\t'),
    #     ylab.padding = 13,
    #     ylab.cex = 0.7,
    #     yaxis.cex = 0.6,
    #     yaxis.tck = 0,
    #     yaxis.fontface = 1,
    #     # left.padding = 10,
    #     # x.spacing = c(- 3),
    #     # y.spacing = c(- 1.5),
    #     # bottom.padding = 3,
    #     print.new.legend = TRUE,
    #     height = 6,
    #     width = 6
    #     );
    # return(mp);
    }
