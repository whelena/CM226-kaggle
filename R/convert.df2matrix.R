convert.df2array <- function(DF, value, x.axis = 'word', y.axis = 'ID') {
    # converts DF to matrix
    if (is.null(DF[[x.axis]]) | is.null(DF[[value]]) | is.null(DF[[y.axis]])) {
        stop(paste('Dataframe does not contain one of the columns:', value, x.axis, y.axis));
        };
    arr <- reshape(
        data = as.data.frame(DF[, c(x.axis, y.axis, value)]),
        v.names = value,
        timevar = y.axis,
        idvar = x.axis,
        direction = 'wide'
        );
    rows            <- arr[, 1];
    cols            <- gsub(paste0(value, '.'), '', names(arr)[-1]);
    arr             <- arr[, (-1)] |> as.data.frame();
    rownames(arr)   <- rows;
    colnames(arr)   <- cols;
    arr[is.na(arr)] <- 0;
    return(arr);
    }