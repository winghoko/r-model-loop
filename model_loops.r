#' WARNING: internal function. May change without notice.
#'
#' Recursively add a new value `value` to all objects in in_list first having
#' the attribute list named `label`.
#'
#' @param in_list - The list to recursively descend into.
#' @param label - The name of the attribute to be modified. The attribute is
#'   assumed to be a list or vector (more specifically, to be `append`
#'   operable).
#' @param value - The value to prepend to the attribute list.
#' @returns a new list whose objects have the attributes modified.
#'
.recurse_prepend_attr <- function(in_list, label, value) {

  if (is.null(attr(in_list, label))) { # recurse case
    in_attrs <- attributes(in_list)
    new_list <- list() # everything is immutable in R, so a new list is needed
    for (sub_list in in_list){
      new_list <- append(new_list, list(
        .recurse_prepend_attr(sub_list, label, value)
      ))
    }
    mostattributes(new_list) <- in_attrs # copy all "safe" attributes
    return(new_list)

  } else { # base case

    attr(in_list, label) <- append(value, attr(in_list, label))
    return(in_list)
  }
}

#' Loop through a data.frame, subset data by values of columns specified by
#' `categories`. Perform a model fit for each subset using `modeler`, and
#' collect the results in a (possibly nested) list.
#'
#' @param data - The data.frame containing the data to be subsetted and
#'   modeled.
#' @param categories - The categories whose distinct values are used to
#'   subset the data.
#' @param formula - R formula object. The formula to be used in modeling.
#' @param modeler - The function used to perform the modeling, e.g., `lm`
#'   and `glm`. The modeler should take `formula` as the first argument
#'   and subsetted `data` as second argument.
#' @param ... - Additional arguments are passed to the modeler.
#' @returns a (possibly nested) list containing model objects returned
#'   by the modeler. Each model object has 2 additional attributes:
#'   `category_keys` containing a copy of `categories`, and `category_values`
#'   containing the particular values of the said keys. (These two attributes
#'   are used in subsequent functions. Avoid tempering them). In addition,
#'   the nested list is named by the value the category that the list is
#'   enumerating through.
#'
model_loop <- function(data, categories, formula, modeler = glm, ...) {

  if (length(categories) == 0) { # base case

    model <- modeler(formula, data, ...)
    attr(model, "category_keys") <- character(0) # to be filled later
    attr(model, "category_values") <- list() # to be filled later
    return(model)

  } else { # recurse case

    cat_used <- categories[[1]]
    cat_remain <- categories[-1]
    cat_values <- unique(data[[cat_used]])
    out_list <- list()
    all_data <- data
    for (val in cat_values){
      data <- all_data[all_data[[cat_used]] == val, ] # subset-ing
      in_list <- model_loop(data, cat_remain, formula, modeler, ...)
      in_list <- .recurse_prepend_attr(in_list, "category_keys", cat_used)
      in_list <- .recurse_prepend_attr(in_list, "category_values", val)
      out_list <- append(out_list, list(in_list))
    }
    names(out_list) <- as.character(cat_values)
    return(out_list)
  }
}

#' Convert the nested list of models returned by `model_loop()` into a
#' flattened list.
#'
#' NOTE: In all subsequent functions (e.g., `predict_loop()`, the flattened
#' list will work just as the nested list do.
#'
#' @param model_list - The list of models originated from `model_loop()`.
#' @param name_sep - The separator used to join all the category values into
#'   a single indexing name.
#' @param out_list - The list to append the models object to. If NULL, a new
#'   list is created, which is usually the desired behavior for user calls
#'   (this argument is mainly intended to support the recursive calls used in
#'   this function).
#' @returns a flattened list containing model objects returned by the modeler,
#'   with the `category_keys` and `category_values` attribute persisted. The
#'   flattened list is named using a concatenation of the category values.
#'
flatten_model_list <- function(model_list, name_sep = ":", out_list = NULL) {

  if (is.null(out_list)) out_list <- list()

  if (is.null(attr(model_list, "category_keys"))) { # recurse step

    for (model in model_list){
      out_list <- flatten_model_list(model, name_sep, out_list)
    }
    return(out_list)

  } else { # base step
    old_names <- names(out_list)
    out_list <- append(out_list, list(model_list))
    new_name <- paste(
      as.character(attr(model_list, "category_values")), collapse = name_sep
    )
    names(out_list) <- append(old_names, new_name)
    return(out_list)
  }
}

#' Extract (and possibly compute) information from each of the model object
#' in `model_list` and collect the results in a single data.frame.
#'
#' @param model_list - The list (nested or flattened) of models originated
#'   from `model_loop()`.
#' @param extractor - The function that extract the required information from
#'   the model objects, e.g., a wrapper around the `stats::coef()` function.
#    The extract should take a model object as its first argument and return
#'   a data.frame (or more precisely, a object that can cbind() with a
#'   data.frame to produce another data.frame). The column names of this
#'   data.frame should be consistent regardless the model object from the
#'   list it operates on.
#' @param out_df - The data.frame for which the extracted data will be
#'   appended to. The structure of the appending data.frame consists of
#'   columns showing category values (with column names being the category
#'   names) of the model, followed by the entries returned by the extractor.
#'   (This argument is mainly intended to support the recursive calls used in
#'   this function; the default (= NULL) is suitable for most user calls).
#' @param out_names - Optionally override the column names returned by the
#'   predictor. If NULL the column names produced by the extractor is used.
#' @param ... - Additional arguments are passed to the extractor.
#' @returns a data.frame, consisting of columns showing category values
#'   (with column names being the category names) of the model, followed by
#'   the entries returned by the extractor.
#'
extract_loop <- function(
  model_list, extractor, out_df = NULL, out_names = NULL, ...
) {

  if (is.null(attr(model_list, "category_values"))) { # recurse step

    for (model in model_list){
      out_df <- extract_loop(model, extractor, out_df, out_names, ...)
    }
    return(out_df)

  } else { # base step

    extraction <- extractor(model_list, ...)
    keys <- as.data.frame(attr(model_list, "category_values"))
    n <- nrow(extraction)
    if (!is.null(n)) keys <- keys[rep(1, n), ]
    colnames(keys) <- attr(model_list, "category_keys")
    rownames(keys) <- NULL

    new_out_df <- cbind(keys, extraction)

    if (!is.null(out_names)) {
      colnames(new_out_df) <- append(colnames(keys), out_names)
    }
    out_df <- rbind(out_df, new_out_df)
    return(out_df)
  }
}

#' Predict behavior of new data based on each of the model object in
#' `model_list` and collect the results in a single data.frame.
#'
#' @param model_list - The list (nested or flattened) of models originated
#'   from `model_loop()`.
#' @param predictor - The function that perform prediction based on the
#'   model objects, e.g., the `stats::predict()` function. The predictor
#'   should take a model object as its first argument and a data.frame
#'   of new data as its second argument. The predictor should return
#'   a data.frame (or more precisely, a object that can cbind() with a
#'   data.frame to produce another data.frame). The column names of this
#'   data.frame should be consistent regardless the model object from the
#'   list it operates on.
#' @param out_df - The data.frame for which the predicted data will be
#'   appended to. The structure of the appending data.frame consists of
#'   columns showing category values (with column names being the category
#'   names) of the model, followed by the entries from the new_data
#'   data.frame, followed by the data.frame returned by the predictor.
#'   (This argument is mainly intended to support the recursive calls used in
#'   this function; the default (= NULL) is suitable for most user calls).
#' @param out_names - Optionally override the column names returned by the
#'   predictor. If NULL the column names produced by the predictor is used.
#' @param ... - Additional arguments are passed to the compare function.
#' @returns a data.frame, consisting of columns showing category values
#'   (with column names being the category names) of the model, followed by
#'   the entries returned by the predictor.
#'
predict_loop <- function(
  model_list, new_data, predictor = predict,
  out_df = NULL, out_names = NULL, ...
) {

  if (is.null(attr(model_list, "category_values"))) { # recurse step

    for (model in model_list){
      out_df <- predict_loop(
        model, new_data, predictor, out_df, out_names, ...
      )
    }
    return(out_df)

  } else { # base step

    prediction <- predictor(model_list, new_data, ...)
    keys <- as.data.frame(attr(model_list, "category_values"))
    keys <- keys[rep(1, nrow(new_data)), ]
    new_out_df <- cbind(keys, new_data)
    cols <- append(attr(model_list, "category_keys"), colnames(new_data))
    colnames(new_out_df) <- cols

    new_out_df <- cbind(new_out_df, prediction)
    if (!is.null(out_names)) {
      colnames(new_out_df) <- append(cols, out_names)
    }
    rownames(new_out_df) <- NULL

    out_df <- rbind(out_df, new_out_df)
    return(out_df)
  }
}

#' Compare each of the model object in `model_list` with a fixed model
#' `base_model`and collect the results in a single data.frame.
#'
#' @param model_list - The list (nested or flattened) of models originated
#'   from `model_loop()`.
#' @param base_model - The model for which each models in `model_list` is
#'   compared to. The `base_model` object should have the `category_keys` and
#'   `category_values` attributes consistent with the models in `model_list`
#' @param cmp_func - The compare function that use to compare the models from
#'   `model_list` with the base model `base_model`. The function should take
#'   a model from `model_list` as a first argument, and `base_model` as its
#'   second argument. The compare function should return a data.frame (or more
#'   precisely, a object that can cbind() with a data.frame to produce another
#'   data.frame). The column names of this data.frame should be consistent
#'   regardless the model object from the list it operates on.
#' @param groupings - The groupings under which the comparisons are grouped.
#'    Specifically, if a category is used in grouping, then two models are
#'    compared only if their corresponding category values agree.
#' @param out_df - The data.frame for which the comparison data will be
#'   appended to. The structure of the appending data.frame consists of
#'   columns showing category values that are fixed (i.e., columns matching
#'   the `groupings` argument), followed by categories that are compared
#'   between model in `model_list` with `base_model` (these are expanded into
#'   pairs: one for model in `model_list` and another for `base_model`),
#'   followed by the data.frame returned by the compare function.
#'   (This argument is mainly intended to support the recursive calls used in
#'   this function; the default (= NULL) is suitable for most user calls).
#' @param suffix - The pair of suffix (as a character vector) used to
#'   distinguish between category values of the model from `model_list` (1st
#'   element) and that from `base_model` (2nd element).
#' @param out_names - Optionally override the column names returned by the
#'   extractor. If NULL the column names produced by `cmp_func` is used.
#' @param ... - Additional arguments are passed to `cmp_func`.
#' @returns a data.frame, consisting of columns showing category values that
#'   are not compared, followed by pairs of categories values that are
#'   compared, followed by the entries returned by `cmp_func`.
#'
compare_loop <- function(
  model_list, base_model, cmp_func, groupings,
  out_df = NULL, suffix = c("_1", "_2"), out_names = NULL, ...
) {

  if (is.null(attr(model_list, "category_values"))) { # recurse step

    for (model in model_list){
      out_df <- compare_loop(
        model, base_model, cmp_func, groupings, out_df, ...
      )
    }
    return(out_df)

  } else { # base step

    keys <- attr(model_list, "category_keys")
    values_1 <- attr(model_list, "category_values")
    values_2 <- attr(base_model, "category_values")

    cat_idx <- integer(0)
    fact_idx <- integer(0)
    for (i in seq_along(keys)) {
      if (keys[[i]] %in% groupings) {
        if (values_1[[i]] != values_2[[i]]) {
          return(out_df) # early return for unmatched case
        } else {
          cat_idx <- append(cat_idx, i)
        }
      } else {
        fact_idx <- append(fact_idx, i)
      }
    }

    labels_names <- character(0)
    labels_values <- list()
    for (i in cat_idx){
      labels_names <- append(labels_names, keys[[i]])
      labels_values <- append(labels_values, values_1[[i]])
    }
    for (i in fact_idx){
      labels_names <- append(labels_names, paste0(keys[[i]], suffix[[1]]))
      labels_names <- append(labels_names, paste0(keys[[i]], suffix[[2]]))
      labels_values <- append(labels_values, values_1[[i]])
      labels_values <- append(labels_values, values_2[[i]])
    }

    new_df <- as.data.frame(labels_values)
    colnames(new_df) <- labels_names

    cmp_result <- cmp_func(model_list, base_model, ...)

    n <- nrow(cmp_result)
    if (!is.null(n)) {
      new_df <- new_df[rep(1, n), ]
    }
    rownames(new_df) <- NULL

    new_df <- cbind(new_df, cmp_result)
    if (!is.null(out_names)) {
      colnames(new_df) <- append(labels_names, out_names)
    }

    out_df <- rbind(out_df, new_df)
    return(out_df)
  }
}

#' Compare each pair of model objects from `model_list` for which the
#' values of the each category in the `groupings` argument agrees.
#'
#' @param model_list - The list (nested or flattened) of models originated
#'   from `model_loop()`.
#' @param cmp_func - The compare function that use to compare the models from
#'   `model_list` with the base model `base_model`. The function should take
#'   a model from `model_list` as a first argument, and a model from
#'   `cmp_models` as its second argument. The compare function should return
#'   a data.frame (or more precisely, a object that can cbind() with a
#'   data.frame to produce another data.frame). The column names of this
#'   data.frame should be consistent regardless the models it operates on.
#' @param groupings - The categories under which the comparisons are grouped.
#'    Specifically, if a category is used in grouping, then two models are
#'    compared only if their corresponding category values agree.
#' @param cmp_models - The models to be cross-compared to the set of models
#'   in `model_list`. Defaults to `model_list` if NULL. (This argument is
#'   mainly intended to support the recursive calls used in this function;
#'   the default is suitable for most user calls). Could be used to make
#'   pairwise comparison between two lists of models if a different list of
#'   model is supplied to this argument. Note that all models from the two
#'   lists should have consistent `category_keys` and `category_values`.
#' @param out_df - The data.frame for which the comparison data will be
#'   appended to. The structure of the appending data.frame consists of
#'   columns showing grouping category values from the `groupings` argument,
#'   followed by categories that are cross compared (these are expanded into
#'   pairs: one for each model), followed by the data.frame returned by the
#'   compare function. (This argument is mainly intended to support the
#'   recursive calls used in this function; the default (= NULL) is suitable
#'   for most user calls).
#' @param suffix - The pair of suffix (as a character vector) used to
#'   distinguish between category values of the model from `model_list` (1st
#'   element) and that from `cmp_models` (2nd element).
#' @param out_names - Optionally override the column names returned by the
#'   extractor. If NULL the column names produced by `cmp_func` is used.
#' @param ... - Additional arguments are passed to `cmp_func`.
#' @returns a data.frame, consisting of columns showing grouping category 
#'   values, followed by pairs of categories values that are compared,
#'   followed by the entries returned by `cmp_func`.
#'
cross_compare_loop <- function(
  model_list, cmp_func, groupings, cmp_models = NULL, out_df = NULL,
  suffix = c("_1", "_2"), out_names = NULL, ...
) {

  if (is.null(cmp_models)) cmp_models <- model_list

  if (is.null(attr(cmp_models, "category_values"))) { # recurse step

    for (model in cmp_models){
      out_df <- cross_compare_loop(
        model_list, cmp_func, groupings, model, out_df,
        suffix, out_names, ...
      )
    }
    return(out_df)

  } else { # base step

    out_df <- compare_loop(
      model_list, cmp_models, cmp_func, groupings, out_df,
      suffix, out_names, ...
    )
    return(out_df)
  }
}

#' Compare each combination of model objects from `model_list` for which the
#' values of the each category in the `groupings` argument agrees. Here
#' combination means that the comparison will run only on one of (a,b) and
#' (b,a), and never on (a,a).
#'
#' @param model_list - The list (nested or flattened) of models originated
#'   from `model_loop()`.
#' @param cmp_func - The compare function that use to compare the models from
#'   `model_list` with the base model `base_model`. The function should take
#'   a model from `model_list` as a first argument, and a model from
#'   `cmp_models` as its second argument. The compare function should return
#'   a data.frame (or more precisely, a object that can cbind() with a
#'   data.frame to produce another data.frame). The column names of this
#'   data.frame should be consistent regardless the models it operates on.
#' @param groupings - The categories under which the comparisons are grouped.
#'    Specifically, if a category is used in grouping, then two models are
#'    compared only if their corresponding category values agree.
#' @param out_df - The data.frame for which the comparison data will be
#'   appended to. The structure of the appending data.frame consists of
#'   columns showing grouping category values from the `groupings` argument,
#'   followed by categories that are cross compared (these are expanded into
#'   pairs: one for each model), followed by the data.frame returned by the
#'   compare function. (This argument is mainly intended to support the
#'   recursive calls used in this function; the default (= NULL) is suitable
#'   for most user calls).
#' @param suffix - The pair of suffix (as a character vector) used to
#'   distinguish between category values of the model from `model_list` (1st
#'   element) and that from `cmp_models` (2nd element).
#' @param out_names - Optionally override the column names returned by the
#'   extractor. If NULL the column names produced by `cmp_func` is used.
#' @param ... - Additional arguments are passed to `cmp_func`.
#' @returns a data.frame, consisting of columns showing grouping category 
#'   values, followed by pairs of categories values that are compared, 
#'   followed by the entries returned by `cmp_func`.
#'
cross_compare_loop2 <- function(
  model_list, cmp_func, groupings, out_df = NULL,
  suffix = c("_1", "_2"), out_names = NULL, ...
) {

  # trivial case: a model is passed rather than a list
  if (!is.null(attr(model_list, "category_values"))) return(out_df)

  # defensive flattening
  model_list <- flatten_model_list(model_list)
  n <- length(model_list)

  # another trivial case: only one model in list
  if (n == 1) return(out_df)

  # double Loop
  for (i in seq(1, n - 1)) {
    for (j in seq(i + 1, n)) {
      out_df <- compare_loop(
        model_list[[i]], model_list[[j]], cmp_func, groupings, out_df,
        suffix, out_names, ...
      )
    }
  }
  return(out_df)
}
