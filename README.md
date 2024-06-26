# r-model-loop

Tools to create list of models based on subsetting a given data.frame by given categories, as well as tools to analyze the resulting (possibly nested) list. 

## Motivation

One advantage of the R ecosystem is the ease at which a regression model can be produced from raw data and subsequent inference can be made based on the model. However, in some circumstances, it is useful to subset a large dataset by various criteria and produce a collection of relatively simple models, rather then producing a single complex model from the whole single dataset.

The purpose of this repo is to provide a set of tools that make it easy to produce a collection of subsetted models from a single dataset, and to make inference based on the resulting models.  

## Typical workflow

The tools provided in this repo is organized as a collection of R functions. All functions are defined within a single `model_loop.r` file, which can be loaded into the global environment via `source()` in base R.

The typical workflow in using these functions are as follows:

![Typical Workflow for using model_loop.r functions](docs/img/model_loop_workflow.svg)

Basically, start by using `model_loop()` (or `model_loop2()`, discussed below) to process the input data into a (possibly nested) list of models. If needed, the models can then be transformed and/or reorganized using `flatten_model_list()` and `transform_loop()`. The resulting model list can then be used as input for `extract_loop()`, `predict_loop()`, `compare_loop()`, `cross_compare_loop()` and `cross_compare_loop2()`. All these 5 functions take the list of models (and sometimes additional data and/or model object(s)) and produce dataframes as output, and all such dataframes include information about the categories used to subset the data. Additionally, the list of models can also optionally be saved, e.g., in an `.rda` file.

Compared to `model_loop()`, `model_loop2()` uses model calls that better resemble interactive calls, which could be useful if there is any need to reproduce the model call later on. However, `model_loop2()` also introduces a dependency to the `rlang` package, which some may prefer to avoid.

In addition to the functions appearing in the above flow chart, `model_loops.r` also include convenience functions `recurse_append_attr()` and `recurse_prepand_attr()`, `nest_model_into()`, and `nest_model_list()`.

## Example

An example for using `model_loop.r` (and the associated data) can be found in the [`example/`](example) subfolder. The resulting html output can be found under the `docs` folder as `model_loops_example.html` (a rendered version and be found [here](https://htmlpreview.github.io/?https://github.com/winghoko/r-model-loop/blob/main/docs/model_loops_example.html)).
