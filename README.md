# model.adapter: an R package providing abstraction layer for statistical/machine learning models

[![Build Status](https://travis-ci.com/Marchen/model.adapter.svg?branch=master)](https://travis-ci.com/Marchen/model.adapter)

## Introduction

This package provides `model.adapter` class which provides unified method to access common data and methods of statistical/machine learning models.

This package is developed for [cv.model](https://github.com/Marchen/cv.models) and [partial.plot](https://github.com/Marchen/partial.plot) packages.

For the details of the class, please see the document of model.adapter in the help page in R.

## Install

Please copy and paste following code into R console.

```{R}
install.packages(
    "model.adapter", type = "source",
    repos = c(
        "http://florivory.net/R/repos", options()$repos
    )
)
```
