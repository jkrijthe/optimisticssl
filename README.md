Optimistic Semi-supervised Least Squares Classification
===============

Authors: Jesse H. Krijthe and Marco Loog

This repository contains the code to generate/reproduce the paper "Optimistic Semi-supervised Least Squares Classification"

Abstract
---------------
The goal of semi-supervised learning is to improve supervised classifiers by using additional unlabeled training examples. In this work we study a simple self-learning approach to semi-supervised learning applied to the least squares classifier. We show that a soft-label and a hard-label variant of self-learning can be derived by applying block coordinate descent to two related but slightly different objective functions. The resulting soft-label approach is related to an idea of dealing with missing data that dates back to the 1930s. We show that the soft-label variant typically outperforms the hard-label variant on benchmark datasets and partially explain this behaviour by studying the relative difficulty of finding good local minima for the corresponding objective functions.

How to use this code
---------------
The easiest way to generate the paper is by using knitr in RStudio to rebuild the optimisticssl.Rnw file. To rerun the experiments in the paper, see the code in the R directory.