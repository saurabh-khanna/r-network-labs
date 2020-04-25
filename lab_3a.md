Introduction to R
================
Saurabh Khanna
2020-04-24

  - [Chapter 3: Basic Introduction to
    R](#chapter-3-basic-introduction-to-r)
      - [Preliminaries](#preliminaries)
      - [Packages](#packages)

# Chapter 3: Basic Introduction to R

Built with R version 3.6.3

This tutorial offers an introduction to the basics of R. R is a general
platform for performing statistical and programing tasks. R offers a
wide variety of network functionality, making it possible to do
everything from simple network construction and graphing to
sophisticated statistical models, all under one platform. Through a
series of R tutorials, we will offer R syntax and examples for each
major topic covered in the main text. In this tutorial, we offer a very
brief overview of R and R packages, just enough to make sure that
everyone can run and follow the tutorials that follow.

Note that we do not offer a dedicated tutorial on programming in R. All
tutorials (except this one) are motivated by a problem related to
network analysis. It will be useful to have some basic familiarity with
R and its syntax (or at least a willingness to learn as we go along\!)
but even this is not strictly necessary. Each tutorial is presented as a
Rmarkdown file, with both syntax and results, making it easier for
readers to follow along, even without detailed knowledge of R. Readers
interested in learning R in more depth should consider one of the many
introductory R textbooks.

## Preliminaries

The first step is to gain access to R, which is free and available on
the R website: <http://cran.r-project.org/>. Simply go to the R website,
select the appropriate location and operating system, and follow the
instructions to download the base distribution of R. Rstudio
(<https://rstudio.com/>) offers a user friendly environment to run R and
is recommended.

Once R is opened we can begin to run commands. R commands can be run
directly from the command line, from the R editor or from a text editor
separate from R. For example, the following syntax does a bit of simple
math:

``` r
5 + 5
```

    ## [1] 10

As another example, here we sum up a series of numbers using a sum
function.

``` r
sum(5, 5, 4)
```

    ## [1] 14

R offers detailed help files for each function. To access help, type
`?name_of_function` in the console. For example, for help on the sum
function, type:

``` r
?sum
```

Note that all statements proceeded by a \# are comments and will not
run. For example:

``` r
#This is a comment. R will not recognize this as a command.
```

## Packages

The functionality of R is extended by loading packages that researchers
have contributed to the R community. Packages are sets of useful
functions that accomplish a set of tasks, such as graphing a network. In
this book we will primarily make use of: `igraph`, `sna`, `network`,
`ergm`, `tergm`, and `Rsiena`. igraph, network and sna perform similar
tasks related to network construction, graphing and network measurement.
sna and network work in tandem while igraph is a fairly stand alone
package. Note that igraph and sna/network construct networks in very
different manners and there is little overlap in syntax. We will present
the code for performing different calculations and tasks in both igraph
and sna/network, where appropriate. The other packages, like ergm,
tergm, and Rsiena, estimate more advanced statistical models and we will
cover them later in the book.

Each package of interest must be installed and loaded before it can be
used. The packages will not be immediately available when R is opened. A
package only has to be installed once on a computer, but the package
will have to be loaded every time R is restarted. To install all of the
packages used in this book, run the following:

``` r
source("https://sites.google.com/site/jeffreysmithdatafiles/setup.R")
```

It is also possible to install each package individually as we need
them. For example, to install `igraph`, `sna`, `network`, `tidygraph`,
and `ggraph`, we would do:

``` r
install.packages("igraph")
install.packages("sna")
install.packages("network")
install.packages("tidygraph")
install.packages("ggraph")
```

Now that we have our packages successfully installed, we can go ahead
and load them into R. Here we will load the `network` package as an
example. We can use of all the functions available in that package once
it is loaded into R. We load packages by using a library command. The
input is the name of the package, not in quotes.

``` r
library(network)
```

We can look up all of the functions within a package by using a help
command. For example, let’s look at the functions available in the
`network` package.

``` r
help(package = network)
```

Note that the package statement is necessary to look up all of the
functions. We can also detach a package if we no longer want it loaded.
This is sometimes useful if two packages do not play well together. Here
we will use a detach command.

``` r
detach(package:network)
```

We will now turn to the basics of data management and network
construction in R, covered in the second tutorial for Chapter 3.
