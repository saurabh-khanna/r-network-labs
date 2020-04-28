Centrality and Hierarchy
================
Saurabh Khanna
2020-04-27

  - [Chapter 9: R Tutorial on Centrality and
    Hierarchy](#chapter-9-r-tutorial-on-centrality-and-hierarchy)
      - [Setting up the Session and Reading in Example Network
        Data](#setting-up-the-session-and-reading-in-example-network-data)
      - [Centrality](#centrality)
      - [Centralization and the Distribution of
        Centrality](#centralization-and-the-distribution-of-centrality)
      - [Clustering and Hierarchy: Tau
        Statistic](#clustering-and-hierarchy-tau-statistic)

# Chapter 9: R Tutorial on Centrality and Hierarchy

Built with R version 3.6.3

This tutorial walks through the analysis of centrality and hierarchy in
R. We will begin by looking at various centrality measures, to determine
how they are interrelated, and to discern what they mean. In this way
students develop different metrics for node network positioning. We will
then explore network-level measures of hierarchy, moving away from
individual level positioning to look at the structure of the whole
network. Note that this tutorial will build directly on the material
from previous tutorials, most clearly from the network measurement
tutorial (Ch 3) and the dyad/triad tutorial (Ch 7).

## Setting up the Session and Reading in Example Network Data

We will work primarily with the igraph package for this tutorial.

``` r
library(igraph)
```

This tutorial uses classroom network data collected by Daniel McFarland.
The class is a biology 2 class at a public high school. We will focus on
two network relations, one based on social interaction (i talks in a
social way with j) and another based on task-based interactions (i
actively engages in a task with j). Let’s go ahead and read in the
network data for the social relation.

``` r
social_data=read.csv(file="https://sites.google.com/site/jeffreysmithdatafiles/social_interactions_s641.csv")
```

Looking at the first six rows:

``` r
head(social_data) 
```

    ##   ego alter social_tie
    ## 1   1     1      0.000
    ## 2   1     2      0.000
    ## 3   1     3      0.000
    ## 4   1     4      0.000
    ## 5   1     5      5.625
    ## 6   1     6      1.500

We now have a data frame called social\_data. The first column is the
ego, the second column is the alter, and the third column shows the
frequency of social interactions between the two students. We will
reduce the data frame to just include those dyads where social
interaction occurred:

``` r
edgelist_social=social_data[social_data$social_tie>0, ] 
head(edgelist_social)
```

    ##    ego alter social_tie
    ## 5    1     5      5.625
    ## 6    1     6      1.500
    ## 22   1    22      1.875
    ## 44   2    22      0.375
    ## 74   4     8      1.875
    ## 89   5     1      5.250

Now we can go ahead and create our igraph object based on the edgelist
defined above. The size of the network is 22, so we will set the
vertices input to define the ids of the nodes, running from 1 to 22.

``` r
s641_social = graph_from_data_frame(d=edgelist_social, directed=T, vertices=(id=1:22)) 
s641_social
```

    ## IGRAPH 574ea68 DN-- 22 57 -- 
    ## + attr: name (v/c), social_tie (e/n)
    ## + edges from 574ea68 (vertex names):
    ##  [1] 1 ->5  1 ->6  1 ->22 2 ->22 4 ->8  5 ->1  5 ->6  5 ->22 6 ->1  6 ->5  6 ->9  7 ->10 8 ->4  9 ->6  10->7  10->12 11->15 12->10 12->16 12->18 15->11 16->12 16->17 16->18 16->19 16->22 17->16 17->18 17->19 17->21 18->12 18->16 18->17 18->19 18->20 18->21 19->16 19->17 19->18 19->20 19->21 19->22 20->18 20->19 20->21 21->17 21->18 21->20 21->22 22->1  22->2  22->5  22->11 22->16 22->18 22->19 22->21

Note that if we did not want the isolates included we could have done:

``` r
net641_social_noisolates = graph_from_data_frame(d=edgelist_social, directed=T) 
net641_social_noisolates
```

    ## IGRAPH 284d1f7 DN-- 19 57 -- 
    ## + attr: name (v/c), social_tie (e/n)
    ## + edges from 284d1f7 (vertex names):
    ##  [1] 1 ->5  1 ->6  1 ->22 2 ->22 4 ->8  5 ->1  5 ->6  5 ->22 6 ->1  6 ->5  6 ->9  7 ->10 8 ->4  9 ->6  10->7  10->12 11->15 12->10 12->16 12->18 15->11 16->12 16->17 16->18 16->19 16->22 17->16 17->18 17->19 17->21 18->12 18->16 18->17 18->19 18->20 18->21 19->16 19->17 19->18 19->20 19->21 19->22 20->18 20->19 20->21 21->17 21->18 21->20 21->22 22->1  22->2  22->5  22->11 22->16 22->18 22->19 22->21

And now we read in the task data:

``` r
task_data=read.csv(file="https://sites.google.com/site/jeffreysmithdatafiles/task_interactions_s641.csv")
head(task_data)
```

    ##   ego alter task_tie
    ## 1   1     1        0
    ## 2   1     2        0
    ## 3   1     3        0
    ## 4   1     4        0
    ## 5   1     5        0
    ## 6   1     6        0

The task\_tie variable shows the frequency of task-based interactions
between nodes i and j. We will now reduce the data to just those dyads
where a task interaction occurred and create the igraph object.

``` r
edgelist_task=task_data[task_data$task_tie>0, ] 
s641_task = graph_from_data_frame(d=edgelist_task, directed=T, vertices=(id=1:22)) 
s641_task
```

    ## IGRAPH 03e7947 DN-- 22 48 -- 
    ## + attr: name (v/c), task_tie (e/n)
    ## + edges from 03e7947 (vertex names):
    ##  [1] 1 ->22 2 ->22 4 ->8  5 ->22 6 ->22 7 ->22 8 ->4  9 ->22 10->22 11->22 13->18 13->22 14->22 15->22 16->19 16->22 17->18 17->21 17->22 18->13 18->17 18->21 18->22 19->16 19->20 19->22 20->19 20->22 21->17 21->18 21->22 22->1  22->2  22->5  22->6  22->7  22->9  22->10 22->11 22->13 22->14 22->15 22->16 22->17 22->18 22->19 22->20 22->21

We will now plot both networks.

``` r
par(mfrow=c(1,2)) 

plot(s641_social, vertex.frame.color=NA, edge.arrow.size=.5, edge.arrow.width=.75, 
     main="Social Interactions", margin=0)

plot(s641_task, vertex.frame.color=NA, edge.arrow.size=.5, edge.arrow.width=.75, 
     main="Task Interactions", margin=0)
```

![](lab_9_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

From the figure alone we can see that the features of these two networks
are very different. The task network would appear to have one very
central node, with lots of ties, while the social network splits more
clearly into groups with one node acting as a bridge between the groups.
We will use measures of centrality and centralization to more formally
explore these features. Our main substantive goal is to determine which
nodes are most important in the classroom and how (or if) this varies
across network relation and measure of centrality. Are individuals who
are prominent in the task network also prominent in the social
interaction network? Which nodes act as bridges? Are they the same nodes
with the highest degree? We also want to uncover something about the
overall level of inequality and hierarchy that exists in this classroom.
Is this a world where one node dominates?

## Centrality

#### 2.1 Centrality Measures for Social Interactions

Centrality measures, such as degree and betweenness, capture something
about which nodes are most important to the network. These node-level
measures can be used as predictors of other outcomes, like attitudes,
behaviors, etc. The centrality scores can also be used as the main
outcome of interest. Here, we will walk through the code to calculate a
number of commonly used measures, with a particular focus on how
different measures offer similar or different pictures of which nodes
are more central to the network. We begin with the social interaction
network.

Indegree centrality measures how many ties each node receives, in this
case the number of people that talks to node i in a social way. The
function is degree. The main inputs are graph (the network, as an igraph
object) and mode (in, out or total). For indegree we set mode to “in”.

``` r
indegree_social = degree(graph=s641_social, mode='in')
indegree_social 
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 
    ##  3  1  0  1  3  3  1  1  1  2  2  3  0  0  1  5  4  7  5  3  5  6

This means that 3 people talk to node 1, 1 person talks to node 2, and
so on. Outdegree centrality measures how many ties the node sends out,
in this case the number of people that the node talks to in a social
way. For outdegree we set mode to “out”.

``` r
outdegree_social = degree(graph=s641_social, mode='out')
outdegree_social
```

    ##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 
    ##  3  1  0  1  3  3  1  1  1  2  1  3  0  0  1  5  4  6  6  3  4  8

Closeness is the inverse of the mean geodesic distance between a given
node and all other nodes. In a directed network, we can think of
in-closeness centrality as the average number of steps one would have to
go through to get TO a given node FROM all other reachable nodes in the
network. Out-closeness centrality measures the same thing with the
directionality reversed (average number of steps to get from i to all
other j nodes). We invert the mean distance to make it a closeness
measure, where higher values mean the nodes are more central (i.e.,
closer to other nodes). The function is closeness and the inputs are:

  - graph=network of interest, as igraph object
  - mode=“in”, or “out”
  - normalized=T/F, should scores be normalized? (divided by n-1, where
    n is the number of people in the network)

Let’s first calculate out-closeness using the igraph function.

``` r
outcloseness_social = closeness(graph=s641_social, mode='out', normalized=F)
outcloseness_social
```

    ##           1           2           3           4           5           6           7           8           9          10          11          12          13          14          15          16          17          18          19          20          21          22 
    ## 0.006756757 0.006535948 0.002164502 0.002267574 0.006756757 0.006250000 0.005617978 0.002267574 0.005714286 0.006134969 0.002267574 0.006666667 0.002164502 0.002164502 0.002267574 0.007042254 0.006578947 0.006756757 0.006993007 0.006535948 0.006896552 0.007246377

We can interpret those values as the inverse of the mean distance of
each node to all other nodes. Now we will do the same thing for
in-closeness.

``` r
incloseness_social = closeness(graph=s641_social, mode='in', normalized=F)
incloseness_social
```

    ##           1           2           3           4           5           6           7           8           9          10          11          12          13          14          15          16          17          18          19          20          21          22 
    ## 0.005319149 0.005181347 0.002164502 0.002267574 0.005319149 0.005050505 0.004739336 0.002267574 0.004739336 0.005050505 0.006578947 0.005347594 0.002164502 0.002164502 0.005988024 0.005524862 0.005291005 0.005586592 0.005464481 0.005263158 0.005464481 0.005555556

Note that igraph assumes for nodes that are not reachable that the
distance (to that node) is the number of nodes in the network, which may
not actually make sense. For example, node 3 has a closeness centrality
score of .002, but this node is a true isolate, with no ties coming in
or going out (see degree calcuations above).

An alternative version of this is to take the mean based on the inverse
distance matrix. This avoids the problem of summing up over infinite
distances when calculating the mean distance to or from each node. Here
we get the distance matrix in the right form for that calculation.

``` r
dist_mat_social=distances(graph=s641_social, mode="out") #calculating the distance matrix
diag(dist_mat_social)=NA #ignoring the diagonal
dist_mat_social_inverted=1/dist_mat_social #getting inverted distance matrix
```

Now we can use an apply statement to take the mean over all of the rows
at once. Note that we use a 1 in the apply statement to take the mean
over the rows, set FUN to the mean and include an option to exclude the
NAs.

``` r
outcloseness_social2=apply(dist_mat_social_inverted, MARGIN=1, FUN=mean, na.rm=T) 
outcloseness_social2
```

    ##          1          2          3          4          5          6          7          8          9         10         11         12         13         14         15         16         17         18         19         20         21         22 
    ## 0.39444444 0.32698413 0.00000000 0.04761905 0.39444444 0.32698413 0.22346939 0.04761905 0.23537415 0.29682540 0.04761905 0.38253968 0.00000000 0.00000000 0.04761905 0.46428571 0.38650794 0.44603175 0.47619048 0.36269841 0.42857143 0.54365079

Let’s compare the two versions of out-closeness centrality:

``` r
cor(outcloseness_social, outcloseness_social2) 
```

    ## [1] 0.9710186

We can see that the two versions of out-closeness are very highly
correlated but not identical. And now we calculate the alternative
version of in-closeness. Here, we take the mean over each column of the
inverted distance matrix to get the in-coming paths. This is
accomplished by using a 2 for MARGIN in the apply statement:

``` r
incloseness_social2=apply(dist_mat_social_inverted, MARGIN=2 , FUN=mean, na.rm=T) 
incloseness_social2
```

    ##          1          2          3          4          5          6          7          8          9         10         11         12         13         14         15         16         17         18         19         20         21         22 
    ## 0.34682540 0.27936508 0.00000000 0.04761905 0.34682540 0.29523810 0.20600907 0.04761905 0.21156463 0.27539683 0.35079365 0.35476190 0.00000000 0.00000000 0.25317460 0.42460317 0.35873016 0.47222222 0.41269841 0.33492063 0.41269841 0.44841270

Betweenness centrality is based on the number of shortest paths going
through a specific vertex; it is returned by the betweenness function.
We need to set normalized to T/F, determining if the scores should be
normalized by the number of pairs of nodes (not including i), capturing
the number of possible paths a node could be between.

``` r
betweenness_social = betweenness(graph=s641_social, normalized=F) 
betweenness_social
```

    ##           1           2           3           4           5           6           7           8           9          10          11          12          13          14          15          16          17          18          19          20          21          22 
    ##  24.0000000   0.0000000   0.0000000   0.0000000  24.0000000  28.0000000   0.0000000   0.0000000   0.0000000  28.0000000  15.0000000  52.0000000   0.0000000   0.0000000   0.0000000  45.8333333   0.8333333  33.0000000  14.7500000   0.2500000  13.5000000 126.8333333

Eigenvector centrality gives greater weight to a node the more it is
connected to other highly connected nodes. A node connected to five
high-scoring nodes will have higher eigenvector centrality than a node
connected to five low-scoring nodes. Thus, it is often interpreted as
measuring a node’s network importance. In directed networks, there are
‘In’ and ‘Out’ versions. In information flow studies, for instance,
In-Eigenvector scores would reflect which nodes are high on receiving
information, while Out-Eigenvector scores would reflect which nodes are
high on broadcasting information. For these data, we will simply
symmetrize to generate an undirected eigenvector centrality score. Note
that, unlike the other centrality measures, the evcent function returns
a complex object rather than a simple vector. Thus, we need to first get
the evcent output and then select the eigenvector scores from it. Here
we symmetrize our network before calculating eigenvector centrality,
making a tie between i and j if i is tied with j or j is tied with i.

``` r
s641_social_undirected = as.undirected(s641_social, mode='collapse') 
```

We now calculate eigenvector centrality:

``` r
ev_obj_social = eigen_centrality(s641_social_undirected)
eigen_social =  ev_obj_social$vector
eigen_social
```

    ##          1          2          3          4          5          6          7          8          9         10         11         12         13         14         15         16         17         18         19         20         21         22 
    ## 0.24182507 0.17353661 0.00000000 0.00000000 0.24182507 0.10048409 0.01530350 0.00000000 0.02004592 0.07671178 0.18072921 0.36922920 0.00000000 0.00000000 0.03605430 0.77412141 0.70272724 1.00000000 0.93859173 0.54829505 0.80984389 0.86988603

Note that we can calculate a closely related Bonacich power centrality
score using the power\_centrality function.

We now have a basic set of centrality scores for our social interaction
network. To facilitate comparison across the different measures, we’ll
construct a data frame with the nodes as rows and the centrality scores
as columns. We include a variable for the network type, here social
interaction, as well as the ids of the nodes. We first grab the ids from
the igraph object.

``` r
ids=V(s641_social)$name
```

And now we put together the data frame.

``` r
central_social = data.frame(ids=ids, net="social", indegree=indegree_social, outdegree=outdegree_social,  
                            incloseness=incloseness_social, incloseness2=incloseness_social2, 
                            outcloseness2=outcloseness_social2,
                            between=betweenness_social, eigen=eigen_social)
head(central_social)
```

    ##   ids    net indegree outdegree incloseness incloseness2 outcloseness2 between     eigen
    ## 1   1 social        3         3 0.005319149   0.34682540    0.39444444      24 0.2418251
    ## 2   2 social        1         1 0.005181347   0.27936508    0.32698413       0 0.1735366
    ## 3   3 social        0         0 0.002164502   0.00000000    0.00000000       0 0.0000000
    ## 4   4 social        1         1 0.002267574   0.04761905    0.04761905       0 0.0000000
    ## 5   5 social        3         3 0.005319149   0.34682540    0.39444444      24 0.2418251
    ## 6   6 social        3         3 0.005050505   0.29523810    0.32698413      28 0.1004841

Now we’ll examine the table to find the most central nodes according to
the different measures we have. When looking at each of these measures,
it’s a good idea to have your plot of the social interaction network on
hand so you can sanity-check the results.

Let’s order each column from high to low centrality seeing which nodes
have the top centrality on each measure. We will use an order function
to rank order the nodes. The order function returns a vector in
ascending or descending order, showing which cases have the
highest/lowest values. We will set decreasing equal to T to make it
descending (running high to low). We will use the order function within
an apply statement. We use apply to order all of the columns at once. We
exclude the first two columns as they are not centrality scores.

``` r
apply(central_social[, -c(1,2)], MARGIN=2, FUN=order, decreasing=T)
```

    ##       indegree outdegree incloseness incloseness2 outcloseness2 between eigen
    ##  [1,]       18        22          11           18            22      22    18
    ##  [2,]       22        18          15           22            19      12    19
    ##  [3,]       16        19          18           16            16      16    22
    ##  [4,]       19        16          22           19            18      18    21
    ##  [5,]       21        17          16           21            21       6    16
    ##  [6,]       17        21          19           17             1      10    17
    ##  [7,]        1         1          21           12             5       1    20
    ##  [8,]        5         5          12           11            17       5    12
    ##  [9,]        6         6           1            1            12      11     1
    ## [10,]       12        12           5            5            20      19     5
    ## [11,]       20        20          17           20             2      21    11
    ## [12,]       10        10          20            6             6      17     2
    ## [13,]       11         2           2            2            10      20     6
    ## [14,]        2         4           6           10             9       2    10
    ## [15,]        4         7          10           15             7       3    15
    ## [16,]        7         8           7            9             4       4     9
    ## [17,]        8         9           9            7             8       7     7
    ## [18,]        9        11           4            4            11       8     3
    ## [19,]       15        15           8            8            15       9     4
    ## [20,]        3         3           3            3             3      13     8
    ## [21,]       13        13          13           13            13      14    13
    ## [22,]       14        14          14           14            14      15    14

If we start with indegree, we see the top nodes are 18, 22 and 16. Or,
for outdegree, the top three nodes are 22, 18 and 19. With In-closeness
we see nodes 11, 15 and 18 have the highest values. A visual inspection
of the plot suggests that 11, 15, and 18 are not central nodes at all.
This seems at odd first. We must remember, however, that igraph takes
the number of nodes instead of path length when there is no direct path
available between node i and j. So for example, node 22 looks really
central, much more central than 11 and 15. The reason why 22 gets a
lower closeness score than 11 and 15 is that both 11 and 15 cannot reach
22 directly, whereas 22 (and the rest of the network) can reach 11 and
15. So, nodes 15 and 11 get score 22 (N nodes) five times (because there
is no path with node 4 and 8 and there are 3 isolates), whereas node 22
gets score 22 seven times (node 4, 8, 11, and 15 and the three
isolates). This outweighs the other shortest paths node 22 gets compared
to 11 and 15. If this is not what we want, we could look at our
alternative version of closeness, incloseness2 (based on the inverted
distance matrix). We now see nodes 18, 22 and 16 are most central, much
more in line with what might expect based on the figure. Finally, we see
similar, but somewhat different ordering for betweenness, with nodes 22,
12 and 16 the top three. Node 12 in particular has much higher
betweenness centrality than they do for degree, closeness, etc.

Let’s make a plot with these summary statistics. To visualize these
data, we can create a barplot for our centrality measures. To make this
task a little easier, we will first rearrange our dataset to make it a
‘long’ format. We will focus on just a handful of our centrality
measures, indegree, incloseness2, betweenness and eigen centrality. Here
we use the melt function (in the reshape package) to rearrange our data,
basically stacking the variables of interest (here centrality scores) on
top of each other, differentiated by an id.vars variables (here the ids
of the nodes).

``` r
library(reshape)
social_long=melt(central_social[,c("ids","indegree","incloseness2","between","eigen")], id.vars="ids")  
head(social_long)
```

    ##   ids variable value
    ## 1   1 indegree     3
    ## 2   2 indegree     1
    ## 3   3 indegree     0
    ## 4   4 indegree     1
    ## 5   5 indegree     3
    ## 6   6 indegree     3

We can see that we now have three variables. An ids variable
corresponding to the node; a variable corresponding to the centrality
measure and value corresponding to the actual score for that person on
that centrality measure. Now, let’s create a barplot using ggplot for
each of our centrality scores. The y-axis is the score, and the x-axis
is the node. We will have 4 different panels, one for each measure of
interest. Within ggplot, we use the aes function and the geom\_bar
function to create the basic bar plot for each node (ids) based on the
value variable; and a facet\_wrap function to set up the plot to have 2
columns.

``` r
library(ggplot2)
ggplot(social_long, aes(x=factor(ids, levels=1:length(ids)), y=value))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~variable, scales="free_y", ncol=2)+
  xlab("Ids")+ylab("Centrality")+
 theme(axis.text=element_text(size=6.5))
```

![](lab_9_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Given our results, what can we say about the social nodes if we compare
the bar plots? Who seems to run the show in sociable affairs? Who seems
to bridge sociable conversations? For example, we can see that node 22
plays a key bridging role and shows up as highly centrality in general,
but particularly for betweenness. Node 22 has the highest centrality by
far in betweenness but has similar indegree to other top nodes. This
suggests that multiple nodes (18, 16, 19, 22) have high volume (or
prominence) in the network, but only one plays the role of bridging
different groups in the classroom (22). To highlight this, let’s go back
to our picture and size the nodes by betweenness centrality (scaled a
bit to make the picture a little nicer):

``` r
plot(s641_social, vertex.size=central_social$between/5, vertex.label=V(s641_social)$name, 
     edge.arrow.size = 0.5, layout=layout.fruchterman.reingold, 
     main='Classroom S641 Social Talk', margin=0)
```

![](lab_9_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

#### 2.2. Correlations between Centrality Measures

We have so far seen which nodes are the most central on different
measures. We now want to formalize this a bit more by computing the
correlations between the centrality scores, showing how closely these
measures of centrality are interrelated. More substantively, we want to
know which measures tend to yield the same nodes as central and which
tend to disagree on the most important nodes. Here we generate a table
of pairwise correlations. Again, we take out the first column of ids and
the second column showing the network type when doing the correlation
matrix.

``` r
cor(central_social[,-c(1,2)]) 
```

    ##                indegree outdegree incloseness incloseness2 outcloseness2   between     eigen
    ## indegree      1.0000000 0.9602386   0.6221517    0.8741212     0.8600408 0.6287438 0.9400209
    ## outdegree     0.9602386 1.0000000   0.5614011    0.8218450     0.8743423 0.7376241 0.9140148
    ## incloseness   0.6221517 0.5614011   1.0000000    0.9138497     0.6732677 0.3765280 0.5319454
    ## incloseness2  0.8741212 0.8218450   0.9138497    1.0000000     0.8787554 0.5479545 0.7943102
    ## outcloseness2 0.8600408 0.8743423   0.6732677    0.8787554     1.0000000 0.5950761 0.7995006
    ## between       0.6287438 0.7376241   0.3765280    0.5479545     0.5950761 1.0000000 0.5065595
    ## eigen         0.9400209 0.9140148   0.5319454    0.7943102     0.7995006 0.5065595 1.0000000

Indegree and outdegree are very closely correlated (rho = 0.96),
indicating that social talk with others is almost always reciprocated
(i.e., if you talk to others, they tend to talk back to you). Indegree
and outdegree are also highly correlated with eigenvector centrality,
indicating that the students that talk the most to others (or,
relatedly, are talked to the most by others) are also the ones that are
connected to other highly connected students – possibly indicating high
density cliques around these individuals. The degree centralities are
less correlated with our closeness centrality scores, suggesting that
nodes with high degree are not always (although often) close to other
nodes.

Betweenness shows the highest correlation with outdegree, followed by
indegree. In the case of this particular network, it seems that the
individuals that talk to the most others are the likeliest to serve as
bridges between the particular cliques (see, e.g., 22 in the plot). Note
that betweenness is not all that highly correlated with closeness
centrality. This suggests that nodes may sit between groups, and thus
have high betweenness, but not necessarily be close to all nodes, on
average. For example, node 19 has high closeness centrality but not
especially high betweenness centrality. If we look at the last plot, we
can see that node is 19 deeply embedded in one social group and has ties
to node 22, who has high betweenness, connecting different parts of of
the network. Thus, node 19 has high closeness as they can reach everyone
else (through node 22) but low betweenness, as the shortest paths
connecting different groups would not have to run through node 19.

Thus, if the process that we thought was most important was about
information flow based on shortest paths, we may think that node 19 is
well positioned to influence the rest of the network. If, however, the
key is being the bridge itself, then 19 is clearly not as important as
node 22. Thus, while there is much agreement between the centrality
scores (with nodes 22, 16, 18 and 19 showing up consistently as central)
it is possible for a node to be high on one measure and low on another.

#### 2.3. Centrality for Task Interactions

We now repeat the analysis for the task interaction network.

``` r
indegree_task = degree(s641_task, mode='in')
outdegree_task = degree(s641_task, mode='out')
incloseness_task =  closeness(s641_task, mode='in', normalized=F)
outcloseness_task = closeness(s641_task, mode='out',normalized=F)

dist_mat_task=distances(graph=s641_task, mode="out")
diag(dist_mat_task)=NA
dist_mat_task_inverted= 1/dist_mat_task

outcloseness_task2=apply(dist_mat_task_inverted,MARGIN=1, FUN=mean, na.rm=T)
incloseness_task2=apply(dist_mat_task_inverted, MARGIN=2 , FUN=mean, na.rm=T) 

betweenness_task = betweenness(s641_task, normalized=F)

s641_task_undirected = as.undirected(s641_task, mode='collapse')
ev_obj_task = evcent(s641_task_undirected)
eigen_task = ev_obj_task$vector
```

And now we put the results together, as before, into a data frame with
all the centrality values

``` r
central_task = data.frame(ids=ids, net="task",indegree=indegree_task, outdegree=outdegree_task,  
                          incloseness=incloseness_task,incloseness2=incloseness_task2, 
                          outcloseness2=outcloseness_task2, 
                          between=betweenness_task, eigen=eigen_task)
head(central_task)
```

    ##   ids  net indegree outdegree incloseness incloseness2 outcloseness2 between        eigen
    ## 1   1 task        1         1 0.008264463   0.42857143    0.42857143       0 2.154856e-01
    ## 2   2 task        1         1 0.008264463   0.42857143    0.42857143       0 2.154856e-01
    ## 3   3 task        0         0 0.002164502   0.00000000    0.00000000       0 4.246452e-17
    ## 4   4 task        1         1 0.002267574   0.04761905    0.04761905       0 3.224155e-17
    ## 5   5 task        1         1 0.008264463   0.42857143    0.42857143       0 2.154856e-01
    ## 6   6 task        1         1 0.008264463   0.42857143    0.42857143       0 2.154856e-01

We will now quickly take a look at the nodes with the top centrality
scores for the task network.

``` r
apply(central_task[, -c(1,2)], MARGIN=2, FUN=order, decreasing=T)
```

    ##       indegree outdegree incloseness incloseness2 outcloseness2 between eigen
    ##  [1,]       22        22          22           22            22      22    22
    ##  [2,]       18        18          18           18            18      18    18
    ##  [3,]       17        17          17           17            17      19    21
    ##  [4,]       19        19          19           19            19       1    17
    ##  [5,]       21        21          21           21            21       2    19
    ##  [6,]       13        13          13           13            13       3    13
    ##  [7,]       16        16          16           16            16       4    16
    ##  [8,]       20        20          20           20            20       5    20
    ##  [9,]        1         1           1            1             1       6     7
    ## [10,]        2         2           2            2             2       7     5
    ## [11,]        4         4           5            5             5       8     1
    ## [12,]        5         5           6            6             6       9     2
    ## [13,]        6         6           7            7             7      10     6
    ## [14,]        7         7           9            9             9      11     9
    ## [15,]        8         8          10           10            10      12    10
    ## [16,]        9         9          11           11            11      13    11
    ## [17,]       10        10          14           14            14      14    14
    ## [18,]       11        11          15           15            15      15    15
    ## [19,]       14        14           4            4             4      16     8
    ## [20,]       15        15           8            8             8      17     3
    ## [21,]        3         3           3            3             3      20    12
    ## [22,]       12        12          12           12            12      21     4

In this case, we can see nodes 22, 18 and 17 are consistently the most
important nodes, but node 22 is by the far most central. This becomes
clear if we plot the network, scaling the nodes by indegree:

``` r
plot(s641_task, vertex.size=central_task$indegree, vertex.label=V(s641_social)$name, edge.arrow.size = 0.5,
     edge.arrow.width=.75, layout=layout.fruchterman.reingold, main='Classroom S641 Taks Interactions', margin=0)
```

![](lab_9_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

#### 2.4. Task/Social Correlations

We have seen that node 22 dominates task interactions in a way that was
less clear-cut in the social interactions network. Let’s explore the
similarities/differences between the two networks by doing a quick bar
plot comparing four centrality measures (indegree, incloseness2,
betweenness and eigen) on social and task centrality. We will first
rearrange the task centrality data frame, as we did with the social
centrality data frame, to make it in a long format.

``` r
task_long=melt(central_task[,c("ids","indegree","incloseness2","between","eigen")], id.vars="ids") 
head(task_long)
```

    ##   ids variable value
    ## 1   1 indegree     1
    ## 2   2 indegree     1
    ## 3   3 indegree     0
    ## 4   4 indegree     1
    ## 5   5 indegree     1
    ## 6   6 indegree     1

Now, let’s also add a variable in both the social and task data frames
that indicate network type.

``` r
task_long$net="task"
social_long$net="social"
```

Now we will put together the two long format data frames:

``` r
social_task_long=rbind(social_long, task_long)
```

Now, let’s produce the same barplot as before, but include the task
centrality score along side the social centrality scores. This is
accomplished by adding a fill option (in the aes function) set to the
net variable, and by setting the colors using a scale\_fill\_discrete
function (to distinguish social from task).

``` r
ggplot(social_task_long, aes(x=factor(ids, levels=1:length(ids)), y=value, fill=net))+
  geom_bar(stat="identity", position="dodge")+
  facet_wrap(~variable, scales="free_y", ncol=2)+ 
  scale_fill_discrete(name="Network", breaks=c("social", "task"), labels=c("Social", "Task"))+
  xlab("Ids")+ylab("Centrality")+
  theme(axis.text=element_text(size=6.5))
```

![](lab_9_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

We can see node 22 has extremely high centrality in the task network
over all measures of centrality. Node 22 also tends to be high
centrality in the social interaction network, but there are other nodes
(like 16, 18 or 19) with similar centrality scores (except for
betweenness). Now, let’s calculate the correlation between task and
social over all our measures.

``` r
cor_tab=cor(central_social[,-c(1,2)], central_task[,-c(1,2)])
```

Let’s add some useful row and column names to our correlation table. We
will a paste command to tack "\_social" onto the existing row names

``` r
rownames(cor_tab)=paste(rownames(cor_tab), "social", sep="_")
```

And here we add "\_task" to the end of the column names.

``` r
colnames(cor_tab)=paste(colnames(cor_tab), "task", sep="_") 
cor_tab
```

    ##                      indegree_task outdegree_task incloseness_task incloseness2_task outcloseness2_task between_task eigen_task
    ## indegree_social          0.5578869      0.5578869        0.3671824         0.4938461          0.4938461    0.3783934  0.6499549
    ## outdegree_social         0.6996636      0.6996636        0.3613162         0.5363748          0.5363748    0.5543594  0.7344542
    ## incloseness_social       0.2181329      0.2181329        0.5676880         0.5410482          0.5410482    0.1433931  0.4129348
    ## incloseness2_social      0.3935895      0.3935895        0.5103380         0.5540266          0.5540266    0.2652492  0.5531783
    ## outcloseness2_social     0.4597653      0.4597653        0.4267209         0.5068913          0.5068913    0.3400393  0.5698573
    ## between_social           0.8034418      0.8034418        0.1765858         0.4120686          0.4120686    0.8318598  0.6806832
    ## eigen_social             0.5491476      0.5491476        0.3569532         0.4812561          0.4812561    0.3438770  0.6571219

eigen\_task is correlated with betweenness\_social (rho=.68) and
outdegree (rho=.73), possibly because those who are important in tasks
also serve as bridges for talk on social issues and have many outbound
ties. indegree\_task and between\_social (rho=.80), and outdegree\_task
and between\_social (rho=.80) are correlated, possibly because the
number of indegree and outdegree ties a node has with respect to task
talk, the more they serve as a bridge on social talk. incloseness\_task2
and incloseness\_social2 (rho=.55) are correlated, meaning that those
who serve in shortest paths past on inbound ties are equivalent for both
social talk and task talk.

## Centralization and the Distribution of Centrality

We have so far seen which nodes are most important in the classroom
using different definitions of centrality. We have also seen how this
differs across social and task interactions. To flesh out the story more
clearly, it will be useful to formally summarize the distribution of the
centrality measures, telling us something about the network as a whole.
For example, a network with one node capturing the vast majority of ties
is highly centralized, or highly unequal, as all of the activity in the
network is focused on a particular node. A highly centralized network is
also relatively fragile, as removing the one central node would greatly
reduce the connectivity of the network. We could examine the
distribution of any of the centrality scores we calculated above. Here,
let’s focus on indegree as a way of exploring the level of
centralization in the two networks. Let’s start with a summary of the
indegree distributions.

``` r
summary(indegree_social)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   1.000   2.500   2.591   3.750   7.000

``` r
summary(indegree_task)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   1.000   1.000   2.182   2.000  17.000

``` r
sd(indegree_social)
```

    ## [1] 2.03912

``` r
sd(indegree_task)
```

    ## [1] 3.459099

We can see that the mean and median of indegree is higher in the social
interaction network than in the task network. In contrast, the maximum
value, as well as the standard deviation, of indegree is much higher in
the task network. This confirms our story from above, where the task
network is centered strongly on one node, while the social interaction
network is based more on groups, where a single node won’t necessarily
dominate. Note that a simple standard deviation score can serve as an
effective measure of centralization. It is also possible to employ
traditional centralization measures.

To calculate centralization, we take the centrality scores of interest
and sum up the total deviations from the highest value. We then
typically divide the total summation by the maximum possible level of
centralization in a network of that size (i.e., the centralization we
would have observed in a hub and spoke structure).

igraph has different centralization functions for each centrality score.
For degree the function is cent\_degree. The inputs are graph (network
of interest), mode (in, out, total) loops (T/F should self-loops be
considered), and normalized (T/F should divide by theoretical max?) Here
we calculate indegree centralization for the social interaction network,
ignoring self loops and dividing by the theoretical max.

``` r
cent_social=centr_degree(graph=s641_social, mode = "in", loops = FALSE, normalized = TRUE)
cent_social
```

    ## $res
    ##  [1] 3 1 0 1 3 3 1 1 1 2 2 3 0 0 1 5 4 7 5 3 5 6
    ## 
    ## $centralization
    ## [1] 0.2199546
    ## 
    ## $theoretical_max
    ## [1] 441

We could also calculate this directly by doing:

``` r
sum(max(indegree_social)-indegree_social)/sum(21-rep(0,21)) 
```

    ## [1] 0.2199546

The code simply takes the max centrality score and subtracts the
centrality of each node in the network, summing over all nodes. We then
divide by the theoretical max, the centralization score if one node
received nominations from everyone (indegree=21 in this case) and
everyone else received none (indegree=0).

And now we do the same thing for the task network.

``` r
cent_task=centr_degree(graph=s641_task, mode =  "in", loops = FALSE,normalized = TRUE)
cent_task
```

    ## $res
    ##  [1]  1  1  0  1  1  1  1  1  1  1  1  0  2  1  1  2  3  4  3  2  3 17
    ## 
    ## $centralization
    ## [1] 0.739229
    ## 
    ## $theoretical_max
    ## [1] 441

Clearly, the task network is considerably more centralized. In fact, the
task network almost approaches maximum centralization, or a perfect hub
and spoke structure.

Now, let’s do a simple plot of the two indegree distributions. We will
put indegree on the x-axis and plot a smoothed density curve for each
distribution. First, we need to get the density curves for each network,
starting with the social interaction network.

``` r
den_social=density(indegree_social, from=0) #from is set to 0 as indegree cannot be less than 0.
```

And now for the task network:

``` r
den_task=density(indegree_task, from=0)
```

And now we set up the plot, plot the two lines and add a legend.

``` r
plot(range(den_social$x, den_task$x), range(den_social$y,den_task$y), type="n", xlab="degree", 
     ylab="density", main="Indegree Distribution for Social and Task Networks")
#QQQ
lines(den_social, col ="red" , lty = 2, lwd = 2)
lines(den_task, col = "light blue", lty = 2, lwd = 2)
legend("topright", c("Social", "Task"),col=c("red", "light blue"), lty=2, lwd=2) 
```

![](lab_9_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

Here we see that for the task network most people have one or two ties
and one person has a very high indegree. The social interaction network
has a much more even distribution, with many people close to the mean.
The story is clear that the task network is highly centralized, with one
node being the focal point of all of the task interactions. Social
interactions are much evenly dispersed, occurring within groups of
people but not centered on a single well-connected node.

More generally, our centrality and centralization analyses paint a
picture of two different kinds of interactional tendencies. For the
social interaction network, we have a set of divided groups bridged by
one focal node with high betweenness. Within each group there are
prominent nodes with high degree, closeness, etc. but only one node
holds the whole network together. For the task network, there is only
one focal node, with everyone doing task interactions with them and few
interactions happening otherwise.

## Clustering and Hierarchy: Tau Statistic

We have so far used centrality and centralization to explore the
classroom networks. Centrality is focused on individual positions in the
network and can tell us who holds important positions and who does not.
Centralization helps us understand how unequally distributed centrality
is in the network. Neither measure (centrality nor centralization) can
tell us much about hierarchy at the group level. We may, however, want
to know if the groups that exist in our classroom are themselves
hierarchically arranged.

To explore hierarchy at the group-level, it will be useful to consider
other kinds of measures. Here, we will use the tau statistic. The tau
statistic captures how micro processes aggregate to create different
macro structures. The basic idea is to create hypotheses in the form of
different triad counts (the micro processes), that should yield
different features at the macro-level. Thus, different micro hypotheses
(about which triads should be in the network at high/low rates)
correspond to different kinds of emergent features at the macro level.
By comparing the observed triad counts to that expected under a null
hypothesis, we can see what kinds of hierarchical arrangements exist in
the network of interest. Formally, we compare the observed triad counts
to the expectations under a null hypothesis of a random network with the
same dyad census. This is analogous to the kinds of tests we explored in
Chapter 7, but the tau statistic is presented as a z-score (how many
standard deviations from the null hypothesis is the observed value),
making it akin to more traditional statistical tests.

Let’s first transform our network from an igraph object into a network
object using the intergraph package (as the function we need assumes a
network object).

``` r
library(intergraph)
```

For this analysis we will focus on the social interaction network.

``` r
s641_social_network=asNetwork(s641_social)
```

Here we read in a function to calculate the tau statistic.

``` r
source(file="https://sites.google.com/site/jeffreysmithdatafiles/tau_functions.R")
```

Now we load the ergm and sna packages which is used by the tau function
read in above.

``` r
library(ergm)
library(sna)
```

Let’s test different hypotheses about the distribution of triads in the
network, telling us something about the macro structure in terms of
hierarchy. We will consider a ranked clustering hypothesis, a clustering
hypothesis and a balance hypothesis. Each hypothesis is represented by a
vector, indicating which triads should be summed up and compared to our
baseline expectations. The triad types are:

  - 003 A, B, C, empty triad.
  - 012 A-\>B, C
  - 102 A\<-\>B, C  
  - 021D A\<-B-\>C
  - 021U A-\>B\<-C
  - 021C A-\>B-\>C
  - 111D A\<-\>B\<-C
  - 111U A\<-\>B-\>C
  - 030T A-\>B\<-C, A-\>C
  - 030C A\<-B\<-C, A-\>C.
  - 201 A\<-\>B\<-\>C.
  - 120D A\<-B-\>C, A\<-\>C.
  - 120U A-\>B\<-C, A\<-\>C.
  - 120C A-\>B-\>C, A\<-\>C.
  - 210 A-\>B\<-\>C, A\<-\>C.
  - 300 A\<-\>B\<-\>C, A\<-\>C, completely connected.

A ranked clustering hypothesis poses that 003, 102, 021D, 021U, 030T,
120D, 120U and 300 should be present in the network at higher rates than
we what we expect based on dyadic processes alone. The idea is that a
network with these triads will tend to create macro structures that
correspond to ranked clustering, where there are mutual ties within
groups and asymmetric ties across groups; where the lower status groups
send ties to higher status groups but not vice versa. Let’s create a
vector that corresponds to the ranked clustering hypothesis, putting a 1
in each spot of the triad census (following the order above) that
corresponds to a triad in that hypothesis.

``` r
weight_vector_rankedcluster=c(1, 0 ,1 ,1 ,1 ,0 ,0, 0, 1, 0, 0 ,1, 1, 0, 0, 1)
```

A clustering hypothesis poses that 003, 102, and 300 should be present
in the network at higher rates than we what we expect based on dyadic
processes alone. The clear difference with the ranked clustering
hypothesis is that triads that create hierarchies (021U, 120U, etc.) are
not included here. The macro network structure implied by a triad census
fitting the clustering model is one with a number of social groups, with
mutual ties within groups and few ties between groups. Thus, there are a
number of groups differentiated by high internal rates of interaction
(see Chapter 8) but there is no clear hierarchy between the groups.

``` r
weight_vector_cluster=c(1, 0 ,1 ,0,0 ,0 ,0, 0, 0, 0, 0 ,0, 0, 0, 0, 1)
```

A balance hypothesis is the simplest hypothesis and only includes 102
and 300. This is very similar to the clustering hypothesis but differs
in the exclusion of the null triad, 003. The key macro structural
difference is that the clustering hypothesis implies a number of social
groups to emerge (with no hierarchy) while the balance hypothesis
implies that only two groups should emerge, with mutual ties within the
groups and few ties between.

``` r
weight_vector_balance=c(0, 0 ,1 ,0,0 ,0 ,0, 0, 0, 0, 0 ,0, 0, 0, 0, 1)
```

The function is tau\_stat\_function. The inputs are the network and the
weight vector. For ranked clustering:

``` r
tau_rankedcluster=tau_stat_function(network=s641_social_network, weight.vector=weight_vector_rankedcluster)
tau_rankedcluster 
```

    ## $tau
    ##          [,1]
    ## [1,] 2.968397
    ## 
    ## [[2]]
    ##                  observed.triads expected.triads weight.vector
    ## triadcensus.003             1029    1.012569e+03             1
    ## triadcensus.012               37    4.579457e+01             0
    ## triadcensus.102              403    4.121511e+02             1
    ## triadcensus.021D               1    1.144864e-01             1
    ## triadcensus.021U               0    1.144864e-01             1
    ## triadcensus.021C               0    2.289728e-01             0
    ## triadcensus.111D               4    6.182267e+00             0
    ## triadcensus.111U              10    6.182267e+00             0
    ## triadcensus.030T               0    5.695842e-04             1
    ## triadcensus.030C               0    1.898614e-04             0
    ## triadcensus.201               38    5.357965e+01             0
    ## triadcensus.120D               0    1.537877e-02             1
    ## triadcensus.120U               0    1.537877e-02             1
    ## triadcensus.120C               0    3.075755e-02             0
    ## triadcensus.210                7    7.996962e-01             0
    ## triadcensus.300               11    2.221378e+00             1

The output is a list, with the first element the tau statistic and the
second a data frame with the observed and expected triads, as well as
the weighting vector. Now for the clustering hypothesis:

``` r
tau_cluster=tau_stat_function(network=s641_social_network, weight.vector=weight_vector_cluster)
tau_cluster
```

    ## $tau
    ##          [,1]
    ## [1,] 2.867246
    ## 
    ## [[2]]
    ##                  observed.triads expected.triads weight.vector
    ## triadcensus.003             1029    1.012569e+03             1
    ## triadcensus.012               37    4.579457e+01             0
    ## triadcensus.102              403    4.121511e+02             1
    ## triadcensus.021D               1    1.144864e-01             0
    ## triadcensus.021U               0    1.144864e-01             0
    ## triadcensus.021C               0    2.289728e-01             0
    ## triadcensus.111D               4    6.182267e+00             0
    ## triadcensus.111U              10    6.182267e+00             0
    ## triadcensus.030T               0    5.695842e-04             0
    ## triadcensus.030C               0    1.898614e-04             0
    ## triadcensus.201               38    5.357965e+01             0
    ## triadcensus.120D               0    1.537877e-02             0
    ## triadcensus.120U               0    1.537877e-02             0
    ## triadcensus.120C               0    3.075755e-02             0
    ## triadcensus.210                7    7.996962e-01             0
    ## triadcensus.300               11    2.221378e+00             1

Now for the balance hypothesis:

``` r
tau_balance=tau_stat_function(network=s641_social_network, weight.vector=weight_vector_balance)
tau_balance
```

    ## $tau
    ##             [,1]
    ## [1,] -0.03377649
    ## 
    ## [[2]]
    ##                  observed.triads expected.triads weight.vector
    ## triadcensus.003             1029    1.012569e+03             0
    ## triadcensus.012               37    4.579457e+01             0
    ## triadcensus.102              403    4.121511e+02             1
    ## triadcensus.021D               1    1.144864e-01             0
    ## triadcensus.021U               0    1.144864e-01             0
    ## triadcensus.021C               0    2.289728e-01             0
    ## triadcensus.111D               4    6.182267e+00             0
    ## triadcensus.111U              10    6.182267e+00             0
    ## triadcensus.030T               0    5.695842e-04             0
    ## triadcensus.030C               0    1.898614e-04             0
    ## triadcensus.201               38    5.357965e+01             0
    ## triadcensus.120D               0    1.537877e-02             0
    ## triadcensus.120U               0    1.537877e-02             0
    ## triadcensus.120C               0    3.075755e-02             0
    ## triadcensus.210                7    7.996962e-01             0
    ## triadcensus.300               11    2.221378e+00             1

In general, larger values offer support for the hypothesis in question.
We can see here that there is little support for the balance hypothesis
compared to the other hypotheses. This suggests that the balance
hypothesis is too simple. More specifically, it looks like there are
many more null triads (003) than we would expect in a network where
everyone falls into two groups (under the balance hypothesis). The tau
statistics are similar between the ranked clustering and clustering
hypotheses. A value of 2.97 (for ranked clustering) suggests that the
observed (summed) counts are about 3 standard deviations away from what
we expect under the null. Values over 2 offer support for the hypothesis
in question under traditional hypothesis testing criteria.

Let’s take a closer look at the results for the ranked clustering model.
We will focus on the expected and observed triad counts, particularly
those triads that are in the ranked clustering model but not the
clustering model (021D, 021U, 030T, 120D, 120U). The idea is to grab the
data frame from the ranked cluster results, only keeping those rows for
certain triads.

``` r
tau_rankedcluster[[2]][rownames(tau_rankedcluster[[2]]) %in% 
                         c("triadcensus.021D", "triadcensus.021U","triadcensus.030T","triadcensus.120D",  "triadcensus.120U"), ]
```

    ##                  observed.triads expected.triads weight.vector
    ## triadcensus.021D               1    0.1144864249             1
    ## triadcensus.021U               0    0.1144864249             1
    ## triadcensus.030T               0    0.0005695842             1
    ## triadcensus.120D               0    0.0153787735             1
    ## triadcensus.120U               0    0.0153787735             1

In every case but 021D, the observed counts are basically the same as
that expected by the null model. In fact, we see 0 observed triads for
021U, 030T, 120D and 120U. This would suggest that the ranked clustering
model really isn’t offering much over the clustering model. The ranked
clustering model offers similar fit to the clustering model but is more
complicated, adding 5 triads that do not seem to deviate much from
chance expectations. We may then have good reason to interpret the
network in terms of the clustering model, where there are multiple
groups but few asymmetries.

Overall, the analysis shows that the social interaction network is best
characterized as a network with multiple groups without a clear
hierarchical arrangement. Given the very high levels of reciprocity in
social interactions, asymmetries are rare and do not consistently emerge
between groups. The tau statistic reinforces our story of the social
interaction network consisting of distinct social groups with one bridge
and no clear hierarchy. Compare this to the task network, which has a
clear hub and spoke structure, but no emergent groups.

We end the tutorial by noting that centrality and hierarchy will come up
again in a number of tutorials; for example, in Chapter 11 (duality),
Chapter 13 (statistical network models) and Chapter 14 (diffusion).
