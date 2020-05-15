Relational Event Models
================
Saurabh Khanna
2020-05-14

# Chapter 13: R Tutorial on Relational Event Models

Built with R version 3.6.3

This is the third tutorial for Chapter 13 on statistical network models.
The first tutorial covered the case of cross-sectional network data. The
second tutorial covered statistical models for discrete, longitudinal
networks. Here, we will walk through relational event models,
appropriate for continuous-time network data. Relational event models
are based on micro-interaction data. The model assumes that there is
time-stamped (or at least ordered) information on the interactions
between a set of actors. This shifts the focus from discrete
relationships (friend, advice, etc.) to the specific interactions
between actors in a setting. The goal of the model is to predict what
the next event is likely to be, based on the interactional tendencies,
or rules, of behavior in the setting. Compare this to STERGM, where the
goal is to predict the adding/dropping of ties from one period to the
next, based on discretely defined networks.

Our data for this tutorial are based on streaming interaction data
collected by Daniel McFarland on students in classrooms. Time-stamped
interactions in each classroom were recorded, with information on the
‘sender’ and ‘receiver’ of the interaction, as well as the nature of
the interaction. Interactions could be social or task-based, for
example. Data were collected across a large number of classrooms and
days. See also Chapter 3c (data processing for dynamic network data) and
Chapter 5 (on visualization). Here we consider one classroom on two
different days; both days are in the second semester of the year. We
pick two days as a means of comparison. The first day was relatively
uneventful and class was orderly. The second day was different, as there
was a much higher rate of sanctioning behavior (i.e., disagreements
between students and teacher on what was going on in the classroom). By
examining two days, we see if the interactional signatures of order and
disorder are different.

### 1 Getting the Data Ready

Let’s begin by loading the main packages and getting the data ready to
run the models.

``` r
library(relevent)
library(sna)
```

relevent contains the functions to run relational event models. We can
cite relevent as: Carter T. Butts (2008). A Relational Event Framework
for Social Action. Sociological Methodology, 38(1), 155–200.

Now, let’s read in the interactional data for the first date. This is a
data set reporting on the social interactions, i.e. talking, between
individuals in the classroom. This will serve as the outcome of
interest, as we will predict what interactional features make certain
events more likely to occur than
others.

``` r
interactions_date1=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_interactions_date1.txt", 
                              header=T)
```

Here we take a look at the first six rows of the data, for the main
variables of interest: send\_col (id of sender), receive\_col (id of
receiver), and time\_estimate\_col (time that interaction
occurred).

``` r
head(interactions_date1[,c("send_col", "receive_col", "time_estimate_col")])
```

    ##   send_col receive_col time_estimate_col
    ## 1       11           2             0.143
    ## 2        2          11             0.286
    ## 3        2           5             0.429
    ## 4        5           2             0.571
    ## 5        9           8             0.714
    ## 6        8           9             0.857

Each row corresponds to an interaction between sender and receiver. For
example, we can see that the third social interaction in this class
period involved node 2 talking to node 5.

We need to manipulate the data a bit to get it in a form that the
relational event model can use. For example, the events must be sorted
in sequential order before we run any models. In this case, this is
already done, but if it were not we would have to sort our data
appropriately. Additionally, each interaction must also happen at a
unique time period. The models are based on sequences of interactions,
so a distinct order of events must be possible to establish. This means
that relational event models are not so appropriate in cases where there
are a large number of simultaneous events. For the sake of simplicity,
we will remove all interactions directed from the teacher to all
students or from all students to the teacher.

We can use the to\_all\_col and from\_all\_col to exclude these cases.
to\_all\_col is equal to 1 if node i broadcasts to all other nodes
simultaneously, while from\_all\_col is equal to 1 if node j receives
from all nodes simultaneously. We will only keep those interactions
where those variables are equal to 0 (i.e., i is not broadcasting to
everyone in the
class).

``` r
interactions_date1=interactions_date1[interactions_date1$to_all_col==0 & interactions_date1$from_all_col==0,]
```

Now, in order to run the model, we need to create an edgelist (as a
matrix object), where the first column is the time of the event, the
second column is the sender and the third column is the receiver. Again,
the events must be ordered
sequentially.

``` r
edgelist_date1=as.matrix(interactions_date1[,c("time_estimate_col", "send_col", "receive_col")])
```

As a final data manipulation, we need to add a row to the end of the
edgelist, showing the stop time where no more interactions are possible.
Let’s look at the end of the data frame:

``` r
tail(edgelist_date1)
```

    ##     time_estimate_col send_col receive_col
    ## 274            42.528       11          17
    ## 275            42.623       17          11
    ## 276            42.717        3          11
    ## 277            42.811       11           3
    ## 278            42.906        7           8
    ## 279            43.000        8           7

We can see that the last social interaction occurred at minute 43 in the
class period. We will set the end of the interactional period at 43.10
(i.e., 6 seconds after the final interaction). To do this we add a row
to the end of the edgelist, with the end time and then two NA values
(for sender and receiver).

``` r
edgelist_date1=rbind(edgelist_date1, c(43.10, NA, NA))
```

Now, we will read in some attribute data, as we want to use information
on gender, grade, etc. as predictors in the
model.

``` r
attributes=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_attributes.txt", 
                      header=T)
head(attributes)
```

    ##   id gnd grd rce
    ## 1  1   2  10   4
    ## 2  2   2  10   3
    ## 3  3   2  10   3
    ## 4  4   2  10   3
    ## 5  5   2  10   3
    ## 6  6   1  10   4

There are four variables: id, gnd (gender: 1=male; 2=female); grd
(grade: 10=10; 11=11; 16=teacher); rce (race: 3=Black; 4=White).

The relevent package requires that a researcher construct the node-level
predictors as distinct columns (as opposed to using a factor command
within the formula). So, we will recode our variables to create the
desired dummy variables of interest. In this case, we will create a
predictor for whether the node is a teacher or not and for gender. We
also need to create a term for the intercept. We will utilize the recode
function in the car package.

``` r
library(car)
```

We first will create a variable for the intercept, which is a simple 1
for all nodes in the class.

``` r
attributes$intercept=1
```

Now, we create a variable called male that is a 0 if gnd is equal to 2
(female) and 1 if gnd is equal to 1 (male).

``` r
attributes$male=recode(attributes$gnd, as.factor=F, "c(1)=1; c(2)=0")
```

And here we do the same thing for grd, creating a binary variable called
teacher. teacher is equal to 1 if they are a teacher (grd=16) and 0
otherwise.

``` r
attributes$teacher=recode(attributes$grd, as.factor=F, "c(16)=1; NA=NA; else=0")
```

Finally, it will also be useful to have the size of the class handy. We
can calculate that as the number of rows in the attribute data frame.

``` r
class_size=nrow(attributes)
```

#### 1.2. Running Initial Models

Relational event modeling is based on the logic of hazard models (or
event history models), where the model predicts the risk of an event
occurring (i.e., the hazard) as a function of different kind of
interactional terms. There are a number of terms that we can include,
including terms for baseline node effects (e.g., girls interact more
than boys). We can also include terms that capture more micro-dynamics.
These are labeled p-shifts, or participation shifts, and are only based
on the previous event in the sequence. For example, if A talks to B,
then we might expect the very next event to be B talking to A. The model
allows us to include these different kinds of terms as a means of seeing
what rules govern the interactions in the case of interest.

We are now in a position to run an initial relational event model. The
function is rem.dyad. The main inputs are:

  - edgelist=input edgelist in the form of time of event, sender,
    receiver
  - n=size of network
  - effects=vector with names of effects to be included in the model
  - covar=list of covariates that must correspond to the terms specified
    in effects
  - ordinal=T/F; T if data are ordinal (ordered but without specific
    time stamps); F if data include time specific information for each
    event

Our first model will be very simple and just includes an intercept,
capturing the baseline rate for events to occur. We will set the effects
to CovSnd. CovSnd is a basic sender effect, in this case initiating
social interactions with others. We use the covar option to include
specific terms for CovSnd. In this case we include the intercept (so all
nodes are assumed to initiate interactions at the same rate). We set
ordinal to FALSE as the data has time stamped information. Let’s also
set a seed to make it easier to replicate.

``` r
set.seed(1000) 
mod1=rem.dyad(edgelist=edgelist_date1, n=class_size, effects=c("CovSnd"), 
                covar=list(CovSnd=attributes$intercept), ordinal=FALSE, hessian=TRUE)
```

And now we look at the results:

``` r
summary(mod1)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##           Estimate   Std.Err Z value  Pr(>|z|)    
    ## CovSnd.1 -4.057353  0.066213 -61.277 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 2306.34 on 228 degrees of freedom
    ##  Chi-square: -3.830135e-05 on 0 degrees of freedom, asymptotic p-value 1 
    ## AIC: 2308.34 AICC: 2308.357 BIC: 2311.769

The coefficient for the intercept (CovSnd.1) isn’t all that interesting
in itself, but it is important to understand what the coefficients mean
(and how we can manipulate them) before moving to more complicated
models. The first thing to note is that if we exponentiate this
coefficient, we get the hazard of any event (interaction between i-j)
occurring. Higher hazards mean the risk for an event occurring is
higher. Second, if we multiple the hazard rate by the number of possible
node pairs who could interact in a given moment, n\*(n-1), we should get
the expected number of interactions occurring per minute in the
classroom. Finally, if we take the inverse of that (1/number of
interactions per minute), we get the expected amount of time between
events, or the wait time between events.

``` r
1/(18*17*exp(-4.057353))
```

    ## [1] 0.1889577

The expected time between any event occurring is .189 standardized
minutes (or .189\*60=11.34 seconds). And let’s check this against the
real data. We will take the total number of minutes for that class and
divide that by the total number of interactions that occurred. We will
define the total number of minutes as 43.1, the end time set above.

``` r
total_classtime=43.1
```

We now define the total number of interactions. We will take the number
of rows in the edgelist and subtract 1, as the last row is the stop time
(not an interaction).

``` r
num_interations=nrow(edgelist_date1)-1 
time_between_events=total_classtime/num_interations
time_between_events
```

    ## [1] 0.1890351

We can see the estimate from the model approximates the raw data quite
well. We can do more substantively interesting things by incorporating
the attributes of the nodes into the model. Let’s first add a term for
gender (coded as male=1 and female=0). We will add a sender effect,
capturing whether males initiate fewer or greater interactions than
females, as well as a receiver effect, capturing whether males receive
fewer/greater interactions than females.

Here we create two matrices, one for sending and one for receiving. Each
matrix will include the covariates we want to include for the sending or
receiving effects. We start with the sender covariate matrix, where we
will include variables for the intercept (it still must be included) and
male.

``` r
CovSnd1=cbind(attributes[,c("intercept", "male")])
```

And now we do the same thing for the receiver covariate matrix,
including a variable for male (note that no intercept term is included
here). Note that even though we already had male as a sender effect we
need to include it separately as part of the receiver covariate matrix
if we want to specify it as a receiver effect.

``` r
CovRec1=cbind(attributes[,c("male")])
```

And now we are ready to estimate the model. The only difference from
before is that we include “CovRec” in the effects and CovRec1 as part of
the covariate (covar)
list.

``` r
mod2a=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec"), 
                covar=list(CovSnd=CovSnd1, CovRec=CovRec1), ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod2a)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##           Estimate   Std.Err  Z value  Pr(>|z|)    
    ## CovSnd.1 -3.760406  0.084699 -44.3973 < 2.2e-16 ***
    ## CovSnd.2 -0.601249  0.160226  -3.7525 0.0001751 ***
    ## CovRec.1 -0.460132  0.154017  -2.9875 0.0028123 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 2282.406 on 226 degrees of freedom
    ##  Chi-square: 23.93399 on 2 degrees of freedom, asymptotic p-value 6.350391e-06 
    ## AIC: 2288.406 AICC: 2288.513 BIC: 2298.694

We can see that the names of the variables are difficult to interpret.
So, let’s create a vector of more useful names, and put that on the
outputted object.

``` r
coef.names=c("intercept", "Sender.male", "Receiver.male")
```

And now we put those names on the coef part of the rem object and redo
the summary of the model.

``` r
names(mod2a$coef)=coef.names
summary(mod2a)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                Estimate   Std.Err  Z value  Pr(>|z|)    
    ## intercept     -3.760406  0.084699 -44.3973 < 2.2e-16 ***
    ## Sender.male   -0.601249  0.160226  -3.7525 0.0001751 ***
    ## Receiver.male -0.460132  0.154017  -2.9875 0.0028123 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 2282.406 on 226 degrees of freedom
    ##  Chi-square: 23.93399 on 2 degrees of freedom, asymptotic p-value 6.350391e-06 
    ## AIC: 2288.406 AICC: 2288.513 BIC: 2298.694

The summarized output now has more easily interpretable names for the
variables. Let’s go ahead and interpret the coefficient on sender.male.
We can start by interpreting the sender.male coefficient in terms of
hazards. By exponentiating the coefficient, we get the relative hazard
for males to initiate the next interaction compared to females.
exp(-.601)=.548. This means that an event with males initiating has a
hazard that is .584 times lower than an event with females initiating.

Hazards themselves are bit hard to interpret. As an alternative, we can
calculate mean wait times, or the expected time between events. Let’s
first calculate the expected time between two male-male events (where
there is a male sender and a male receiver). Note that this calculation
must incorporate all of the coefficients (intercept, Sender.male and
Receiver.male). The calculation is the same as we did above with the
intercept only model, where the expected wait is equal to:
1/(dyads\_at\_risk\*hazard), where dyads\_at\_risk is the number of
different ways that the event could occur, in this case the number of
possible events that could involve two males.

Let’s do a quick table to see how many males are in the classroom.

``` r
table(attributes$male)
```

    ## 
    ##  0  1 
    ## 12  6

We can see that there are 12 females and 6 males. This means that there
are 6\*5 different ways that we could have a boy as a sender and a boy
as a receiver. We will use that in the calculation below:

``` r
dyads_at_risk=6*5
```

Now we calculate the hazard. We take the coefficients, multiply them by
the vector of input values (here setting Sender.male and Receiver.male
to 1), sum it up and then exponentiate it.

``` r
inputs=c(intercept=1, Sender.male=1, Receiver.male=1)
hazard_male_male=exp(sum(mod2a$coef*inputs))
hazard_male_male
```

    ## [1] 0.008052391

And now to calculate wait time:

``` r
1/(dyads_at_risk*hazard_male_male)
```

    ## [1] 4.139557

This means that we would expect to wait 4.14 minutes between events that
involve two boys. Now, let’s do the same thing for girl-girl
interactions. Here we set Sender.male to 0 and Receiver.male to 0.

``` r
inputs=c(intercept=1, Sender.male=0, Receiver.male=0)
```

We define the dyads at risk to be 12\*11 as there are 12 females in the
class.

``` r
dyads_at_risk=12*11
```

Here we calculate the hazard.

``` r
hazard_female_female=exp(sum(mod2a$coef*inputs))
hazard_female_female
```

    ## [1] 0.02327429

We can see that the hazard for female-female events is higher than
male-male events. And now for the wait time.

``` r
1/(dyads_at_risk*hazard_female_female)
```

    ## [1] 0.325499

We can see also that the wait time between female-female events is much
lower. This is the case both because there are more females in the class
and because males have a lower hazard of taking part in social
interactions.

Now, let’s add our teacher variable to the model. This is accomplished
by creating new CovSnd and CovRec matrices that include the teacher
variable.

``` r
CovSnd2=cbind(attributes[,c("intercept", "male", "teacher")])
CovRec2=cbind(attributes[,c("male", "teacher")])
```

And now we rerun our model with the updated CovSnd and CovRec
matrices.

``` r
mod2b=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec"), 
                covar=list(CovSnd=CovSnd2, CovRec=CovRec2), ordinal=FALSE, hessian=TRUE)
```

Again, we can add better labels to the variable names and summarize the
results.

``` r
coef.names=c("intercept", "Sender.male", "Sender.teacher", "Receiver.male", "Receiver.teacher")
names(mod2b$coef)=coef.names
summary(mod2b)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## intercept        -3.759707  0.084668 -44.4054 < 2.2e-16 ***
    ## Sender.male      -0.483081  0.164160  -2.9427  0.003253 ** 
    ## Sender.teacher   -1.132818  0.596658  -1.8986  0.057617 .  
    ## Receiver.male    -0.514723  0.169125  -3.0434  0.002339 ** 
    ## Receiver.teacher  0.280787  0.325987   0.8613  0.389048    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 2276.504 on 224 degrees of freedom
    ##  Chi-square: 29.83545 on 4 degrees of freedom, asymptotic p-value 5.286827e-06 
    ## AIC: 2286.504 AICC: 2286.775 BIC: 2303.651

It looks like the teacher variables do not add much to the model. Let’s
compare the fit using BIC across the two models.

``` r
mod2a$BIC - mod2b$BIC
```

    ## [1] -4.957228

Given that we want lower values, the simple model (mod2a, which just
includes gender) would appear to be the better option.

As a third kind of term, we will consider adding co-events to the model.
Co-events are predictors that are based on attributes of a dyad. Here,
we will add the seating structure of the class to the model. The basic
idea is that nodes who are close in the classroom are more likely to
talk to one another. Let’s first read in the
data:

``` r
seating_date1=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_seating_date1.txt", 
                         header=T)
head(seating_date1)
```

    ##   ego_id alter_id
    ## 1      1        7
    ## 2      1        9
    ## 3      1       15
    ## 4      2        3
    ## 5      2        5
    ## 6      2       11

Here we set the size of the network on the edgelist:

``` r
attr(seating_date1,"n")=class_size
```

As in the previous tutorial, the data is stored as an edgelist,
indicating if node i is sitting adjacent to node j. The rem.dyad
function requires that this information be transformed into a matrix.
So, we will go ahead and create a matrix of seating, where there is a 1
if i and j are next to each other in the class and 0 otherwise. We will
accomplish this by taking the seating edgelist, turning it into a
network object and then extracting the matrix from the network object.

``` r
seating_network_date1=network(x=seating_date1, directed=F) 
seating_matrix_date1=as.matrix(seating_network_date1)
seating_matrix_date1
```

    ##    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
    ## 1  0 0 0 0 0 0 1 0 1  0  0  0  0  0  1  0  0  0
    ## 2  0 0 1 0 1 0 0 0 0  0  1  0  0  0  0  0  1  0
    ## 3  0 1 0 0 1 0 0 0 0  0  1  1  0  0  0  0  1  0
    ## 4  0 0 0 0 0 1 0 1 0  0  0  0  0  1  0  0  0  1
    ## 5  0 1 1 0 0 0 0 0 0  0  1  1  0  0  0  0  1  0
    ## 6  0 0 0 1 0 0 0 1 0  0  0  1  0  1  0  0  0  1
    ## 7  1 0 0 0 0 0 0 0 1  0  0  0  0  0  1  0  0  0
    ## 8  0 0 0 1 0 1 0 0 0  0  0  0  0  1  0  0  0  1
    ## 9  1 0 0 0 0 0 1 0 0  0  0  0  0  0  1  0  0  0
    ## 10 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 11 0 1 1 0 1 0 0 0 0  0  0  0  0  0  0  0  1  0
    ## 12 0 0 1 0 1 1 0 0 0  0  0  0  0  0  0  1  0  0
    ## 13 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 14 0 0 0 1 0 1 0 1 0  0  0  0  0  0  0  0  0  1
    ## 15 1 0 0 0 0 0 1 0 1  0  0  0  0  0  0  0  0  0
    ## 16 0 0 0 0 0 0 0 0 0  0  0  1  0  0  0  0  0  0
    ## 17 0 1 1 0 1 0 0 0 0  0  1  0  0  0  0  0  0  0
    ## 18 0 0 0 1 0 1 0 1 0  0  0  0  0  1  0  0  0  0

Now, we can run our model. We must include “CovEvent” in the effects
input. We must also add the seating matrix to the list of covariates
(covar), set with CovEvent. We will use the CovSnd and CovRec matrices
that only include gender (so no teacher
variable).

``` r
mod3a=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec", "CovEvent"), 
                covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=seating_matrix_date1), 
                ordinal=FALSE, hessian=TRUE)
```

Let’s compare the fit between our previously preferred model and our new
model.

``` r
mod2a$BIC - mod3a$BIC
```

    ## [1] 416.5262

It looks like the seating arrangement does strongly shape what events
occur in the classroom, as the fit is dramatically improved. Now, let’s
go ahead and add a second co-event matrix to the model. Here, we add
information about the friendships that exist in the classroom.
Friendship information was collected for each semester. Students were
asked who they hung around with in the class. We will treat this
information like a co-event, with the idea that interactions during the
class period are more likely to involve friends than non-friends. We
will first read in the data for friendship during the second semester
(when the class of interest took
place).

``` r
friends_sem2=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_edgelist_sem2.txt", 
                        header=T)
head(friends_sem2)
```

    ##   sender receiver
    ## 1      1        4
    ## 2      1        5
    ## 3      1        7
    ## 4      1        9
    ## 5      1       15
    ## 6      2        3

The edgelist captures if student i nominated student j as a friend. Note
that the ids must match that found on the other data (interaction data,
attributes, etc.). Here we set the size of the network on the edgelist:

``` r
attr(friends_sem2, "n")=class_size
```

As before we need to need to turn our edgelist into a matrix of 0s and
1s.

``` r
friends_sem2_network=network(x=friends_sem2, directed=T) 
friends_matrix_sem2=as.matrix(friends_sem2_network)
friends_matrix_sem2
```

    ##    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
    ## 1  0 0 0 1 1 0 1 0 1  0  0  0  0  0  1  0  0  0
    ## 2  0 0 1 0 1 0 0 0 0  0  1  0  0  0  0  0  1  0
    ## 3  0 1 0 0 1 1 0 0 0  0  1  0  0  0  0  0  1  0
    ## 4  0 1 0 0 0 1 0 0 0  0  0  0  0  1  0  0  0  1
    ## 5  1 0 1 0 0 0 0 0 0  0  0  0  0  0  0  0  1  1
    ## 6  0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 7  1 1 0 0 0 1 0 1 1  0  0  0  0  0  1  0  0  1
    ## 8  0 0 0 1 0 1 0 0 0  0  0  0  0  1  0  0  0  1
    ## 9  1 0 0 0 0 1 1 1 0  0  0  0  0  0  1  0  1  0
    ## 10 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 11 0 1 1 0 1 0 0 0 0  0  0  0  0  0  0  0  1  0
    ## 12 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 13 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 14 1 0 0 1 0 1 0 1 0  0  0  0  0  0  0  0  0  1
    ## 15 1 0 0 0 0 1 1 1 1  0  0  0  0  0  0  0  0  0
    ## 16 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0
    ## 17 0 1 1 1 1 0 0 0 1  0  1  0  0  0  0  0  0  0
    ## 18 0 0 0 1 0 1 0 0 0  0  0  0  0  1  0  0  0  0

While it is relatively simple to include a single co-event matrix in the
model (see seating example above), it is a bit tricky to include
multiple co-event matrices. The rem.dyad function requires that multiple
matrices first be put together as an p X n X n array, where p is the
number of matrices and n is the size of the network.

We will go ahead and create that array. We will first create an array of
NAs with the right structure (2 X 18 X 18).

``` r
CovEvent_date1=array(data=NA, dim=c(2, class_size, class_size))
```

We will now put the first matrix, the seating matrix, in the first slot.

``` r
CovEvent_date1[1,,]=seating_matrix_date1
```

We will now put the second matrix, the friendship matrix, in the second
slot.

``` r
CovEvent_date1[2,,]=friends_matrix_sem2
```

Checking the dimensions:

``` r
dim(CovEvent_date1)
```

    ## [1]  2 18 18

Note, that we would get unexpected (i.e., wrong) results if we had
created the array to be 18 X 18 X 2. We are now in a position to run the
model, putting in the newly created CovEvent array as input into the
covar
list.

``` r
mod3b=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec", "CovEvent"),
                covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=CovEvent_date1), 
                ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod3b)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##            Estimate  Std.Err  Z value  Pr(>|z|)    
    ## CovSnd.1   -5.49675  0.17648 -31.1470 < 2.2e-16 ***
    ## CovSnd.2   -0.47785  0.16200  -2.9498  0.003180 ** 
    ## CovRec.1   -0.43877  0.15428  -2.8440  0.004455 ** 
    ## CovEvent.1  1.82666  0.23713   7.7031 1.332e-14 ***
    ## CovEvent.2  1.52491  0.23856   6.3921 1.636e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1812.664 on 224 degrees of freedom
    ##  Chi-square: 493.6753 on 4 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1822.664 AICC: 1822.935 BIC: 1839.811

Here we look at the results after adding meaningful names to the
coefficients:

``` r
names(mod3b$coef)=c("Intercept", "Sender.male", "Receiver.male", "Adjacent Seating", "Friendship")
summary(mod3b)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                  Estimate  Std.Err  Z value  Pr(>|z|)    
    ## Intercept        -5.49675  0.17648 -31.1470 < 2.2e-16 ***
    ## Sender.male      -0.47785  0.16200  -2.9498  0.003180 ** 
    ## Receiver.male    -0.43877  0.15428  -2.8440  0.004455 ** 
    ## Adjacent Seating  1.82666  0.23713   7.7031 1.332e-14 ***
    ## Friendship        1.52491  0.23856   6.3921 1.636e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1812.664 on 224 degrees of freedom
    ##  Chi-square: 493.6753 on 4 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1822.664 AICC: 1822.935 BIC: 1839.811

We can see that both friendship and adjacent seating predict the
occurrence of a social interaction event between i and j. Thus, if i and
j sit next to each other and/or are friends they are much more likely to
have an interaction event. Note that the gender differences in sending
and receiving still seem to be present.

#### 1.3. Micro Rules of Interaction

So far we have built up a simple but plausible model of interactions in
this classroom. Girls tend to talk more than boys, while friends and
those sitting close to each other also tend to interact during class.
What we have yet to capture is something about the ‘rules’ of
interaction. For example, we might expect turn taking (i talks to j and
then j talks to i) above what we can capture from friendship and seating
effects alone.

We will build up our model slowly, including more complicated rules as
we go along. As a start, let’s include terms that capture recency of
events. The two terms of interest are “RRecSnd” and “RSndSnd”. With
RRecSnd, we test if i is more likely to talk to j if j recently talked
to i. With RSndSnd, we test if i is more likely to talk to j if i
recently talked to j. The effects capture the idea that if j recently
talked to i (or i recently talked to j), then an i-\>j event is more
likely to be the next event. We will specify this model by including the
“RRecSnd” and “RSndSnd” in the vector of effects. Note that we do not
need to add anything to the covar list. For this first model we will not
control for friendship or
seating.

``` r
mod4a=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec", "RRecSnd", "RSndSnd"),
               covar=list(CovSnd=CovSnd1, CovRec=CovRec1), ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod4a)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##          Estimate  Std.Err  Z value  Pr(>|z|)    
    ## RRecSnd   6.31792  0.22376  28.2355 < 2.2e-16 ***
    ## RSndSnd  -2.44296  0.21764 -11.2250 < 2.2e-16 ***
    ## CovSnd.1 -5.14311  0.14179 -36.2717 < 2.2e-16 ***
    ## CovSnd.2 -0.82672  0.17750  -4.6575   3.2e-06 ***
    ## CovRec.1 -0.47810  0.16423  -2.9112  0.003601 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1419.167 on 224 degrees of freedom
    ##  Chi-square: 887.1722 on 4 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1429.167 AICC: 1429.438 BIC: 1446.314

Adding meaningful names to the
coefficients:

``` r
names(mod4a$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male", "Receiver.male")
summary(mod4a)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##               Estimate  Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji     6.31792  0.22376  28.2355 < 2.2e-16 ***
    ## Recency.ij    -2.44296  0.21764 -11.2250 < 2.2e-16 ***
    ## Intercept     -5.14311  0.14179 -36.2717 < 2.2e-16 ***
    ## Sender.male   -0.82672  0.17750  -4.6575   3.2e-06 ***
    ## Receiver.male -0.47810  0.16423  -2.9112  0.003601 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1419.167 on 224 degrees of freedom
    ##  Chi-square: 887.1722 on 4 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1429.167 AICC: 1429.438 BIC: 1446.314

We see a positive coefficient on the recency receiver effects,
suggesting that if j recently talked to i then i is likely to talk to j.
On the other hand, there is a negative coefficient for the recency
sender effects. This means that if i recently talked to j the next event
is actually less likely to be i to j again. Note that the gender effects
remain. Now we run the full model with seating and friendship
included.

``` r
mod4b=rem.dyad(edgelist_date1, n=class_size, effects=c("CovSnd", "CovRec", "CovEvent","RRecSnd", "RSndSnd"), 
               covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=CovEvent_date1), 
               ordinal=FALSE, hessian=TRUE)
```

``` r
names(mod4b$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male",
                    "Receiver.male", "Adjacent Seating", "Friendship")

summary(mod4b)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        5.726043  0.223170  25.6577 < 2.2e-16 ***
    ## Recency.ij       -3.051556  0.224247 -13.6080 < 2.2e-16 ***
    ## Intercept        -6.070958  0.218092 -27.8367 < 2.2e-16 ***
    ## Sender.male      -0.201135  0.175123  -1.1485 0.2507489    
    ## Receiver.male    -0.089231  0.164541  -0.5423 0.5876111    
    ## Adjacent Seating  1.060937  0.278799   3.8054 0.0001416 ***
    ## Friendship        0.919033  0.272150   3.3769 0.0007330 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1348.516 on 222 degrees of freedom
    ##  Chi-square: 957.8238 on 6 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1362.516 AICC: 1363.025 BIC: 1386.521

The first thing to note is that the model fit is much improved from our
previously preferred model.

``` r
mod3b$BIC - mod4b$BIC
```

    ## [1] 453.2898

The second thing to note is that the gender coefficients are no longer
significant. This suggests that controlling for both recency of events
and seating explains the gender differences in social interaction. Girls
are more likely to sit next to each other; couple this with the
interactional tendencies to respond to a recent interaction, and we see
why girls are more likely to be involved in events.

Now, let’s see if we can consider other rules that may be shaping how
nodes in this classroom interact with each other. We will now consider
terms that capture p-shifts, or participation shifts. p-shifts are based
strictly on the most recent event that occurred, rather than recency
effects which can go back further in time. The idea is to capture micro
rules in how interactions play out, based on the last interaction that
took place. For our first example, we will add a turn taking rule, where
A talks to B and the very next event is B talking to A. This is
specified as PSAB-BA (as part of the effects vector). The rest of the
model is the same as the previous model.

``` r
mod4c=rem.dyad(edgelist_date1, n=class_size, 
               effects=c("CovSnd", "CovRec", "CovEvent", "RRecSnd", "RSndSnd", "PSAB-BA"), 
               covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=CovEvent_date1),
               ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod4c)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##             Estimate   Std.Err  Z value  Pr(>|z|)    
    ## RRecSnd     3.140606  0.300287  10.4587 < 2.2e-16 ***
    ## RSndSnd    -1.447230  0.209216  -6.9174   4.6e-12 ***
    ## CovSnd.1   -6.023163  0.207242 -29.0635 < 2.2e-16 ***
    ## CovSnd.2    0.003476  0.168344   0.0206  0.983526    
    ## CovRec.1    0.135468  0.162346   0.8344  0.404031    
    ## CovEvent.1  0.960246  0.252733   3.7995  0.000145 ***
    ## CovEvent.2  0.892416  0.249509   3.5767  0.000348 ***
    ## PSAB-BA     3.334932  0.187187  17.8161 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1006.862 on 221 degrees of freedom
    ##  Chi-square: 1299.478 on 7 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1022.862 AICC: 1023.519 BIC: 1050.296

``` r
names(mod4c$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male", 
                    "Receiver.male", "Adjacent Seating", "Friendship", "PSAB-BA")
summary(mod4c)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        3.140606  0.300287  10.4587 < 2.2e-16 ***
    ## Recency.ij       -1.447230  0.209216  -6.9174   4.6e-12 ***
    ## Intercept        -6.023163  0.207242 -29.0635 < 2.2e-16 ***
    ## Sender.male       0.003476  0.168344   0.0206  0.983526    
    ## Receiver.male     0.135468  0.162346   0.8344  0.404031    
    ## Adjacent Seating  0.960246  0.252733   3.7995  0.000145 ***
    ## Friendship        0.892416  0.249509   3.5767  0.000348 ***
    ## PSAB-BA           3.334932  0.187187  17.8161 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 1006.862 on 221 degrees of freedom
    ##  Chi-square: 1299.478 on 7 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1022.862 AICC: 1023.519 BIC: 1050.296

We can see that the fit is improved greatly, as there is a large effect
of turn taking in social interactions. We can also see that the effects
for recency are much reduced from the previous model. Now, let’s add a
somewhat more complicated interactional rule. Here we will add a term
for ‘turn continuing’, PSAB-AY. This means that A talks with B and the
very next event is A talking to someone else (besides B). We will also
add a term for ‘turn receiving’, PSAB-BY. Here, A talks to B and the
very next event is B talking to someone else (besides A).

``` r
mod4d=rem.dyad(edgelist_date1, n=class_size, 
               effects=c("CovSnd", "CovRec", "CovEvent", "RRecSnd", "RSndSnd", "PSAB-BA", "PSAB-BY", "PSAB-AY"), 
               covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=CovEvent_date1),
               ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod4d)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##             Estimate   Std.Err  Z value  Pr(>|z|)    
    ## RRecSnd     3.277777  0.306179  10.7054 < 2.2e-16 ***
    ## RSndSnd    -1.445961  0.209454  -6.9035 5.075e-12 ***
    ## CovSnd.1   -6.166486  0.218192 -28.2618 < 2.2e-16 ***
    ## CovSnd.2    0.026808  0.168883   0.1587 0.8738774    
    ## CovRec.1    0.148637  0.162911   0.9124 0.3615667    
    ## CovEvent.1  0.960884  0.253534   3.7900 0.0001507 ***
    ## CovEvent.2  0.858668  0.249639   3.4396 0.0005825 ***
    ## PSAB-BA     3.364802  0.190062  17.7037 < 2.2e-16 ***
    ## PSAB-BY     1.180495  0.305239   3.8674 0.0001100 ***
    ## PSAB-AY     0.098791  0.423885   0.2331 0.8157134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 995.3731 on 219 degrees of freedom
    ##  Chi-square: 1310.967 on 9 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1015.373 AICC: 1016.387 BIC: 1049.667

``` r
names(mod4d$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male",
                    "Receiver.male", "Adjacent Seating", "Friendship", "PSAB-BA", "PSAB-BY", "PSAB-AY")

summary(mod4d)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        3.277777  0.306179  10.7054 < 2.2e-16 ***
    ## Recency.ij       -1.445961  0.209454  -6.9035 5.075e-12 ***
    ## Intercept        -6.166486  0.218192 -28.2618 < 2.2e-16 ***
    ## Sender.male       0.026808  0.168883   0.1587 0.8738774    
    ## Receiver.male     0.148637  0.162911   0.9124 0.3615667    
    ## Adjacent Seating  0.960884  0.253534   3.7900 0.0001507 ***
    ## Friendship        0.858668  0.249639   3.4396 0.0005825 ***
    ## PSAB-BA           3.364802  0.190062  17.7037 < 2.2e-16 ***
    ## PSAB-BY           1.180495  0.305239   3.8674 0.0001100 ***
    ## PSAB-AY           0.098791  0.423885   0.2331 0.8157134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 995.3731 on 219 degrees of freedom
    ##  Chi-square: 1310.967 on 9 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1015.373 AICC: 1016.387 BIC: 1049.667

Looking at our two added terms, we see that only the PSAB-BY coefficient
is significant (at traditional levels). This suggest that there are
norms about whose ‘turn it is’ to talk. Once A talks to B, it is now B’s
turn to talk. They are very likely to talk back to A (PSAB-BA) but may
also talk to another node (PSAB-BY). There is little evidence for nodes
talking twice in a row to different people. It looks like the model fit
is improved very slightly from the previous model:

``` r
mod4c$BIC - mod4d$BIC
```

    ## [1] 0.6297514

Let’s look at one more set of terms, here focusing on interactional
tendencies related to usurping the conversation. We include p-shift
terms for PSAB-XA and PSAB-XB. With PSAB-XA, A talks to B and then
another node (X) usurps the conversation and answers A. With PSAB-XB, A
talks to B and then another node (X) usurps the conversation and talks
to B. In both cases, a node talks ‘out of turn’ relative to the previous
event. Let’s go ahead and add these terms to the model.

``` r
mod4e=rem.dyad(edgelist_date1, n=class_size,
                effects=c("CovSnd", "CovRec", "CovEvent", "RRecSnd", "RSndSnd",
                          "PSAB-BA", "PSAB-BY","PSAB-AY", "PSAB-XA", "PSAB-XB"), 
                covar=list(CovSnd=CovSnd1, CovRec=CovRec1, CovEvent=CovEvent_date1),
                ordinal=FALSE, hessian=TRUE)
```

``` r
summary(mod4e)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##             Estimate   Std.Err  Z value  Pr(>|z|)    
    ## RRecSnd     3.255338  0.305936  10.6406 < 2.2e-16 ***
    ## RSndSnd    -1.435095  0.208876  -6.8706 6.394e-12 ***
    ## CovSnd.1   -6.345151  0.229722 -27.6210 < 2.2e-16 ***
    ## CovSnd.2    0.052905  0.169351   0.3124 0.7547372    
    ## CovRec.1    0.189967  0.163464   1.1621 0.2451803    
    ## CovEvent.1  0.965638  0.252718   3.8210 0.0001329 ***
    ## CovEvent.2  0.869857  0.248674   3.4980 0.0004688 ***
    ## PSAB-BA     3.532669  0.200569  17.6133 < 2.2e-16 ***
    ## PSAB-BY     1.332495  0.311164   4.2823 1.850e-05 ***
    ## PSAB-XA     0.389228  0.397964   0.9781 0.3280495    
    ## PSAB-XB     1.179549  0.286743   4.1136 3.895e-05 ***
    ## PSAB-AY     0.256822  0.428279   0.5997 0.5487330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 981.8918 on 217 degrees of freedom
    ##  Chi-square: 1324.448 on 11 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1005.892 AICC: 1007.343 BIC: 1047.044

``` r
names(mod4e$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male",
                    "Receiver.male", "Adjacent Seating", "Friendship",
                    "PSAB-BA", "PSAB-BY", "PSAB-XA", "PSAB-XB", "PSAB-AY")

summary(mod4e)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        3.255338  0.305936  10.6406 < 2.2e-16 ***
    ## Recency.ij       -1.435095  0.208876  -6.8706 6.394e-12 ***
    ## Intercept        -6.345151  0.229722 -27.6210 < 2.2e-16 ***
    ## Sender.male       0.052905  0.169351   0.3124 0.7547372    
    ## Receiver.male     0.189967  0.163464   1.1621 0.2451803    
    ## Adjacent Seating  0.965638  0.252718   3.8210 0.0001329 ***
    ## Friendship        0.869857  0.248674   3.4980 0.0004688 ***
    ## PSAB-BA           3.532669  0.200569  17.6133 < 2.2e-16 ***
    ## PSAB-BY           1.332495  0.311164   4.2823 1.850e-05 ***
    ## PSAB-XA           0.389228  0.397964   0.9781 0.3280495    
    ## PSAB-XB           1.179549  0.286743   4.1136 3.895e-05 ***
    ## PSAB-AY           0.256822  0.428279   0.5997 0.5487330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 981.8918 on 217 degrees of freedom
    ##  Chi-square: 1324.448 on 11 degrees of freedom, asymptotic p-value 0 
    ## AIC: 1005.892 AICC: 1007.343 BIC: 1047.044

We can see that the PSAB-XB effect is much stronger than the PSAB-XA
effect. This suggests that if one usurps the conversation from A to B,
then one must interact with B in the next interaction, in essence giving
B their rightful turn coming up next. And again, it looks like the model
fit is improved a bit from the previous model.

``` r
mod4d$BIC - mod4e$BIC
```

    ## [1] 2.622676

So, the overall story is one where there are clear micro rules to
interacting in a classroom (above the effects for friendship, seating
and so on). The basic rules could be summarized as: if A talks to B then
the next event should be B to A; B to someone else; or someone else to
B. Thus, there are clear norms about turn taking. If A talks to B, B is
very likely to be part of the next event (one way or another). Of
course, we could imagine looking at other kinds of terms, but this is
pretty good start to the model.

#### 1.4. Assessing Model Fit

In interpreting our results, it is useful to see if our models are
fitting well. While BIC can offer some evidence if one model is
preferred to another, we can look at the residuals and the predicted
classification to see how well the model is actually predicting the
data. Here we will work with mod4e, our preferred model from above. One
useful part of the output is predicted.match.

``` r
head(mod4e$predicted.match)
```

    ##   send_col receive_col
    ## 1    FALSE       FALSE
    ## 2     TRUE        TRUE
    ## 3    FALSE       FALSE
    ## 4     TRUE        TRUE
    ## 5    FALSE       FALSE
    ## 6     TRUE        TRUE

Each row corresponds to an observed event. The first column shows if the
model predicted the sender of that event correctly and the second column
shows if the model predicted the receiver of that event correctly. Note
that the model is trying to predict the specific sequence of events
(i.e., the exact order of sender-receiver events). Let’s see how we did
by doing a table of the send and receive
columns.

``` r
table(mod4e$predicted.match[,"send_col"], mod4e$predicted.match[,"receive_col"])
```

    ##        
    ##         FALSE TRUE
    ##   FALSE    89    6
    ##   TRUE     14  119

We can see that 119 times we predicted the exact sender and the exact
receiver correct (in sequence), while 89 times we got neither the sender
nor the receiver correct. And now let’s transform the table into
proportions, showing the proportion where we get the exact sequence
right (wrong,
etc.).

``` r
prop.table(table(mod4e$predicted.match[,"send_col"], mod4e$predicted.match[,"receive_col"]))
```

    ##        
    ##              FALSE       TRUE
    ##   FALSE 0.39035088 0.02631579
    ##   TRUE  0.06140351 0.52192982

We can see that about 52% of the time we get the exact event correct,
while 39% of the time we miss completely and do not get the sender or
receiver. The model is thus doing an okay job of prediction but is
clearly missing some element that is important for predicting
interaction events. Let’s see if we can identify the cases (i.e.,
events) where the model is not doing such a good job at prediction. We
first summarize the residuals for the model.

``` r
summary(mod4e$residuals)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.2790 -0.8395  1.2354  4.3030  8.7932 25.5613

Now, we will identify some of the outlying cases, those with high
residuals. We will define that for convenience as cases with residuals
greater than 10.

``` r
high_resids=which(mod4e$residuals>10)
```

Here we take a look at the events with high residuals, reducing the
edgelist to just those cases where the model is not fitting well.

``` r
edgelist_date1[high_resids,]
```

    ##     time_estimate_col send_col receive_col
    ## 5               0.714        9           8
    ## 9               1.429        3          11
    ## 21              3.286        8           7
    ## 25              4.286        5           2
    ## 37              6.321        3           2
    ## 45              7.393       18           4
    ## 55              8.893       11           3
    ## 57             10.041       14          18
    ## 59             10.419        5           2
    ## 71             12.595        5          17
    ## 79             13.446        9           8
    ## 101            16.391        3          12
    ## 104            16.913       11          12
    ## 106            17.826        3          12
    ## 107            18.609        2          17
    ## 111            19.789       15           8
    ## 113            21.105       11          12
    ## 119            22.289       11           2
    ## 121            22.947        4          14
    ## 127            24.500        8          12
    ## 130            25.125        8          12
    ## 154            26.970       11          12
    ## 155            27.091        2          12
    ## 156            27.333       11          12
    ## 157            27.455        2          12
    ## 160            28.909       11          12
    ## 165            29.758        8          12
    ## 184            33.152       11          17
    ## 188            34.640        7           1
    ## 200            37.520       11          17
    ## 204            38.566        5           2
    ## 210            39.132        8          15
    ## 212            39.321        1           8
    ## 216            39.698        8          15
    ## 235            40.264        7           8
    ## 260            41.208        8          15
    ## 278            42.906        7           8

Node 12 seems to show up quite a bit in the receiver column of these
events. Let’s take a look at the attributes for node 12.

``` r
attributes[12, ]
```

    ##    id gnd grd rce intercept male teacher
    ## 12 12   1  16   3         1    1       1

Node 12 is the teacher in the class. So, perhaps we were too hasty in
removing the teacher variable, as it looks like we are missing the set
of interactions where students talk socially to the teacher. Let’s rerun
our model but use CovSnd2 and CovRec2, which includes both the gender
and teacher effects.

``` r
mod4f=rem.dyad(edgelist_date1, n=class_size,
                effects=c("CovSnd", "CovRec", "CovEvent", "RRecSnd", "RSndSnd",
                          "PSAB-BA", "PSAB-BY","PSAB-AY", "PSAB-XA", "PSAB-XB"), 
                covar=list(CovSnd=CovSnd2, CovRec=CovRec2, CovEvent=CovEvent_date1),
                ordinal=FALSE, hessian=TRUE)
```

Let’s check the fit compared to the previous model (with no teacher
sender/receiver effects).

``` r
summary(mod4f)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##             Estimate   Std.Err  Z value  Pr(>|z|)    
    ## RRecSnd     3.505626  0.324672  10.7974 < 2.2e-16 ***
    ## RSndSnd    -1.576378  0.210817  -7.4775 7.572e-14 ***
    ## CovSnd.1   -6.447708  0.242992 -26.5347 < 2.2e-16 ***
    ## CovSnd.2    0.214852  0.171483   1.2529 0.2102408    
    ## CovSnd.3   -1.149644  0.635693  -1.8085 0.0705304 .  
    ## CovRec.1    0.015545  0.179621   0.0865 0.9310361    
    ## CovRec.2    1.431011  0.381853   3.7475 0.0001786 ***
    ## CovEvent.1  0.858065  0.256411   3.3464 0.0008185 ***
    ## CovEvent.2  0.992665  0.292979   3.3882 0.0007036 ***
    ## PSAB-BA     3.478414  0.202098  17.2115 < 2.2e-16 ***
    ## PSAB-BY     1.362151  0.311813   4.3685 1.251e-05 ***
    ## PSAB-XA     0.400182  0.398088   1.0053 0.3147715    
    ## PSAB-XB     1.151251  0.286728   4.0151 5.941e-05 ***
    ## PSAB-AY     0.231130  0.428191   0.5398 0.5893469    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 962.8608 on 215 degrees of freedom
    ##  Chi-square: 1343.479 on 13 degrees of freedom, asymptotic p-value 0 
    ## AIC: 990.8608 AICC: 992.8327 BIC: 1038.872

``` r
mod4e$BIC - mod4f$BIC
```

    ## [1] 8.172225

It looks like adding the teacher terms did help the fit a bit. And now
let’s look at the
results.

``` r
names(mod4f$coef)=c("Recency.ji", "Recency.ij", "Intercept", "Sender.male", "Sender.teacher", 
                    "Receiver.male", "Receiver.teacher", "Adjacent Seating", "Friendship", 
                    "PSAB-BA", "PSAB-BY", "PSAB-XA", "PSAB-XB", "PSAB-AY")

summary(mod4f)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        3.505626  0.324672  10.7974 < 2.2e-16 ***
    ## Recency.ij       -1.576378  0.210817  -7.4775 7.572e-14 ***
    ## Intercept        -6.447708  0.242992 -26.5347 < 2.2e-16 ***
    ## Sender.male       0.214852  0.171483   1.2529 0.2102408    
    ## Sender.teacher   -1.149644  0.635693  -1.8085 0.0705304 .  
    ## Receiver.male     0.015545  0.179621   0.0865 0.9310361    
    ## Receiver.teacher  1.431011  0.381853   3.7475 0.0001786 ***
    ## Adjacent Seating  0.858065  0.256411   3.3464 0.0008185 ***
    ## Friendship        0.992665  0.292979   3.3882 0.0007036 ***
    ## PSAB-BA           3.478414  0.202098  17.2115 < 2.2e-16 ***
    ## PSAB-BY           1.362151  0.311813   4.3685 1.251e-05 ***
    ## PSAB-XA           0.400182  0.398088   1.0053 0.3147715    
    ## PSAB-XB           1.151251  0.286728   4.0151 5.941e-05 ***
    ## PSAB-AY           0.231130  0.428191   0.5398 0.5893469    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2306.34 on 228 degrees of freedom
    ## Residual deviance: 962.8608 on 215 degrees of freedom
    ##  Chi-square: 1343.479 on 13 degrees of freedom, asymptotic p-value 0 
    ## AIC: 990.8608 AICC: 992.8327 BIC: 1038.872

We can see that the teacher is part of more interactions as the receiver
(being talked to) than we would expect based on other terms in the
model. This is likely the case because the teacher can easily talk to
anyone in the class (i.e., the teacher is not subject to only talking to
those adjacent to them in the classroom), and so the term on the seating
arrangement pushes the previous model to under predict interactions with
the teacher. This is now rectified in the current model.

#### 1.5. Comparison to a Second Date (Optional Second Example)

We have so far run a number of models, interpreted the results and
learned a bit about the interactional dynamics in this classroom. Here,
we run through the same exercise (an abbreviated version) using
interactional data from a different date. The classroom is the same, so
the actors are the same, but this class takes place later in the second
semester. More importantly, this was a date where there was a great deal
more misbehaving in the class and the teacher had to sanction students
to a much larger extent. Our question is how (or if) the interactional
tendencies are different in a day where the class is less orderly and
controlled. We begin by reading in the interactional data for this
second
date.

``` r
interactions_date2=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_interactions_date2.txt", 
                              header=T)
```

And again, we will take out those interactions where one node talks to
the entire class
simultaneously.

``` r
interactions_date2=interactions_date2[interactions_date2$to_all_col==0 & interactions_date2$from_all_col==0,]
```

And now we create the edgelist matrix, adding a row for the stop time
for the interactions (again, .10 standardized minutes after the last
recorded
interaction).

``` r
edgelist_date2=as.matrix(interactions_date2[,c("time_estimate_col", "send_col", "receive_col")])
tail(edgelist_date2)
```

    ##     time_estimate_col send_col receive_col
    ## 398            32.242       17          11
    ## 399            32.394       11          17
    ## 400            32.545       17           5
    ## 401            32.697        5          17
    ## 402            32.848        8           6
    ## 403            33.000        6           8

``` r
edgelist_date2=rbind(edgelist_date2, c(33.10, NA, NA))
```

The friendship data is the same as above (corresponding to the second
semester) but we need to read in the seating data for this
day.

``` r
seating_date2=read.table("https://sites.google.com/site/jeffreysmithdatafiles/class_seating_date2.txt", 
                         header=T)
head(seating_date2)
```

    ##   ego_id alter_id
    ## 1      1        7
    ## 2      3        5
    ## 3      3       11
    ## 4      3       17
    ## 5      4       14
    ## 6      4       18

Here we turn the edgelist into a matrix, as we did before.

``` r
seating_network_date2=network(x=seating_date2, directed=F) 
seating_matrix_date2=as.matrix(seating_network_date2)
```

And once again, we need to create a covEvent array with the new seating
matrix.

``` r
CovEvent_date2=array(data=NA, dim=c(2, class_size, class_size))
```

We will now put the first matrix, the seating matrix, in the first slot.

``` r
CovEvent_date2[1,,]=seating_matrix_date2
```

We will now put the second matrix, the friendship matrix, in the second
slot.

``` r
CovEvent_date2[2,,]=friends_matrix_sem2
```

We are now in a position to run the same model as we did above. We will
just run the preferred model (mod4f), with all terms included. Note that
the CovRec and CovSnd matrices are the same as above (as the attributes
are the same across time in this case).

``` r
mod4f_date2=rem.dyad(edgelist_date2, n=class_size,
                effects=c("CovSnd", "CovRec", "CovEvent", "RRecSnd", "RSndSnd",
                          "PSAB-BA", "PSAB-BY","PSAB-AY", "PSAB-XA", "PSAB-XB"), 
                covar=list(CovSnd=CovSnd2, CovRec=CovRec2, CovEvent=CovEvent_date2),
                ordinal=FALSE, hessian=TRUE)
```

Let’s add some more meaningful variable names. In, this case the terms
are the same as with mod4f, so we can grab those names and use them
directly.

``` r
names(mod4f_date2$coef)=names(mod4f$coef)
summary(mod4f_date2)
```

    ## Relational Event Model (Temporal Likelihood)
    ## 
    ##                   Estimate   Std.Err  Z value  Pr(>|z|)    
    ## Recency.ji        2.750085  0.308911   8.9025 < 2.2e-16 ***
    ## Recency.ij       -0.669326  0.211806  -3.1601  0.001577 ** 
    ## Intercept        -7.099816  0.302561 -23.4657 < 2.2e-16 ***
    ## Sender.male       0.151184  0.139797   1.0815  0.279494    
    ## Sender.teacher    0.634180  0.445491   1.4236  0.154576    
    ## Receiver.male     0.028254  0.143930   0.1963  0.844370    
    ## Receiver.teacher  2.368031  0.337424   7.0180 2.251e-12 ***
    ## Adjacent Seating  1.569521  0.254591   6.1649 7.054e-10 ***
    ## Friendship        1.442197  0.294272   4.9009 9.540e-07 ***
    ## PSAB-BA           3.613193  0.160669  22.4885 < 2.2e-16 ***
    ## PSAB-BY           1.980615  0.270846   7.3127 2.618e-13 ***
    ## PSAB-XA           0.722104  0.371715   1.9426  0.052061 .  
    ## PSAB-XB           0.968809  0.313228   3.0930  0.001982 ** 
    ## PSAB-AY           0.538728  0.398286   1.3526  0.176179    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## Null deviance: 2598.114 on 284 degrees of freedom
    ## Residual deviance: 704.5596 on 271 degrees of freedom
    ##  Chi-square: 1893.554 on 13 degrees of freedom, asymptotic p-value 0 
    ## AIC: 732.5596 AICC: 734.1209 BIC: 783.6452

We will create a little data frame to compare the coefficients from our
two days.

``` r
compare_coefs=data.frame(date1=mod4f$coef, date2=mod4f_date2$coef)
compare_coefs
```

    ##                       date1       date2
    ## Recency.ji        3.5056256  2.75008453
    ## Recency.ij       -1.5763777 -0.66932642
    ## Intercept        -6.4477083 -7.09981624
    ## Sender.male       0.2148523  0.15118430
    ## Sender.teacher   -1.1496443  0.63418049
    ## Receiver.male     0.0155446  0.02825445
    ## Receiver.teacher  1.4310115  2.36803143
    ## Adjacent Seating  0.8580655  1.56952143
    ## Friendship        0.9926651  1.44219703
    ## PSAB-BA           3.4784138  3.61319323
    ## PSAB-BY           1.3621509  1.98061497
    ## PSAB-XA           0.4001821  0.72210423
    ## PSAB-XB           1.1512505  0.96880924
    ## PSAB-AY           0.2311302  0.53872774

Overall, much of the same interactional rules we saw above (in the
‘normal’ day) hold when looking at this second date, where the class
was more unruly. We still see turn taking in interactions, for example
(AB and then BA). We still see rules around usurping the conversation,
such that when A talks to B and then X jumps in, they are likely to talk
to B. Still, there would appear to be some potentially important
differences (we would want to explore this more formally). For example,
the effects for friendship and seating are particularly important for
the second day. Similarly, there is some evidence that the tendency for
PSAB-BY p-shifts are relatively high here. A class that is more unruly
tends to have interactions that are based more on friendship and
adjacent seating (i.e. talking to neighbors rather than doing
discussion). Additionally, there may be a higher (relative) tendency for
nodes to form a kind of two-step interaction (A-B-Y) rather than just a
simple return to the person addressing them (A-B-A). This would
potentially create more disruption in the classroom, as a larger number
of students are brought into the initial interaction event.

It is also useful to compare these results to the kind of models we saw
in the previous tutorial on STERGM. In general, STERG models allow us to
test hypotheses about formation and persistence of ties. This opens up
questions about triadic effects and larger group structures. In the
language of relational event models, A may talk to B, and then B may
talk to Y; but when B talks to Y, Y is very likely to be someone that A
generally talks with. Thus, little groups in the classroom emerge that
are harder to see in the relational event model than with STERGM. On the
other hand, STERGM completely obscure the actual dynamics of
moment-to-moment interactions, missing some of the ‘rules’ of
interaction that come out so clearly in the relational event results.

Chapter 13 has covered statistical network models, moving from the
cross-sectional case all the way up to continuous-time network data. In
Chapter 14, we still utilize statistical network models, but we focus on
problems related to peer influence and diffusion.
