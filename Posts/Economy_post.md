The Role of the Economy in Elections
================
Samuel Thau
9/19/2020

## Introduction

Many election forecasting models rely at least in part on so called
“fundamentals,” that are outside of the candidates control. The
classic example of a “fundamental” is the economy. The incumbent
challengers (and theoretically, a candidate from the incumbent party)
could have some influence on the economy, but in general it is well
outside of the control of any one person or campaign. For example, some
believe that the economy was the deciding factor in the 2012 election,
given then President Barack Obama the edge over Mitt Romney, with the
economy growing just enough to overcome the typical anit-incumbent
sentiment in United State elections\[1\].

In doing so, a number of assumptions are made about voters. There are
three main factors to consider when attempting to model voter choice as
a function of the economy:

1)  How direct is the relationship between voting and the economy?
2)  Do voters have complete information?
3)  Do voters have a sociotropic or individual focus?

Questions 2 and 3 have been studied empirically, and will inform the
model that I build during this post. The answer to question 1 is more
philosophical: does seeing higher GDP numbers make voters more inclined
to vote one way or another? Or is it more indirect, where a higher GDP
leads to more money in people’s pockets, making them happier which makes
them more inclined to vote for the status quo? Figuring out the answer
is tricky.

## Choosing Model Inputs

To model voter behavior, we must first think about the inputs into a
model. Let’s begin with the issue of time frame. It is a well documented
fact that people have short memories when it comes to abstract ideas
like the economy, and that remains true when looking at voting outcomes.
Not only are people much more responsive to the election year
economy\[2\], placing nearly 75% of the weight on the election year
economy, they place the majority of their weight on the final two
quarters before the election\[3\]. Because of this, I will use data from
the second quarter of 2020 (as third quarter data does not yet exist).

To deal with the problem of figuring out which variables to choose,
consider both questions 1 and 3 from the introduction. I believe that it
is a reasonable assumption that people care about their own income, but
also are influenced by the national economy (either indirectly through
the job market, or just through the news). Because of this, I will use
two economic indicators: national level GDP growth in quarters 1 and 2
of the election year, and personal income growth in quarters 1 and 2. In
both cases, the data I use is compared to the previous quarter and
controls for the time of year. I will vary the scope (e.g. national vs
state) of the personal income data, depending on the exact model.

Varying the scope of personal income is intentional: I believe it is a
reasonable assumption to make that for more direct economic indicators,
people may be influenced a more local level. After examining the
results, we will return to if this is actually a reasonable assumption
to make.

## A National Model

We can begin with a national level model, looking at data over time. We
can regress national personal income growth in the first and second
quarters, along with national GDP growth on the incumbents two party
vote share in each election year.

    ## 
    ## Call:
    ## lm(formula = vote_margin ~ 1 + GDP_growth_qt_1 + GDP_growth_qt_2 + 
    ##     RDI_growth_1 + RDI_growth_2, data = nat_econ_df)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -6.478 -2.683  1.478  2.616  5.366 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       48.343      2.621  18.447 4.72e-09 ***
    ## GDP_growth_qt_1    2.254      1.467   1.536    0.156    
    ## GDP_growth_qt_2    2.149      1.426   1.507    0.163    
    ## RDI_growth_1      -1.675      2.149  -0.779    0.454    
    ## RDI_growth_2       1.416      1.928   0.735    0.479    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.388 on 10 degrees of freedom
    ## Multiple R-squared:  0.5003, Adjusted R-squared:  0.3004 
    ## F-statistic: 2.503 on 4 and 10 DF,  p-value: 0.1091

This model does an all together mediocre job of prediction.

  - It has an R^2 value of `0.50`, which means that the model explains
    almost exactly half of the variance in the dataset.

  - There is a negative coefficient on the first quarter personal income
    of `-1.675`, while there is a positive coefficient on the second
    quarter personal income of `1.416`. This would suggest that voters
    really have a “what have you done for me lately” attitude. This
    means that for an increase of one percent personal income growth in
    the first quarter, holding all else constant, the vote share for the
    incumbent party is expected to decrease.

  - In general, the standard errors for the coefficients are quite high,
    relative to the magnitude of the coefficients, suggesting that these
    results cannot be taken particuarly seriously. However, this may
    just be a problem with sample size.

All together, this model seems like it could be useful, but we must
first map it onto the state level. As discussed in last week’s post, the
states are what really matters for winning elections.

## A State Level Model

1.  Sides, John, et al. The Gamble: Choice and Chance in the 2012
    Presidential Election - Updated Edition. Princeton University Press,
    2014. Project MUSE muse.jhu.edu/book/64467.

2.  Healy, Andrew, and Lenz, Gabriel S. “Substituting the End for the
    Whole: Why Voters Respond Primarily to the Election-Year Economy.”
    American Journal of Political Science, vol. 58, no. 1, 2014,
    pp. 31–47.

3.  Achen, Christopher H, and Bartels, Larry M. Democracy for Realists.
    REV - Revised ed., Princeton University Press, 2017.
