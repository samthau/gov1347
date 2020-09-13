Introduction: Electoral Results and Tipping Points
================
Samuel Thau
9/12/2020

## Introduction

Before we get into prediction, it will be helpful to get some background
on the history of electionis in the United States. Let’s start with a
relatively simple question: how do election results change year to year?
There are several ways to measure the results of an election:

1.  Popular vote share - this is what most people think of as the
    “popular vote.”
2.  Two party popular vote share - this exculdes third party candidates,
    which have not had a major impact on presidential election outcomes
    since before World War II. However, at a state level, third parties
    can have a signficant impact, as there have been relatively recent
    instances of third party candidates winning small numbers of states
    and electoral votes.
3.  Electoral votes - awarded on a state by state basis, this is what
    candidates have to win to win the presidency. I’ll take a closer
    look at what states seem matter (by one definition of “matter”)
    later in this entry.

## Popular Vote Share

Because the presidential election is determined on a state by state
basis, it’s good to start looking at some maps. For now, I’ll stick to
the two party vote share, becuase intuitively that’s what matters for
election outcomes; because of the weakness of third party campaigns
since WWII, whoever wins between the Republican and Democrat almost
always wins the state and therefore the electoral votes.

![](Introduction_files/figure-gfm/margins-1.png)<!-- -->

Looking at the voting margins in general can be deceptive: because
states are mostly winner-take-all, a narrow win counts the same as a
blowout in terms of results. For example, in `1972`, **Richard Nixon**
beat **George McGovern** by `59.9` percentage points in Mississippi,
meaning that if **185k people** switched their votes from Nixon to
McGovern, Nixon *still* would have won Mississippi and it’s electoral
votes. With that in mind, ignoring the gradient and focusing solely on
winners and losers may paint a better picture of electoral trends over
time.

![](Introduction_files/figure-gfm/winner-1.png)<!-- -->

There are a lot of different conclusions that can be drawn from the
winner take all graphic, many of which I’m excited to explore as the
class continues.For example, certain states have remained remarkably
consistent in which party they vote for: Texas has not voted for a
Democrat since `1976`, and Minnesota has not voted for a Republican since
`1972`. Exploring this stability is something I will try to focus on
throughout the blog.

A key part of the trend that I am interested in is the geographic
components of this polarizaiton: since `1992`, the west coast and
northeast have reliably voted for Democrats, while much of the center of
the country has voted for Republicans. Understanding what drives this
trend is crucial to predicitng future elections, as it may give
important information about how large swathes of the country will vote.

## Tipping Point States

The combination of the electoral college and looking at the vote share
over time begs a question: which states matter to decide elections? To
do this, I’ll use the idea of a tipping point state.

A **tipping point state** is a state that puts a candidate over the
threshold to win the electoral college. We calculate this threshold
value by taking the total number of electoral votes, dividing by 2 and
then adding 1.

Then, to figure out the tipping point state, we order the states by net
vote share. For example, in `2016`, Donald Trump won `72.1` of the vote in
West Virginia, so that would be the first state on his list. The rest of
the states are ordered by the margin they voted for or against Trump,
keeping track of the electoral college votes for each state. From there,
the state that puts the winner (in this example, Trump) over the
threshold is the so called tipping point state.

All of this disucssion leads to a natural question: what are the most
common tipping point states? We can produce a list of the tipping point
state in every election going back to WWII. I matched the electoral
college values that I got from
[archive.gov](https://www.archives.gov/electoral-college/results) and
[Wikipedia](https://en.wikipedia.org/wiki/United_States_Electoral_College)
for each state with the two party vote shares from Lab 1. There are some
distinct drawbacks to using two party vote share, instead of raw vote
share:

1.  In `1948` and `1964`, Alabama refused to put Harry Truman and Lyndon B.
    Johnson, respectively, on the ballot. The electoral votes were not
    pledged, so I set the Alabama electoral college votes for those
    years to zero.

2.  Some third party candidates have won small numbers of electoral
    votes, which will not be accounted for with the two party vote
    share. Strom Thurmond in `1948`, and George Wallace in `1968` were the
    only third party candidates to pick up electoral votes. Becuase I am
    using the two party vote share, I set the electoral college values
    of the states they won to zero because they did not go to major party
    candidates.

3.  The two party vote share margins may not be the same as the raw vote
    share margins, leading to discrepencies in actual calculations.

Two unrelated issues to consider are the ideas of faithless electors and
states splitting their electoral votes: for simplicity, I ignored both
of these factors in the calculations as they tend to have minimal
effects. None the less, I continue forward and calculate the tipping
point states for each election since
WWII.

| Election | Tipping Point State | Vote Share Margin in Tipping State | Electoral College Margin |
| :------- | :------------------ | :--------------------------------- | :----------------------- |
| 1948     | Illinois            | D+0.85                             | D+115                    |
| 1952     | Michigan            | R+11.533                           | R+353                    |
| 1956     | Louisiana           | R+14.846                           | R+383                    |
| 1960     | New Jersey          | D+0.804                            | D+97                     |
| 1964     | Washington          | D+24.756                           | D+444                    |
| 1968     | Ohio                | R+2.59                             | R+111                    |
| 1972     | Ohio                | R+22.069                           | R+504                    |
| 1976     | Wisconsin           | D+1.723                            | D+56                     |
| 1980     | Illinois            | R+8.679                            | R+358                    |
| 1984     | Michigan            | R+19.093                           | R+512                    |
| 1988     | Michigan            | R+7.956                            | R+314                    |
| 1992     | Colorado            | D+5.604                            | D+202                    |
| 1996     | Pennsylvania        | D+10.322                           | D+220                    |
| 2000     | Florida             | R+0.009                            | R+4                      |
| 2004     | Ohio                | R+2.117                            | R+34                     |
| 2008     | Colorado            | D+9.101                            | D+190                    |
| 2012     | Pennsylvania        | D+5.464                            | D+126                    |
| 2016     | Pennsylvania        | R+0.751                            | R+72                     |

This table shows us the tipping point state in each election, and the
margin of victory in that state. There are a few key points from this
table:

1.  Which states matter in terms of the overall outcomes of elections.
    These states roughly line up with the popular idea of which states
    are “swing states.” In `2020`, Michigan, Ohio, and Pennsylvania are
    all like to be important. According to
    [FiveThirtyEight](https://projects.fivethirtyeight.com/swing-states-2020-election/)
    all three have swung during the past two elections, and Pennsylvania
    is
    [projected](https://projects.fivethirtyeight.com/2020-election-forecast/)
    to be the tipping point state.

2.  Which elections were competitive, in one sense. We can see that `2000`
    and `2016` had razor thin margins in Florida and Pennsylvania,
    respecitively, demonstrating the extraordinarily competitive
    elections. On the opposite end of the spectrum, Lyndon B. Johnson
    trounced Barry Goldwater in `1964`, as clearly indicated by Johson
    winning the tipping point state by nearly 25 percentage points.

3.  Close results in the tipping point state do not neccesarily imply
    close results in the overall electoral college margin. This is
    because results from the tipping point state do not *have* to be
    correlated with national results. It may be that a state like
    Pennsylvania votes similarly to Ohio, but not to Florida or New York
    which carry large numbers of electoral votes.

We can take a closer look at the `2016`
election:

| State        | Trump Victory Margin | Electoral College Votes | Cumulative Electoral College Votes |
| :----------- | -------------------: | ----------------------: | ---------------------------------: |
| Arizona      |                3.780 |                      11 |                                230 |
| Florida      |                1.238 |                      29 |                                259 |
| Wisconsin    |                0.816 |                      10 |                                269 |
| Pennsylvania |                0.751 |                      20 |                                289 |
| Michigan     |                0.235 |                      16 |                                305 |

States that Donald Trump narrowly won in 2016.

Donald Trump was able to win a number of states by a small margin, with
a razor thin win in Pennsylvania to seal the electoral college victory.
Despite losing the popular vote, Trump was able to win just enough
states on small margins to win the overall victory, meaning that he had
an electoral college advantage.

When we turn to forecasting and prediction for the rest of this class,
having this information as a background will be useful. Close predicted
margins in the projected tipping point state likley means a high degree
of uncertainty. In addition, this demonstrates the heterogeneity of
voting in different states, suggesting that an effective forecasting
model will work on a state by state basis. Keeping track of different
paths to victory for the candidates will be crucial to accurately
predicting the overall outcome, because that will determine the
victor.
