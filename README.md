# Pension-Funding-Shortfall
This repository covers the pension funding shortfall. More specifically, it deals with the extension of the work of Milevsky, Ho and Robinson. In this work, the non-normality in the financial market is incorporated. 

The financial market in our case is the Belgian BEL20 index. 

To work with this non-normality, simulations need to be made. The calibration of t-distributions to the data is done in an efficient way using the tidy-verse in R. Afterwards, a copula framework is used to simulate returns on the whole market. 

This work has been done for an assignment for the cours "Actuarial Mathematics for Pensions" at the KU Leuven.
