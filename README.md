Voting with your tweet: 2012 edition
====================

This repo holds the prediction algorithms, code, and supporting data
for real-time prediction of the 2012 Congressional elections using the
Twitter feed. The project is based on a [working
paper][http://markhuberty.berkeley.edu/files/twitter_paper.pdf.zip] by
[Mark Huberty][http://markhuberty.berkeley.edu/],
entitled "Voting with your tweet: forecasting elections with social
media data". 

Background
-----------------------
A range of very interesting papers have attempted to predict elections
based on the content of election-related messages in the Twitter
feed. These include papers on the U.S. Presidential Election ([O'Connor
et al 2010][http://brenocon.com/oconnor_balasubramanyan_routledge_smith.icwsm2010.tweets_to_polls.pdf]), the
German Bundestag ([Tumasjan et al 2010][https://www.aaai.org/ocs/index.php/ICWSM/ICWSM10/paper/view/1441/1852]), and the British
Parliament ([Tweetminster 2010][http://www.scribd.com/doc/29154537/Tweetminster-Predicts]). More recently, Daniel Gayo-Avello pointed out various
problems with most of these papers. One of the more significant
problems he identifies is the lack of /a priori/ prediction of future
elections. This project is an experiment doing just that.

The algorithms in the </algorithms> folder were trained on the results
of the 2010 United States Congressional elections. They use a bigram
bag-of-words language model and the SuperLearner machine learning
algorithm proposed by [van der Laan et al (2007)][http://biostats.bepress.com/ucbbiostat/paper222/]. Both binary (win/loss) and
continuous (vote share) algorithms are provided. More detail can be found in the
working paper cited above. 

<!---
During the election
-----------------------
During the election, we will post real-time prediction updates (and
more information) at [][]. 
-->
