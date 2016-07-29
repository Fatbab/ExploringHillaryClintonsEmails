# ExploringHillaryClintonsEmails

This R code inlcudes creating wordclouds, dendrogram clusters and a not very efficient (in terms of lines-of-code) for performing sentiment analysis on Hillary Clinton's emails published on [Kaggle](https://www.kaggle.com/kaggle/hillary-clinton-emails).

### Goal
Study how the sentiments used in private emails match the sentiments presented to the public via interviews with news agencies, around specific matters.

To narrow down the volume of work, specific keywords are picked and searched for in the emails (subject line + body). Then we focused on only 20 words before and after each of these keywords to form a corpus. This also helps reduce the chance of getting sentiments from a different topics counted in for the sentiments about the subject matter. 

### Steps
1. Load data from a local sqlite server 
2. Create word cloud of emails
3. Dendrogram clustering, emails and interviews
4. Sentiment Analysis 

### Results
1. Wrodclouds     
2. Sentiment charts    
3. Interviews and emails dendrograms    

### To Be Improved
1. Many of the sentiment analysis steps can be re-structured into function calls.
1. More interviews should be analysed to make the training set sizes comparable.
2. Use of bigrams and trigrams for capturing sentiments.
3. Research on other ways to form a reliable and comprehensive corpus.

### References
1. [Kaggle for emails data set](https://www.kaggle.com/kaggle/hillary-clinton-emails)
2. [Transcript of Interviews](http://www.state.gov/r/pa/ei/speeches/) 
3. [CRAN entry on SYUZHET package](https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html)
4. [Getting Started with Quanteda](https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html)
5. [StackOverflow entry on replacing LIWC dictionary with the freely–available Moral Foundation Dictionary](http://stackoverflow.com/posts/34199517/revisions)
6. [Moral Foundation Dictionary](http://www.moralfoundations.org/othermaterials)
