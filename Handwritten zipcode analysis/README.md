# Handwritten zipcode analysis

For details, please check the report [HERE](https://github.com/yovalishere/Computer-Vision/tree/main/Handwritten%20zipcode%20analysis)
### Project description
This project aims at comparing **linear regression** and **K-nearest neighbor(KNN) regression** on their performance in predicting the handwritten digits which were scanned
from envelopes by the U.S. Postal Service.

### Data description
The first column of the [dataset](https://web.stanford.edu/~hastie/ElemStatLearn//datasets/zip.info.txt) is digit ID(ranging from 0-9) which is our dependent variable ; whereas the
remaining 256 columns are the grayscale value, which are our independent variables. To narrow down the scope, a subset of the dataset, which only contains either digitID = 2 
and digitID=7 is used in this analysis. 

<img src="https://github.com/yovalishere/Computer-Vision/blob/main/Handwritten%20zipcode%20analysis/2_7.jpg" width="600" height="250" />
*The above images are the visualisations generated from the 2 data points randomly selected from our subset. 


### Project findings
The optimal value for K in KNN is found to be 3 since it gives the lowest testing error. However, linear regression was found to perform better in general
in terms of testing error mean and variance.
