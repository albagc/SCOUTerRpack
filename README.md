# SCOUTerRpack
## SCOUTer package in R
Simulating anomalous data is an extremely common procedure. However, very little attention is paid to this step and it is usually defined ad hoc, existing a lack of standard. In this package, a new framework to simulate outliers directly controlling their outlying properties has been proposed. 
This framework offers the possibility of generating data sets with *all type of desired properties*, given that the user can control the pair of statistics that essentially define outliers: the Squared Prediction Error ( _SPE_ ) and the Hotelling-T<sup>2</sup> ( _T<sup>2</sup>_ ).
These metrics evaluate in a complementary way how far is an observation from the majority of a data set. Since 
Given an observation with initial values for the statistics, a PCA model and target values for the statistics, our simulation method drifts the observation in a direction that shifts the initial _SPE_ and the _T<sup>2</sup>_ until reaching their target values. 