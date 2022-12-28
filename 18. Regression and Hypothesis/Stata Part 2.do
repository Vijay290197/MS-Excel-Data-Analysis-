Do File for Codes

/* Part 2 */

* Data Load
import excel "D:\Augment Systems Kasotiya\Excel\18. AH0814981- Regression and Hypothesis\AH081498176908249stathw-Copy.xlsx", sheet("Height") firstrow

*Regress the dataset
regress height female sleep shoe
predict pred_height
list height pred_height in 1/10
predict resid_height, residuals
list height pred_height resid_height in 1/10

* Graph
scatter pred_height resid_height


*/ Data: Writer.csv 

Reference Group */

regress agedeath novelist poet

regress agedeath ib0.novelist ib0.poet

regress agedeath ib1.novelist ib1.poet


*/ Testing */

* c
oneway agedeath writer, tabulate

* d
ttest agedeath, by(poet)
