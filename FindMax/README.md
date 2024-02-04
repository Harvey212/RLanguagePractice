# hw2

### Name: [葉冠宏]
### Student ID: [108753208]

## cmd

```R
Rscript hw2_yourID.R --target male/female --input meth1 meth2 ... methx --output result.csv

```

* Read in multiple files
* Positive case defined by “--target” option
* Find out which method contains the max
* yourID should be your student ID number, i.e., hw2_106769999.R
* You should write your own function to calculate sensitivity, specificity and F1 except AUC which could be done by ROCR package.
* AUC is calculated based on the curve True Positive Rate vs False Positive Rate.
* hw2_example.R is for reference only.

## Inputs

* examples/method1.csv
* the last column, pred.score, is the predicted probability of "Male".


persons,prediction,reference,pred.score

person1,male,male,0.807018548483029

person2,male,male,0.740809247596189

person3,female,male,0.0944965328089893

person4,female,female,0.148418645840138

## Output
* examples/output1.csv

method,sensitivity,specificity,F1,AUC

method1,0.91,0.96,0.85,0.79

method2,0.99,0.98,0.86,0.70

highest,method2,method2,method2,method1

## Examples

```R
Rscript hw2_yourID.R --target male --input examples/method1.csv examples/method2.csv --output examples/output1.csv
Rscript hw2_yourID.R --target male --input examples/method1.csv examples/method3.csv examples/method5.csv --output examples/output2.csv
Rscript hw2_yourID.R --target female --input examples/method2.csv examples/method4.csv examples/method6.csv --output examples/output3.csv
```

## Scores

5 testing data

```R
Rscript hw2_5566.R --target male --input hw2/data/set1/method1.csv hw2/data/set1/method2.csv … --output hw2/your_ID/output1.csv
Rscript hw2_5566.R --target female --input hw2/data/set2/method1.csv hw2/data/set2/method2.csv … --output hw2/your_ID/output2.csv
```
Correct answer gets 18 points of each testing data.
**Please do not set input/output in your local path or URL.** 
Otherwise, your code will fail due to fixed path problem.

## Bonus

- Output format without “: 3 points
- Number in 2 digitals : 3 points
- Set format without file path: 4 points

## Penalty
- can not detect missing --input/--ouptut flag 
- -2 points of each problem
