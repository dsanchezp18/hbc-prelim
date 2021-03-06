# Honesty by Convenience: Additional Data Wrangling and Analysis Scripts

This is the README file for the repository with the additional R scripts to produce the databases needed to replicate the paper. While the `hbc-ctol`already have manipulated dataframes, this script shows all of the code required to produce those databases on your own, if you were to download them from [the LAPOP AmericasBarometer website](https://www.vanderbilt.edu/lapop/free-access.php). 

You must use a fork of the `margins` package for this to work, as for some reason there seems to be some kind of mistake in the coding of the `margins` package for survey-weighted generalized linear models. I've explained this [here](https://stackoverflow.com/a/69051248/15555143). The sad thing that I've discovered is that this fork is not perfect: it causes the computation of the APEs of any model very, very [lengthy](https://github.com/leeper/margins/issues/124). In the future I hope there's a solution, because I'm way over my head in that. 

The RMarkdown notebooks which can be seen on [RPubs](https://rpubs.com/dsanchezp998) do not compile as a pdf, you must simply preview them so that the html files are generated, and you can see them through RStudio's viewers. This is because it uses elements from the global environment- you must run the main script if you want it to work. I'll work on this soon to avoid more issues and ensure no reproducibility issues. 

To be honest, this project wasn't thought to be as reproducible as the other repository, so once again I'm open to any comments and questions in my email, dsanchezp998@gmail.com. 

Again, this is in honor of my grandpa. Descansa en paz abuelito.

Regards,

Daniel

