# Reporting an Issue with teal.modules.tern

Please briefly describe your problem and, when relevant, the output you expect.
Please also provide the output of `utils::sessionInfo()` or
`devtools::session_info()` at the end of your post.

If at all possible, please include a [minimal, reproducible
example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example). If the data is restricted for your example then try to generate random data that re-creates the problem, the [random.cdisc.data](https://github.roche.com/Rpackages/random.cdisc.data) R package might be useful for that.
The `teal.modules.tern` team will be much more likely to resolve your issue if they are
able to reproduce it themselves locally.

Please delete this preamble after you have read it.

---

your brief description of the problem

```r
library(teal.modules.tern) # library(teal.tern) if you are using older version

# your reproducible example here, e.g.

library(random.cdisc.data)
ASL <- radam("ASL", N=200)

x <- teal::init(
  data = list(ASL=ASL),
  modules = root_modules(
    tm_data_table(),
    tm_t_summary(
    	label = "Demographic Table",
    	dataname = "ASL",
        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
        summarize_vars = choices_selected(c("SEX", "RACE"), "SEX")
    )
  )
)

shinyApp(x$ui, x$server)
```
