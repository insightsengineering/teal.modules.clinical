# Reporting an Issue with teal.modules.clinical

Please briefly describe your problem and, when relevant, the output you expect.
Please also provide the output of `utils::sessionInfo()` or
`devtools::session_info()` at the end of your post.

If at all possible, please include a [minimal, reproducible
example](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example). If the data is restricted for your example then try to generate random data that re-creates the problem, the [random.cdisc.data](https://github.roche.com/NEST/random.cdisc.data) R package might be useful for that.
The `teal.modules.clinical` team will be much more likely to resolve your issue if they are
able to reproduce it themselves locally.

Please delete this preamble after you have read it.

---

your brief description of the problem

```r
library(teal.modules.clinical)

# your reproducible example here, e.g.

library(random.cdisc.data)
ADSL <- radsl(seed = 1)

app <- init(
  data = cdisc_data(
    cdisc_dataset("ADSL", ADSL)
    code = "ADSL <- radsl(seed = 1)"
    check = TRUE
  ),
  modules = root_modules(
    tm_t_summary(
    	label = "Demographic Table",
    	dataname = "ADSL",
        arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
        summarize_vars = choices_selected(c("SEX", "RACE"), "SEX")
    )
  )
)

shinyApp(app$ui, app$server)
```
