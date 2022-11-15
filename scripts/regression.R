# This chunk will run the linear regression and print the output
# run model
model <- lm(
  persinc2 ~ rsex + religcat + orient + uninatid + tunionsa + rsuper + rage,
  data = nilt
)

# Set name of variables
cov_labels <-c(
  'Sex: Female (ref.: Male)',
  'Religion: Protestant (ref.: Catholic)',
  'Religion: No religion',
  'Sexual Orientation: Homosexual (ref.: Heterosexual)',
  'Sexual Orientation: bi-sexual',
  'Sexual Orientation: Other',
  'Constitutional View: Nationalist (ref.: Unionist)',
  'Constitutional view: Neither',
  'Trade union membership: No (ref.: Yes)',
  'Supervisor: No (ref.: Yes)',
  'Age',
  'Constant'
)
# Print result
stargazer(
  model,
  header=FALSE,
  summary=TRUE,
  title="Regression results",
  covariate.labels = cov_labels,
  font.size = "small",
  dep.var.caption  = "Dependent Variable",
  dep.var.labels   = "Annual Personal Income (GBP)",
  table.placement = "H"
)
