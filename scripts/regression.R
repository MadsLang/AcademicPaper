# run model
model_simple <- lm(
  persinc2 ~ rsex,
  data = nilt
)

model <- lm(
  persinc2 ~ rsex + religcat + orient + uninatid + tunionsa + rsuper + rage,
  data = nilt
)

# Set name of variables
cov_labels <-c(
  'Gender: Female (ref.: Male)',
  'Religion: Protestant (ref.: Catholic)',
  'Religion: No religion',
  'Sexual Orientation: Homosexual (ref.: Heterosexual)',
  'Sexual Orientation: bi-sexual',
  'Sexual Orientation: Other',
  'Constitutional View: Nationalist (ref.: Unionist)',
  'Constitutional view: Neither',
  'Trade union membership: Yes (ref.: No)',
  'Supervisor: Yes (ref.: No)',
  'Age',
  'Constant'
)
# Print result
stargazer(
  model_simple,
  model,
  column.labels = c("Model excl. controls", "Model incl. controls"),
  model.numbers=FALSE,
  header=FALSE,
  summary=TRUE,
  title="Regression results",
  covariate.labels = cov_labels,
  font.size = "small",
  dep.var.caption  = "Dependent Variable",
  dep.var.labels   = "Annual Personal Income (GBP)",
  table.placement = "H"
)
