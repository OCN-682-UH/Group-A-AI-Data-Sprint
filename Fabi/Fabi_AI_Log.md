## Fabiola Gonzalez Zamora

### AI Log for OPIHI Data Sprint

- Prompt:
  - `I have the anonymized data and need to merge the 2024 data since there are two datasheets that have discrepencies in row numbers and naming schemes. Please help me identify and remove blank rows, clean each form to make it compatible for binding.`
- Response:
  - First run did not work, issue was in the the `semi_join` function when trying to sort the columns
  - `arrange()` had to be placed before `select()` so that `site_key` was not dropped from the clean file
- Output:
  - `2024_joined_long.csv`
  
