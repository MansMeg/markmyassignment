# Frequently asked questions (FAQ) / Troubleshooting

Below are different identified problemas and possible solutions.

## Frequently asked questions

*Do I need to be connected to internet while doing my assignment?*

No. Only when you set the assignment initially with `set_assignment()`. Then all nessecary data is downloaded to your R session (and is deleted when you close down R).

*Does `markmyassignment` on all operating system?*

Yes. It should work on all systems that can install R.

## Troubleshooting

#### Problem with the SSL CA cert

*Error message:*

`Error: Problem with the SSL CA cert (path? access rights?)`

*Solution:*

1. Re-install R packages `curl` and `httr`
2. Restart R
