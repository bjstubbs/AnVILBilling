# AnVILBilling 0.0.7

- No warnings on R CMD check or BiocCheck; one ERROR concerning support site registration
- Includes browse_reck(), an app that tabulates expenses over time
- Updates to vignette, including demonstrative static graphics
- It seems that the page_size parameter does not properly propagate to BigQueryConnection;
	documenting this will take time; for now avoid querying for long intervals of time
