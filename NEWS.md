# AnVILBilling 0.0.10
- New parameter to browse_reck() that defaults to NOT running bq_auth explicitly.  Now
	only one authentication per session is performed if do_auth is FALSE

# AnVILBilling 0.0.9
- Plots in plot tab are now plotly for pointwise segmented display, and simple cumulative display

# AnVILBilling 0.0.7

- No warnings on R CMD check or BiocCheck; one ERROR concerning support site registration
- Includes browse_reck(), an app that tabulates expenses over time
- Updates to vignette, including demonstrative static graphics
- It seems that the page_size parameter does not properly propagate to BigQueryConnection;
	documenting this will take time; for now avoid querying for long intervals of time
