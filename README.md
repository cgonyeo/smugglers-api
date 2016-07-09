# smugglers

This project aims to provide a simple HTTP JSON API that wraps sc.bevager.com.
This exists to facilitate creating alternative frontends for members of the
Rumbustion Society.

The wrapping is done by logging in as users to sc.bevager.com and scraping the
HTML for their data. This information is then cached in a local postgres
database for faster data lookup.
