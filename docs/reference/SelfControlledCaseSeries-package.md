# SelfControlledCaseSeries: Self-Controlled Case Series

Execute the self-controlled case series (SCCS) design using
observational data in the OMOP Common Data Model. Extracts all necessary
data from the database and transforms it to the format required for
SCCS. Age and season can be modeled using splines assuming constant
hazard within calendar months. Event-dependent censoring of the
observation period can be corrected for. Many exposures can be included
at once (MSCCS), with regularization on all coefficients except for the
exposure of interest. Includes diagnostics for all major assumptions of
the SCCS.

## See also

Useful links:

- <https://ohdsi.github.io/SelfControlledCaseSeries/>

- <https://github.com/OHDSI/SelfControlledCaseSeries>

- Report bugs at
  <https://github.com/OHDSI/SelfControlledCaseSeries/issues>

## Author

**Maintainer**: Martijn Schuemie <schuemie@ohdsi.org>

Authors:

- Patrick Ryan

- Trevor Shaddox

- Marc Suchard
