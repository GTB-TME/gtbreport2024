#!/bin/bash
# NOTE this is more a reminder of how to re-run individual scripts than any suggestion they should be run in one go

#from R/
R -q -e "rmarkdown::render(\"0datacases.R\",output_dir=\"../html\")"
R -q -e "source(\"1prepdata.R\")"


#from scripts/
./runchoice.sh # runs 2stanfitting.R for the choice


#from R/
R -q -e "source(\"4update_fr.R\")"
R -q -e "source(\"6incidenceoutput.R\")"
R -q -e "source(\"7Hincidence.R\")"
R -q -e "source(\"8RR_mort.R\")"
R -q -e "source(\"9fqrinRR.R\")"

