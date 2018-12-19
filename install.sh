# Move into script directory.
OLD_DIR=`pwd`
SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

# install package.
R -e 'devtools::document(); devtools::install()'

# Return to the original directory.
cd $OLD_DIR
