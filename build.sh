# Move into script directory.
OLD_DIR=`pwd`
cd `(cd $(dirname $0); pwd)`

# Clone repository.
mkdir build
cd build
git clone https://github.com/Marchen/model.adapter.git
cd model.adapter

# Remove extra files.
rm install.sh
rm include.r
rm .travis.yml
rm build.sh

# Build package.
R -e 'devtools::document(); devtools::build(path = "..")'

# Return to the original directory.
cd $OLD_DIR
