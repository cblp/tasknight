[![Build Status](https://travis-ci.org/cblp/tasknight.svg?branch=master)](https://travis-ci.org/cblp/tasknight)

# Tasknight

Your notification center.

## Development

### Build

    $ ./build.hs

### Test

1. Install Postgres
  - Ubuntu:

          $ sudo apt install postgresql
  - Mac OS:

          $ brew install postgresql
          $ brew services start postgresql
2. Run
        $ ./build.hs --test

### Run

    $ ./build.hs --yesod-devel
