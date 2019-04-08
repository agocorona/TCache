#!/bin/bash

# recusivly clean out all the .tcachedata dirs inside the project folder
find . -type d -name ".tcachedata" -print0 | xargs -0 -I {} /bin/rm -rf "{}"
