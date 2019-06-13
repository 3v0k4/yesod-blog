#!/bin/bash

USER_AT_HOSTNAME="$USER@$HOSTNAME"

docker run --rm -v $(pwd):/src yesod-blog /bin/bash -c "cd /src && stack clean && stack build"
ssh -t $USER_AT_HOSTNAME "sudo systemctl stop yesod"
scp -r config $USER_AT_HOSTNAME:~/yesod
scp -r static $USER_AT_HOSTNAME:~/yesod
scp -r .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/yesod-blog/yesod-blog $USER_AT_HOSTNAME:~/yesod
ssh -t $USER_AT_HOSTNAME "sudo systemctl start yesod"
