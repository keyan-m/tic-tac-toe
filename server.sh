# SCRIPT TO COPY SOURCE FILES TO A REMOTE
# SERVER AND BUILD/RUN THE SERVER REMOTELY.


clear
date
echo -e "===============================\n"


# {{{ CONSTANTS 

# Change these accordingly:
remoteUser="delixir"
remoteIP="172.16.42.7"
remoteDir="/home/delixir/learnServant"
project="tic-tac-toe"
remoteProjectDir="$remoteUser@$remoteIP:$remoteDir/$project"

# Note the project directory is presumed to be structured as:
#
# $project              # main directory
# | $project-frontend   # frontend directory
# | $project-backend    # backend directory
# | server.sh
# | README.md
# | etc.

# }}}


# {{{ FUNCTIONS 
copyBackend() { # Copy the backend files to the remote server.
  # {{{
  rsync -av -e ssh           \
    --exclude=".*"           \
    $project-backend         \
    $remoteProjectDir
  # }}}
}

copyFrontend() { # Copy the frontend files to the remote server.
  # {{{
  rsync -av -e ssh           \
    --exclude=".*"           \
    --exclude="node_modules" \
    --exclude="elm-stuff"    \
    $project-frontend        \
    $remoteProjectDir
  # }}}
}

closeServer() { # Two attempts at finding the server process and killing it.
  # {{{
  echo -e "Closing the server..."
  pid=$(runRemoteCommand "ps -eo pid,stat,command | grep $remoteDir/$project/$project-backend/.stack-work/install/* | grep 'Ssl' | awk '{print \$1}'")
  runRemoteCommand "kill $pid"
  pid=$(runRemoteCommand "ps -eo pid,stat,command | grep $remoteDir/$project/$project-backend/.stack-work/install/* | grep 'Sl' | awk '{print \$1}'")
  runRemoteCommand "kill $pid"
  echo -e "Done.\n"
  # }}}
}

runRemoteCommand() { # Run a given command on the remote machine via ssh.
  # {{{
  ssh $remoteUser@$remoteIP $1
  # }}}
}

runNpm() { # Run an npm script command inside the remote frontend folder.
  # {{{
  runRemoteCommand "cd $remoteDir/$project/$project-frontend && npm $1"
  # }}}
}

copyConfig() { # Copy a given file from the remote frontend folder to the local machine.
  # {{{
  scp $remoteProjectDir/$project-frontend/$1 $project-frontend/$1
  # }}}
}
# }}}


case $1 in
  npm)
    # {{{
    copyFrontend
    runNpm $2
    ;;
    # }}}
  start)
    # {{{
    closeServer
    copyBackend
    copyFrontend
    case $2 in
      npmStart)
        runNpm start
        ;;
    esac
    runRemoteCommand "cd $remoteDir/$project/$project-backend && stack run &"
    ;;
    # }}}
  close)
    # {{{
    closeServer
    ;;
    # }}}
  copyConfigs)
    # {{{
    copyConfig webpack.*.js
    copyConfig package.json
    copyConfig tailwind.config.js
    copyConfig postcss.config.js
    copyConfig elm.json
    ;;
    # }}}
  *)
    # {{{
    echo -e "\nUse one of the following arguments:"
    echo -e "  • Build the frontend:\t\t\tnpm <script command>"
    echo -e "  • Start the server:\t\t\tstart (optionally add \`npmStart\` to also build the frontend)"
    echo -e "  • Close the server:\t\t\tclose"
    echo -e "  • Copy config files from server:\tcopyConfigs\n"
    ;;
    # }}}
esac
