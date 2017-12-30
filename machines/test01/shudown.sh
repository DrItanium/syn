#!/bin/bash

echo "Shutting down separate units!"
rlwrap $1/syn -f2 $1/machines/test01/ShutdownMachine.clp

tmux kill-session -t test01_xu

#TODO: put in code to delete the frontend connection
rm -rf /tmp/machines/test01
