#!/bin/bash

echo "Shutting down separate units!"
rlwrap ./syn -f2 $1/machines/test01/ShutdownMachine.clp


tmux attach -t test01_xu:1 
tmux send-keys "exit" C-m
tmux attach -t test01_xu:2
tmux send-keys "exit" C-m
tmux attach -t test01_xu:3
tmux send-keys "exit" C-m
tmux attach -t test01_xu:4
tmux send-keys "exit" C-m
tmux attach -t test01_xu:5
tmux send-keys "exit" C-m

#TODO: put in code to delete the frontend connection
rm -rf /tmp/machines/test01
