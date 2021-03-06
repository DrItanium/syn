#!/bin/bash

mkdir -p /tmp/machines/test01
pushd $1
make
popd

tmux -2 new-session -d -s test01_xu

echo "Setting up register file!"
tmux new-window -t test01_xu:1 -n "gpr"
tmux send-keys "cd $1" C-m
tmux send-keys "rlwrap ./syn -f2 machines/test01/RegisterFile_desc.clp" C-m

echo "Setting up ALU!"
tmux new-window -t test01_xu:2 -n "alu"
tmux send-keys "cd $1" C-m
tmux send-keys "rlwrap ./syn -f2 machines/test01/ALU_desc.clp" C-m

echo "Setting up comparator!"
tmux new-window -t test01_xu:3 -n "cmp"
tmux send-keys "cd $1" C-m
tmux send-keys "rlwrap ./syn -f2 machines/test01/Comparator_desc.clp" C-m

echo "Setting up binary unit!"
tmux new-window -t test01_xu:4 -n "blu"
tmux send-keys "cd $1" C-m
tmux send-keys "rlwrap ./syn -f2 machines/test01/BLU_desc.clp" C-m

echo "Setting up Memory Unit!"
tmux new-window -t test01_xu:5 -n "mem"
tmux send-keys "cd $1" C-m
tmux send-keys "rlwrap ./syn -f2 machines/test01/MemoryBlock16_desc.clp" C-m
