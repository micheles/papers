#!/bin/bash
# test that killing the parent does NOT kill the child
python parent.py&
sleep 3
kill `cat parent.pid`
sleep 1
ps aux | grep python
