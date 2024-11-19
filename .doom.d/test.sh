#!/bin/bash
set -ex

emacs -batch -l ert -l test-chun-core.el -f ert-run-tests-batch-and-exit
