#!/bin/bash

runfunc(){
  echo $1 has uuid `uuidgen`
}

export -f runfunc
parallel runfunc ::: A B C
