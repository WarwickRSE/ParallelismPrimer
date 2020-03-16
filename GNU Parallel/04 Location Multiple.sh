#!/bin/bash

parallel echo {1} Says hello to {2} ::: A B C ::: D E F
