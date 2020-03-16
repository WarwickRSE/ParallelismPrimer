#!/bin/bash

parallel echo {1} is job number {#} ::: A B C
