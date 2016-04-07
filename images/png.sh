#!/bin/bash

for i in *.pdf; do sips -s format png $i --out $i.png;done
