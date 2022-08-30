#!/bin/bash

uid=$1
port=$2

read -p "XTB[$uid] password: " -s pass

export XTB_UID=$uid
export XAPI_PORT=$port
export XTB_PASS=$pass
