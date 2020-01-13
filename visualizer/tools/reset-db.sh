#!/bin/sh

sudo mysql -e 'drop database afcens; create database afcens; connect afcens; \. tools/afcens-XXXX-XX-XX-dump.sql'
