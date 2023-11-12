#!/bin/bash
# This is a simple shell script to add rows to a csv file using the ookla command line speedtest
#
# Note that you must run this first with by head with --output-header set and then add
# the header for the date by hand in the created file.
# 
# After run this from cron at the desired frequency
#

(echo -n `date +%Y-%m-%dT%H:%M:%S%z`; echo -n ",") >> /a_file_location_that_is_shared/speedtest_results.csv
/Users/hubert/bin/speedtest --format=csv >> /a_file_location_that_is_shared/speedtest_results.csv