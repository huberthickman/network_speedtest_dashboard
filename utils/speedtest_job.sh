#!/bin/bash
(echo -n `date +%Y-%m-%dT%H:%M:%S%z`; echo -n ",") >> /a_file_location_that_is_shared/speedtest_results.csv
/Users/hubert/bin/speedtest --format=csv >> /a_file_location_that_is_shared/speedtest_results.csv