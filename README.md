# Network Speed Test Result app

This is R/Shiny applicaton that takes the output generated from Ookla's command line speedtest <https://www.speedtest.net/apps/cli> and displays both upload and download speeds over time.

There is a [sample bash script] (utils/speedtest_job.sh) in the utils folder

Note that you must run this first with by head with --output-header set and then add the header for the date by hand in the created file.

After run this from cron at the desired frequency
