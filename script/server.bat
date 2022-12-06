cd ../config
set LogFile=\"../logs/server.log\"
start werl +P 1024000 +K true -smp enable -name sd2@127.0.0.1 -setcookie sd1 -boot start_sasl -config log -pa ../ebin -kernel error_logger {file,"%LogFile%"} -s sd server_start -extra 127.0.0.1 7788 1
exit