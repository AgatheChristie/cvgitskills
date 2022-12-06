cd ../config
set LogFile=\"../logs/gateway.log\"
werl +P 1024000 +K true -smp enable -name sd1@127.0.0.1 -setcookie sd1 -boot start_sasl -config log -pa ../ebin -kernel error_logger {file,"%LogFile%"} -s sd gateway_start -extra 127.0.0.1 5566 0
pause