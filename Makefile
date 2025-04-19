all: upload remote

remote:
	ssh -t ubuntu@nexus.teaql.io "cd ~/workspace/teaql-spring-starter && bash ./remote-build.sh"
local:
	./gradlew publish
upload:
	rsync -avz   --exclude={'build/','.idea/','.gradle/','.git/'}  ./  ubuntu@nexus.teaql.io:~/workspace/teaql-spring-starter/
